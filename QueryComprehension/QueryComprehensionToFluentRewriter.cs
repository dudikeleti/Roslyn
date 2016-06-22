using System;
using System.Linq;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace DebuggerShared.Visualizer.QueryComprehension
{
    public class QueryComprehensionToFluentRewriter : CSharpSyntaxRewriter
    {
        private class QueryState
        {
            public SyntaxToken AnonymousTypeIdentifier { get; } = Identifier("@t");
            public Dictionary<string, int> IdentifiersChain { get; } = new Dictionary<string, int>();
            public ExpressionSyntax FluentExpression { get; set; }
            public SyntaxToken SourceIdentifier { get; set; }
            public bool IsAnonymousType { get; set; }
        }

        private readonly SemanticModel _model;
        private readonly QueryState _state;

        public QueryComprehensionToFluentRewriter(SemanticModel model)
        {
            _model = model;
            _state = new QueryState();
        }

        #region Visitors

        public override SyntaxNode VisitQueryExpression(QueryExpressionSyntax node)
        {
            _state.FluentExpression = (ExpressionSyntax)VisitFromClause(node.FromClause);
            VisitQueryBody(node.Body);
            var fluentQuery = ConcatInvocations();
             return _state.FluentExpression;
        }

        public override SyntaxNode VisitQueryBody(QueryBodySyntax node)
        {
            foreach(QueryClauseSyntax clause in node.Clauses)
            {
				 var fromClauseSyntax = clause as FromClauseSyntax;
                InvocationExpressionSyntax fluentInvocation =
                    fromClauseSyntax != null
                    ? CreateSelectMany(fromClauseSyntax)
                    : Visit(clause) as InvocationExpressionSyntax;

                if(fluentInvocation != null)
                {
                    _state.FluentExpression =
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                _state.FluentExpression,
                                (IdentifierNameSyntax)fluentInvocation.Expression),
                            fluentInvocation.ArgumentList);
                }
            }

            var selectOrGroup = (InvocationExpressionSyntax)Visit(node.SelectOrGroup);
            _state.FluentExpression = InvocationExpression(
                   MemberAccessExpression(
                       SyntaxKind.SimpleMemberAccessExpression,
                       _state.FluentExpression,
                       (IdentifierNameSyntax)selectOrGroup.Expression),
                   selectOrGroup.ArgumentList);

            if(node.Continuation != null)
            {
                VisitQueryContinuation(node.Continuation);
            }
            return node;
        }

        public override SyntaxNode VisitQueryContinuation(QueryContinuationSyntax node)
        {
            _state.IsAnonymousType = false;
            _state.IdentifiersChain.Clear();
            _state.SourceIdentifier = node.Identifier;
            _state.IdentifiersChain[_state.SourceIdentifier.ValueText] = 0;
            VisitQueryBody(node.Body);
            return node;
        }

        public override SyntaxNode VisitFromClause(FromClauseSyntax node)
        {
            _state.SourceIdentifier = node.Identifier;
            _state.IdentifiersChain[_state.SourceIdentifier.ValueText] = 0;

            if(node.Type == null)
            {
                return node.Expression;
            }

			 var typeList = new SeparatedSyntaxList<TypeSyntax>();
             typeList = typeList.Add(node.Type);
			 
            return  InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            source,
                            GenericName(
                                Identifier("Cast"),
                                TypeArgumentList(typeList))));
        }

        public override SyntaxNode VisitWhereClause(WhereClauseSyntax node)
        {
            return BuildFluentInvocation("Where", BuildSimpleLambdaExpression(node.Condition));
        }

        public override SyntaxNode VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            var left = (ExpressionSyntax)Visit(node.Left);
            var right = (ExpressionSyntax)Visit(node.Right);
            return node.Update(left, node.OperatorToken, right);
        }

        public override SyntaxNode VisitSelectClause(SelectClauseSyntax node)
        {
            var selectExpression = (ExpressionSyntax)Visit(node.Expression);
            return BuildFluentInvocation("Select", BuildSimpleLambdaExpression(selectExpression));
        }

        public override SyntaxNode VisitGroupClause(GroupClauseSyntax node)
        {
            var newGroup = (GroupClauseSyntax)base.VisitGroupClause(node);
            return BuildFluentInvocation("GroupBy",
                BuildSimpleLambdaExpression(newGroup.ByExpression),
                BuildSimpleLambdaExpression(newGroup.GroupExpression));
        }

        public override SyntaxNode VisitLetClause(LetClauseSyntax node)
        {
            var letExpression = (ExpressionSyntax)Visit(node.Expression);

            var nameEqualsExpressions =
                new List<Tuple<NameEqualsSyntax, ExpressionSyntax>>
                {
                    Tuple.Create<NameEqualsSyntax, ExpressionSyntax>(
                        null,
                        IdentifierName(GetLambdaParameterToken(node.Expression))),
                    Tuple.Create(
                        NameEquals(node.Identifier.ValueText),
                        letExpression),
                };

            var selectInvocation = BuildFluentInvocation("Select",
                BuildSimpleLambdaExpression(
                    BuildAnonymousObject(nameEqualsExpressions)));

            IncreasChain();
            _state.IdentifiersChain[node.Identifier.ValueText] = 0;

            _state.IsAnonymousType = true;

            return selectInvocation;
        }

        public override SyntaxNode VisitOrderByClause(OrderByClauseSyntax node)
        {
            bool moreThanOneOrdering = false;
            foreach(OrderingSyntax orderingSyntax in node.Orderings)
            {
                var orderExpression = (ExpressionSyntax)Visit(orderingSyntax.Expression);

                var orderByThenBy = moreThanOneOrdering ? "ThenBy" : "OrderBy";
                orderByThenBy += orderingSyntax.AscendingOrDescendingKeyword.ValueText?.ToLower() == "descending"
                                  ? "Descending"
                                  : "";

                InvocationExpressionSyntax orderByInvocationvocation = 
                    BuildFluentInvocation(
                        orderByThenBy,
                        BuildSimpleLambdaExpression(orderExpression));

                _state.FluentExpression =
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            _state.FluentExpression,
                            (IdentifierNameSyntax)orderByInvocationvocation.Expression),
                        orderByInvocationvocation.ArgumentList);

                moreThanOneOrdering = true;
                MapClause(orderExpression);
            }
            return null;
        }

        public override SyntaxNode VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            return _state.IsAnonymousType
                       ? (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)
                       : base.VisitMemberAccessExpression(node);
        }

        public override SyntaxNode VisitIdentifierName(IdentifierNameSyntax node)
        {
            return GetMemberAccessForVariable(node);
        }

        #endregion

        #region Help methods

        private InvocationExpressionSyntax CreateSelectMany(FromClauseSyntax currentFrom)
        {
            List<SyntaxToken> lambdaParameters =
              new List<SyntaxToken>
              {
                  GetLambdaParameterToken(currentFrom.Expression),
                  currentFrom.Identifier
              };

            var firstLambda = BuildSimpleLambdaExpression(currentFrom.Expression);
            var secondLambda = BuildLambdaExpression(lambdaParameters, BuildAnonymousObject(lambdaParameters));
            var selectMany = BuildFluentInvocation("SelectMany", firstLambda, secondLambda);

            IncreasChain();

            _state.IdentifiersChain[currentFrom.Identifier.ValueText] = 0;

            _state.IsAnonymousType = true;

            return selectMany;
        }

        private ExpressionSyntax GetMemberAccessForVariable(IdentifierNameSyntax variable)
        {
            var rangeVariable = _model.GetSymbolInfo(variable).Symbol as IRangeVariableSymbol;

            int depth;
            if(!_state.IsAnonymousType ||
                rangeVariable == null ||
                !_state.IdentifiersChain.TryGetValue(variable.Identifier.ValueText, out depth))
            {
                return variable;
            }

            var anonymousTypeNameExpression = IdentifierName(_state.AnonymousTypeIdentifier);
            var memberAccessExpression = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    anonymousTypeNameExpression,
                    variable);
            if(depth == 0)
            {
                return memberAccessExpression;
            }

            ExpressionSyntax pre = anonymousTypeNameExpression;
            for(int i = 0; i < _state.IdentifiersChain[variable.Identifier.ValueText]; i++)
            {
                memberAccessExpression = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    pre,
                    anonymousTypeNameExpression);
                pre = memberAccessExpression;
            }

            memberAccessExpression = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    pre,
                    variable);

            return memberAccessExpression;
        }

        private void IncreasChain()
        {
            if(_state.IsAnonymousType)
            {
                foreach(string syntaxToken in _state.IdentifiersChain.Keys.ToList())
                {
                    _state.IdentifiersChain[syntaxToken]++;
                }
            }
        }

        private InvocationExpressionSyntax BuildFluentInvocation(
            string methodName,
            params LambdaExpressionSyntax[] expressions)
        {
            return InvocationExpression(
                IdentifierName(methodName)).
                WithArgumentList(
                ArgumentList(
                    SeparatedList(
                        expressions.Select(Argument))));
        }

        private SimpleLambdaExpressionSyntax BuildSimpleLambdaExpression(ExpressionSyntax expression)
        {
            return SimpleLambdaExpression(GetLambdaParameter(expression), expression);
        }

        private ParenthesizedLambdaExpressionSyntax BuildLambdaExpression(
           List<SyntaxToken> tokens,
           ExpressionSyntax expression)
        {
            return ParenthesizedLambdaExpression(
                ParameterList(
                    SeparatedList(
                        tokens.Select(Parameter))),
                expression);
        }

        private AnonymousObjectCreationExpressionSyntax BuildAnonymousObject(
            List<SyntaxToken> tokens)
        {
            return AnonymousObjectCreationExpression(
                SeparatedList(
                    tokens.Select(token =>
                        AnonymousObjectMemberDeclarator(
                            IdentifierName(token)))));
        }

        private AnonymousObjectCreationExpressionSyntax BuildAnonymousObject(
            List<Tuple<NameEqualsSyntax, ExpressionSyntax>> nameEqulasAndExpression)
        {
            return AnonymousObjectCreationExpression(
                SeparatedList(
                    Enumerable.Range(0, nameEqulasAndExpression.Count).
                    Select(
                        index => nameEqulasAndExpression[index].Item1 == null ?
                        AnonymousObjectMemberDeclarator(
                            nameEqulasAndExpression[index].Item2) :
                            AnonymousObjectMemberDeclarator(
                                nameEqulasAndExpression[index].Item1,
                                nameEqulasAndExpression[index].Item2)
                            )
                        )
                    );
        }

        private ParameterSyntax GetLambdaParameter(ExpressionSyntax expression)
        {
            return Parameter(
                GetLambdaParameterToken(expression));
        }

        private SyntaxToken GetLambdaParameterToken(ExpressionSyntax expression)
        {
            return _state.IsAnonymousType ?
                _state.AnonymousTypeIdentifier :
                Identifier(GetRangeVariable(expression) ?? _state.SourceIdentifier.ValueText);
        }

        private string GetRangeVariable(ExpressionSyntax node)
        {
            try
            {
                return node.DescendantNodesAndSelf().
               Select(n => _model.GetSymbolInfo(n).Symbol).
               SingleOrDefault(n => n is IRangeVariableSymbol)?.Name;
            }
            catch(Exception)
            {
                return null;
            }
        }

        #endregion
    }
}