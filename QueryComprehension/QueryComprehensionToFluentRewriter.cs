using System;
using System.Linq;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Roslyn.Rewriters
{
	// This is a sub class of a bigger class that do the rewrite, among other things.
    // Hence, it may be that part of the code will seem redundant.
    public class QueryComprehensionToFluentRewriter : CSharpSyntaxRewriter
    {
        private class QueryState
        {
            public SyntaxToken AnonymousTypeIdentifier { get; set; }
            public Dictionary<string, int> IdentifiersChain { get; } = new Dictionary<string, int>();
            public ExpressionSyntax FluentExpression { get; set; }
            public SyntaxToken SourceIdentifier { get; set; }
            public bool IsAnonymousType { get; set; }

            internal void Reset(QueryContinuationSyntax node)
            {
                IsAnonymousType = false;
                IdentifiersChain.Clear();
                SourceIdentifier = node.Identifier;
                IdentifiersChain[SourceIdentifier.ValueText] = 0;
            }
        }

        private readonly SemanticModel _model;
        private readonly QueryState _state;
        private bool _visitInvocation = true;
        private bool _visitQuerySyntax = true;
        private readonly List<InvocationExpressionSyntax> _pendingInvocationExpressions;
        private readonly RangeVariablesComparer _rangeVariablesComparer = new RangeVariablesComparer();

        public QueryComprehensionToFluentRewriter(SemanticModel model)
        {
            _model = model;
            _pendingInvocationExpressions = new List<InvocationExpressionSyntax>();
            _state = new QueryState();
        }

        #region Visitors

        public override SyntaxNode VisitQueryExpression(QueryExpressionSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitQueryExpression(node);

            SetAnonymousTypeName(node);

            VisitFromClause(node.FromClause);

            VisitQueryBody(node.Body);

            ExpressionSyntax currentExpression = ParenthesizedExpression((InvocationExpressionSyntax)_state.FluentExpression);

            foreach (var invocation in _pendingInvocationExpressions.AsEnumerable().Reverse())
            {
                var fluentNode = InvocationExpression(MemberAccessExpression(
                       SyntaxKind.SimpleMemberAccessExpression,
                       currentExpression,
                        ((MemberAccessExpressionSyntax)invocation.Expression).Name),
                        invocation.ArgumentList);

                currentExpression = fluentNode;
            }
            _pendingInvocationExpressions.Clear();
            _state.IsAnonymousType = false;

            return _state.FluentExpression;
        }

        public override SyntaxNode VisitQueryBody(QueryBodySyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitQueryBody(node);

            _visitInvocation = false;

            // Visit each clause
            // Concat the result to create a Fluent query expression
            VisitClauses(node.Clauses);
            VisitClauses(new SyntaxNode[] { node.SelectOrGroup });

            // If we have a continuation ("into"), 
            // handle it like it belong to the previous clause, and continue to visit his body. 
            // Otherwise return the the fluent expression.
            if (node.Continuation != null)
            {
                VisitQueryContinuation(node.Continuation);
            }
            return node;
        }

        void VisitClauses(IEnumerable<SyntaxNode> clauses)
        {
            foreach (SyntaxNode clause in clauses)
            {
                // if we have from clause here, it's SelecetMany
                var fromClauseSyntax = clause as FromClauseSyntax;

                InvocationExpressionSyntax fluentInvocation =
                    fromClauseSyntax != null ?
                    CreateSelectMany(fromClauseSyntax) :
                    (InvocationExpressionSyntax)Visit(clause);

                // Get the invocation parts of the expression
                var simpleNameAndArgumentsTuple = GetInvocationsNameAndArgumentsFromExpression(fluentInvocation);

                foreach (Tuple<SimpleNameSyntax, ArgumentListSyntax> tuple in simpleNameAndArgumentsTuple)
                {
                    _state.FluentExpression =
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                _state.FluentExpression,
                                tuple.Item1),
                            tuple.Item2);
                }
            }
        }

        public override SyntaxNode VisitQueryContinuation(QueryContinuationSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitQueryContinuation(node);

            _state.Reset(node);
            VisitQueryBody(node.Body);
            return node;
        }

        public override SyntaxNode VisitFromClause(FromClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitFromClause(node);

            var temp = _visitInvocation;
            _visitInvocation = false;
            var newFrom = (FromClauseSyntax)base.VisitFromClause(node);
            _visitInvocation = temp;

            _state.SourceIdentifier = newFrom.Identifier;
            _state.IdentifiersChain[_state.SourceIdentifier.ValueText] = 0;

            ExpressionSyntax source = GetQuerySource(newFrom) ?? newFrom.Expression;

            ExpressionSyntax fluentExpression = newFrom.Expression;

            if (newFrom.Type != null)
            {
                var typeList = new SeparatedSyntaxList<TypeSyntax>();
                typeList = typeList.Add(newFrom.Type);

                fluentExpression =
                    InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            source,
                            GenericName(
                                Identifier("Cast"),
                                TypeArgumentList(typeList))));

                if (!AreEquivalent(source, newFrom.Expression))
                {
                    bool isSourceFound = false;
                    foreach (ExpressionSyntax expression in newFrom.Expression.
                        DescendantNodesAndSelf().
                        OfType<ExpressionSyntax>().
                        Reverse())
                    {
                        if (!isSourceFound && !AreEquivalent(expression, source))
                        {
                            continue;
                        }

                        isSourceFound = true;
                        var invocation = expression as InvocationExpressionSyntax;
                        var memberAccess = invocation?.Expression as MemberAccessExpressionSyntax;
                        if (memberAccess == null)
                            continue;

                        fluentExpression = InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            fluentExpression,
                            memberAccess.Name)).WithArgumentList(invocation.ArgumentList);
                    }
                }
            }
            _state.FluentExpression = fluentExpression;
            return newFrom;
        }

        public override SyntaxNode VisitWhereClause(WhereClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitWhereClause(node);

            var condition = (ExpressionSyntax)SetFlagAndVisit(node.Condition);
            return BuildFluentInvocation("Where", BuildSimpleLambdaExpression(condition));
        }

        public override SyntaxNode VisitLetClause(LetClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitLetClause(node);

            var letExpression = (ExpressionSyntax)SetFlagAndVisit(node.Expression);

            var nameEqualsExpressions =
                new List<Tuple<NameEqualsSyntax, ExpressionSyntax>>
                {
                    Tuple.Create<NameEqualsSyntax, ExpressionSyntax>(
                        null,
                        IdentifierName(GetLambdaParameterToken(letExpression))),
                    Tuple.Create(
                        NameEquals(node.Identifier.ValueText),
                        letExpression),
                };

            var selectInvocation = BuildFluentInvocation("Select",
                BuildSimpleLambdaExpression(
                    BuildAnonymousObject(nameEqualsExpressions)));

            SetAnonymousState(node.Identifier);

            return selectInvocation;
        }

        public override SyntaxNode VisitOrderByClause(OrderByClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitOrderByClause(node);

            InvocationExpressionSyntax orderByInvocation;
            if (node.Orderings.Count == 1)
            {
                orderByInvocation = HandleOrderByExpression(node.Orderings.Single(), true);
            }
            else
            {
                orderByInvocation = HandleOrderByExpression(node.Orderings[0], true);
                foreach (OrderingSyntax orderingSyntax in node.Orderings.Skip(1))
                {
                    var invocation = HandleOrderByExpression(orderingSyntax, false);
                    orderByInvocation =
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                orderByInvocation,
                                (IdentifierNameSyntax)invocation.Expression),
                            invocation.ArgumentList);
                }
            }
            return orderByInvocation;
        }

        private InvocationExpressionSyntax HandleOrderByExpression(OrderingSyntax orderingSyntax, bool firstOrderExpression)
        {
            var orderExpression = (ExpressionSyntax)SetFlagAndVisit(orderingSyntax.Expression);

            var orderByThenBy = firstOrderExpression ? "OrderBy" : "ThenBy";
            orderByThenBy += orderingSyntax.AscendingOrDescendingKeyword.ValueText?.ToLower() == "descending"
                                 ? "Descending"
                                 : "";

            InvocationExpressionSyntax orderByInvocationvocation = BuildFluentInvocation(
                orderByThenBy,
                BuildSimpleLambdaExpression(orderExpression));

            return orderByInvocationvocation;
        }

        public override SyntaxNode VisitJoinClause(JoinClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitJoinClause(node);

            var joinClause = (JoinClauseSyntax)SetFlagAndVisit(node);
            if (joinClause.Into == null)
            {
                SyntaxToken[] rangeVariables = { GetLambdaParameterToken(joinClause.LeftExpression), joinClause.Identifier };
                var join = BuildFluentInvocation(
                    "Join",
                    Argument(joinClause.InExpression),
                    Argument(BuildSimpleLambdaExpression(joinClause.LeftExpression)),
                    Argument(BuildSimpleLambdaExpression(joinClause.RightExpression, Parameter(joinClause.Identifier))),
                    Argument(BuildLambdaExpression(rangeVariables, BuildAnonymousObject(rangeVariables))));

                SetAnonymousState(rangeVariables);
                return join;
            }
            else
            {
                SyntaxToken[] rangeVariables = { GetLambdaParameterToken(joinClause.LeftExpression), Identifier(joinClause.Into.Identifier.ValueText) };
                var groupJoin =
                    BuildFluentInvocation(
                        "GroupJoin",
                        Argument(joinClause.InExpression),
                        Argument(BuildSimpleLambdaExpression(joinClause.LeftExpression)),
                        Argument(BuildSimpleLambdaExpression(joinClause.RightExpression, Parameter(joinClause.Identifier))),
                        Argument(BuildLambdaExpression(rangeVariables, BuildAnonymousObject(rangeVariables))));

                SetAnonymousState(joinClause.Into.Identifier);
                return groupJoin;
            }
        }

        public override SyntaxNode VisitSelectClause(SelectClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitSelectClause(node);

            var selectExpression = (ExpressionSyntax)SetFlagAndVisit(node.Expression);
            return BuildFluentInvocation("Select", BuildSimpleLambdaExpression(selectExpression));
        }

        public override SyntaxNode VisitGroupClause(GroupClauseSyntax node)
        {
            if (!_visitQuerySyntax)
                return base.VisitGroupClause(node);

            var newGroup = (GroupClauseSyntax)SetFlagAndVisit(node);
            return BuildFluentInvocation("GroupBy",
                BuildSimpleLambdaExpression(newGroup.ByExpression),
                BuildSimpleLambdaExpression(newGroup.GroupExpression));
        }

        public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            if (_visitInvocation)
            {
                var memberAccess = node.Expression as MemberAccessExpressionSyntax;
                if (memberAccess != null && _state.FluentExpression == null)
                {
                    _pendingInvocationExpressions.Add(node);
                }
            }
            return base.VisitInvocationExpression(node);
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

        private void SetAnonymousTypeName(QueryExpressionSyntax node)
        {
            var rangeVariables = GetRangeVariables(node).Select(rv => rv.ValueText).ToList();
            string defaultAnonymousTypeName = "t";
            string anonymousTypeName = defaultAnonymousTypeName;
            int i = 1;
            while (rangeVariables.Any(val => val == anonymousTypeName))
            {
                anonymousTypeName = defaultAnonymousTypeName + i++;
            }
            _state.AnonymousTypeIdentifier = Identifier(anonymousTypeName);
        }

        private SyntaxNode SetFlagAndVisit(SyntaxNode node)
        {
            _visitQuerySyntax = false;
            var result = Visit(node);
            _visitQuerySyntax = true;
            return result;
        }

        private void SetAnonymousState(params SyntaxToken[] identifiers)
        {
            IncreasChain();
            foreach (SyntaxToken syntaxToken in identifiers)
            {
                _state.IdentifiersChain[syntaxToken.ValueText] = 0;
            }
            _state.IsAnonymousType = true;
        }

        private Stack<Tuple<SimpleNameSyntax, ArgumentListSyntax>> GetInvocationsNameAndArgumentsFromExpression(
         InvocationExpressionSyntax fluentInvocation)
        {
            // In case of OrderBy clause we might have several invocations with member access,
            // otherwise we just return the single invocation.
            var invocations = new Stack<Tuple<SimpleNameSyntax, ArgumentListSyntax>>();
            var expr = fluentInvocation.Expression;
            var args = fluentInvocation.ArgumentList;
            while (expr is MemberAccessExpressionSyntax)
            {
                var memberAccess = (MemberAccessExpressionSyntax)expr;
                invocations.Push(Tuple.Create(memberAccess.Name, args));
                var invocation = (InvocationExpressionSyntax)memberAccess.Expression;
                expr = invocation.Expression;
                args = invocation.ArgumentList;
            }
            invocations.Push(Tuple.Create((SimpleNameSyntax)expr, args));
            return invocations;
        }

        private ExpressionSyntax GetQuerySource(FromClauseSyntax fromClause)
        {
            var node = fromClause.Expression;
            ExpressionSyntax source = RemoveParenthesized(node);

            var memberAccess =
                node.DescendantNodesAndSelf().
                OfType<InvocationExpressionSyntax>().
                FirstOrDefault()?.Expression as MemberAccessExpressionSyntax;
            if (memberAccess == null)
            {
                return node;
            }

            List<ExpressionSyntax> invocations = new List<ExpressionSyntax> { node };
            do
            {
                invocations.Add(memberAccess.Expression);
                memberAccess = (memberAccess.Expression as InvocationExpressionSyntax)?.Expression as MemberAccessExpressionSyntax;
            } while (memberAccess != null);

            // Add here your own logic to get the right collection source

            return source;
        }

        private ExpressionSyntax RemoveParenthesized(ExpressionSyntax expression)
        {
            var parenthesizedExpression = expression as ParenthesizedExpressionSyntax;
            if (parenthesizedExpression == null)
                return expression;

            return RemoveParenthesized(parenthesizedExpression.Expression);
        }

        private InvocationExpressionSyntax CreateSelectMany(FromClauseSyntax currentFrom)
        {
            var expression = (ExpressionSyntax)Visit(currentFrom.Expression);
            List<SyntaxToken> lambdaParameters =
              new List<SyntaxToken>
              {
                  GetLambdaParameterToken(expression),
                  currentFrom.Identifier
              };

            var firstLambda = BuildSimpleLambdaExpression(expression);
            var secondLambda = BuildLambdaExpression(lambdaParameters, BuildAnonymousObject(lambdaParameters));
            var selectMany = BuildFluentInvocation("SelectMany", firstLambda, secondLambda);

            SetAnonymousState(currentFrom.Identifier);

            return selectMany;
        }

        private ExpressionSyntax GetMemberAccessForVariable(IdentifierNameSyntax variable)
        {
            var rangeVariable = _model.GetSymbolInfo(variable).Symbol as IRangeVariableSymbol;

            int depth;
            if (rangeVariable == null ||
                !_state.IsAnonymousType ||
                !_state.IdentifiersChain.TryGetValue(variable.Identifier.ValueText, out depth))
            {
                return variable;
            }

            var anonymousTypeNameExpression = IdentifierName(_state.AnonymousTypeIdentifier);
            var memberAccessExpression = MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    anonymousTypeNameExpression,
                    variable);
            if (depth == 0)
            {
                return memberAccessExpression;
            }

            ExpressionSyntax pre = anonymousTypeNameExpression;
            for (int i = 0; i < _state.IdentifiersChain[variable.Identifier.ValueText]; i++)
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
            if (_state.IsAnonymousType)
            {
                foreach (string syntaxToken in _state.IdentifiersChain.Keys.ToList())
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

        private InvocationExpressionSyntax BuildFluentInvocation(
            string methodName,
            params ArgumentSyntax[] arguments)
        {
            return InvocationExpression(
                IdentifierName(methodName)).
                WithArgumentList(
                ArgumentList(
                    SeparatedList(arguments)));
        }

        private SimpleLambdaExpressionSyntax BuildSimpleLambdaExpression(ExpressionSyntax expression, ParameterSyntax parameter = null)
        {
            return SimpleLambdaExpression(parameter ?? GetLambdaParameter(expression), expression);
        }

        private ParenthesizedLambdaExpressionSyntax BuildLambdaExpression(
           IEnumerable<SyntaxToken> tokens,
           ExpressionSyntax expression)
        {
            return ParenthesizedLambdaExpression(ParameterList(SeparatedList(tokens.Select(Parameter))), expression);
        }

        private AnonymousObjectCreationExpressionSyntax BuildAnonymousObject(
            IEnumerable<SyntaxToken> tokens)
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
                GetRangeVariable(expression);
        }

        private SyntaxToken GetRangeVariable(ExpressionSyntax node)
        {
            try
            {
                return Identifier(
                    node.DescendantNodesAndSelf().
                    Select(n => _model.GetSymbolInfo(n).Symbol).
                    SingleOrDefault(n => n is IRangeVariableSymbol)?.Name);
            }
            catch (Exception)
            {
                return Identifier(_state.SourceIdentifier.ValueText);
            }
        }

        private IEnumerable<SyntaxToken> GetRangeVariables(params SyntaxNode[] nodes)
        {
            return (from node in nodes
                    from child in node.DescendantNodesAndSelf()
                    select _model.GetSymbolInfo(child).Symbol ??
                           _model.GetDeclaredSymbol(child) into symbol
                    where symbol is IRangeVariableSymbol
                    select Identifier(symbol.Name)).Distinct(_rangeVariablesComparer);
        }
        #endregion
    }

    internal class RangeVariablesComparer : IEqualityComparer<SyntaxToken>
    {
        public bool Equals(SyntaxToken x, SyntaxToken y)
        {
            return x.ValueText == y.ValueText;
        }

        public int GetHashCode(SyntaxToken obj)
        {
            return obj.ValueText.GetHashCode();
        }
    }
}