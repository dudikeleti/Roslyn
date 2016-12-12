public class MakeStaticSyntaxRewriter : CSharpSyntaxRewriter
        {
            const string ThisFieldName = "_this_";
            private readonly SemanticModel _model;

            public MakeStaticSyntaxRewriter(SemanticModel model)
            {
                if(model == null)
                {
                    throw new ArgumentNullException(nameof(model));
                }
                _model = model;
            }

            public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                var newNode = (MethodDeclarationSyntax)base.VisitMethodDeclaration(node);
                var modifiers = newNode.Modifiers;
                if(modifiers.All(mod => mod.Kind() != SyntaxKind.StaticKeyword))
                {
                    modifiers = modifiers.Add(Token(SyntaxKind.StaticKeyword).WithTrailingTrivia(Whitespace(" ")));
                }
                var parentClass = node.Ancestors().OfType<ClassDeclarationSyntax>().First();
                var className = parentClass.Identifier.ValueText;
                bool? hasTypeParameters = parentClass.TypeParameterList?.Parameters.Any();
                if(hasTypeParameters != null && (bool)hasTypeParameters)
                    className += parentClass.TypeParameterList.ToString();

                // I pass the 'this' parameter in anyway.
                var parameterSyntax =
                    Parameter(SyntaxFactory.Identifier(" " + ThisFieldName)).
                        WithType(ParseTypeName(className));

                if(newNode.ParameterList?.Parameters.Count > 0)
                    parameterSyntax = parameterSyntax.WithLeadingTrivia(SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " "));

                var separatedSyntaxList = new SeparatedSyntaxList<ParameterSyntax>();
                separatedSyntaxList = separatedSyntaxList.Add(parameterSyntax);
                var parameterList = ParameterList(separatedSyntaxList);
                var newParameterList = newNode.ParameterList == null
                                    ? parameterList
                                    : newNode.ParameterList.AddParameters(parameterSyntax);

                return newNode.ReplaceNode(newNode,
                    newNode.Update(newNode.AttributeLists, modifiers, newNode.ReturnType, newNode.ExplicitInterfaceSpecifier,
                        newNode.Identifier, newNode.TypeParameterList, newParameterList, newNode.ConstraintClauses, newNode.Body,
                        newNode.ExpressionBody, newNode.SemicolonToken));
            }

            public override SyntaxNode VisitIdentifierName(IdentifierNameSyntax node)
            {
                var symbol = _model.GetSymbolInfo(node).Symbol;
                if(symbol == null || symbol.IsStatic)
                {
                    return node;
                }
                switch(symbol.Kind)
                {
                    case SymbolKind.Field:
                    case SymbolKind.Property:
                    case SymbolKind.Method:
                    case SymbolKind.Event:
                        // In cases such as "a.b.c.d", we only want to add the 
                        // ThisFieldName prefix to the "a" part, not the other parts.
                        var parentMemberAccess = node.Parent as MemberAccessExpressionSyntax;
                        if(parentMemberAccess == null || parentMemberAccess.Expression == node)
                        {
                            var result = MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName(
                                    SyntaxFactory.Identifier(ThisFieldName)),
                                IdentifierName(node.Identifier));

                            return result;
                        }
                        break;
                }
                return node;
            }

            public override SyntaxNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                var expressions = node.Initializer?.Expressions;
                if(expressions == null)
                {
                    return base.VisitObjectCreationExpression(node);
                }
                if(node.ArgumentList != null)
                {
                    VisitArgumentList(node.ArgumentList);
                }
                foreach(AssignmentExpressionSyntax assignment in node.Initializer.Expressions.OfType<AssignmentExpressionSyntax>())
                {
                    Visit(assignment.Right);
                }
                return node;
            }

            public override SyntaxNode VisitAnonymousObjectCreationExpression(AnonymousObjectCreationExpressionSyntax node)
            {
                var initializers = new SeparatedSyntaxList<AnonymousObjectMemberDeclaratorSyntax>();
                foreach(AnonymousObjectMemberDeclaratorSyntax anonymousObjectMemberDeclaratorSyntax in node.Initializers)
                {
                    if(anonymousObjectMemberDeclaratorSyntax.NameEquals == null)
                    {
                        initializers = initializers.Add(
                            AnonymousObjectMemberDeclarator(
                                (ExpressionSyntax)Visit(anonymousObjectMemberDeclaratorSyntax.Expression)));
                    }
                    else
                    {
                        initializers = initializers.Add(
                            AnonymousObjectMemberDeclarator(
                                anonymousObjectMemberDeclaratorSyntax.NameEquals,
                                (ExpressionSyntax)Visit(anonymousObjectMemberDeclaratorSyntax.Expression)));
                    }
                }
                return node.Update(node.NewKeyword, node.OpenBraceToken, initializers, node.CloseBraceToken);
            }

            public override SyntaxNode VisitThisExpression(ThisExpressionSyntax node)
            {
                return IdentifierName(SyntaxFactory.Identifier(ThisFieldName));
            }

            public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
            {
                var identifierNameSyntax = node.Expression as IdentifierNameSyntax;
                if(identifierNameSyntax != null)
                {
                    var symbol = _model.GetSymbolInfo(node).Symbol;
                    if(symbol != null && !symbol.IsStatic)
                    {
                        var methodSymbol = symbol as IMethodSymbol;
                        if(methodSymbol?.MethodKind == MethodKind.Ordinary)
                        {
                            var newExpression =
                               MemberAccessExpression(
                                   SyntaxKind.SimpleMemberAccessExpression,
                                   IdentifierName(
                                       SyntaxFactory.Identifier(ThisFieldName)),
                                   IdentifierName(
                                      identifierNameSyntax.Identifier));

                            return node.ReplaceNode(node,
                                node.Update(newExpression, node.ArgumentList));
                        }
                    }
                }
                return base.VisitInvocationExpression(node);
            }
            
            public override SyntaxNode VisitConditionalAccessExpression(ConditionalAccessExpressionSyntax node)
            {
                // update the expression but not the Elvis itself
                var newExpression = (ExpressionSyntax)Visit(node.Expression);
                return node.Update(newExpression, node.OperatorToken, node.WhenNotNull);
            }
        }
