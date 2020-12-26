module Java7AstHelpers exposing (getRefsInClassBody, getRefsInReferenceType)

import Java7Parser exposing (..)
import List.Nonempty

getRefsInClassBody : ClassBody -> List String
getRefsInClassBody { declarations } =
    List.concatMap getRefsInClassBodyDeclaration declarations

getRefsInClassBodyDeclaration : ClassBodyDeclaration -> List String
getRefsInClassBodyDeclaration dec =
    case dec of
        CBSemicolon -> []
        CBMember { decl } -> getRefsInMemberDecl decl
        CBBlock { block } -> getRefsInBlock block

getRefsInMemberDecl : MemberDecl -> List String
getRefsInMemberDecl d = 
    case d of
        MDMethodOrField decl ->
            getRefsInMethodOrFieldDecl decl
        MDVoidMethod { rest } ->
            getRefsInVoidMethodDeclaratorRest rest
        MDConstructor { rest } ->
            getRefsInConstructorDeclaratorRest rest
        MDGenericMethodOrConstructor decl ->
            getRefsInGenericMethodOrConstructorDecl decl
        MDClass decl ->
            getRefsInClassDeclaration decl
        MDInterface decl ->
            getRefsInInterfaceDeclaration decl

getRefsInMethodOrFieldDecl : MethodOrFieldDecl -> List String
getRefsInMethodOrFieldDecl { type_, rest } =
    getRefsInType type_ ++ getRefsInMethodOrFieldRest rest

getRefsInType : Type -> List String
getRefsInType t =
    case t of
        BasicType b -> []
        RefType ref -> getRefsInReferenceType ref
        ArrayType a -> getRefsInType a

getRefsInReferenceType : ReferenceType -> List String
getRefsInReferenceType r =
    let
        list = List.Nonempty.toList r
        getTypeArgs : (String, List TypeArgument) -> List String
        getTypeArgs (_, args) = List.concatMap getRefsInTypeArgument args
        mainType = String.join "." <| List.map Tuple.first list
    in
        mainType :: List.concatMap getTypeArgs list

getRefsInTypeArgument : TypeArgument -> List String
getRefsInTypeArgument t =
    case t of
        ReferenceTypeArgument rt -> getRefsInReferenceType rt
        WildCardSuper rt -> getRefsInReferenceType rt
        WildCardExtends rt -> getRefsInReferenceType rt

getRefsInMethodOrFieldRest : MethodOrFieldRest -> List String
getRefsInMethodOrFieldRest r =
    case r of
        FieldRest rest -> getRefsInFieldDeclaratorsRest rest
        MethodRest rest -> getRefsInMethodDeclaratorRest rest

getRefsInFieldDeclaratorsRest : FieldDeclaratorsRest -> List String
getRefsInFieldDeclaratorsRest { varaibleRest, more } =
    (getRefsInVariableDeclaratorRest varaibleRest)
      ++ (List.concatMap getRefsInVariableDeclarator more)

getRefsInVariableDeclaratorRest : VariableDeclaratorRest -> List String
getRefsInVariableDeclaratorRest { initializer } =
    case initializer of
        Just i -> getRefsInVariableInitializer i
        Nothing -> []

getRefsInVariableInitializer : VariableInitializer -> List String
getRefsInVariableInitializer i =
    case i of
        VIArray ai -> List.concatMap getRefsInVariableInitializer ai
        VIExpression exp -> getRefsInExpression exp

getRefsInExpression : Expression -> List String
getRefsInExpression (Expression { exp1, rest }) =
    getRefsInExpression1 exp1
      ++ case rest of 
            Just (_, exp12) -> getRefsInExpression1 exp12
            Nothing -> []

getRefsInExpression1 : Expression1 -> List String
getRefsInExpression1 (Expression1 { exp2, rest }) =
    getRefsInExpression2 exp2
      ++ case rest of 
            Just (exp, exp1) ->
                getRefsInExpression exp ++ getRefsInExpression1 exp1
            Nothing -> []

getRefsInExpression2 : Expression2 -> List String
getRefsInExpression2 (Expression2 { exp3, rest }) =
    getRefsInExpression3 exp3 ++ getRefsInExpression2Rest rest

getRefsInExpression3 : Expression3 -> List String
getRefsInExpression3 e =
    case e of
        E3Prefix { exp3 } -> getRefsInExpression3 exp3
        E3BracketedExpression { expression, exp3 } ->
            getRefsInExpression expression ++ getRefsInExpression3 exp3
        E3BracketedType { type_, exp3 } ->
            getRefsInType type_ ++ getRefsInExpression3 exp3
        E3Primary { primary, selectors } ->
            getRefsInPrimary primary
              ++ List.concatMap getRefsInSelector selectors

getRefsInPrimary : Primary -> List String
getRefsInPrimary p =
    case p of
        PrimaryLiteral _ -> []
        PrimaryParExpression (Par exp) -> getRefsInExpression exp
        PrimaryThis args -> List.concatMap getRefsInExpression args
        PrimarySuper superSuffix -> getRefsInSuperSuffix superSuffix
        PrimaryNew creator -> getRefsInCreator creator
        PrimaryTypeArgsAndEGIS { typeArgs, suffix } ->
            List.concatMap getRefsInTypeArgument typeArgs
              ++ getRefsInExplicitGenericInvocationSuffix suffix
        PrimaryTypeArgsAndGeneric { typeArgs, thisArgs } ->
            List.concatMap getRefsInTypeArgument typeArgs
              ++ List.concatMap getRefsInExpression thisArgs
        PrimaryIdentifier { suffix } ->
            case suffix of
                Just s -> getRefsInIdentifierSuffix s
                Nothing -> []
        BasicTypeDotClass _ -> []
        VoidDotClass -> []

getRefsInExpression2Rest : Expression2Rest -> List String
getRefsInExpression2Rest r =
    case r of
        E2RInstanceof type_ -> getRefsInType type_
        E2RInfixOp ops ->
            List.concatMap (\(_, exp3) -> getRefsInExpression3 exp3) ops

getRefsInIdentifierSuffix : IdentifierSuffix -> List String
getRefsInIdentifierSuffix is =
    case is of
        NoSuffix -> []
        IdSuffixArrayDotClass _ -> []
        IdSuffixExpression exp -> getRefsInExpression exp
        IdSuffixArguments exps -> List.concatMap getRefsInExpression exps
        IdSuffixDotClass -> []
        IdSuffixDotEGI egi -> getRefsInExplicitGenericInvocation egi
        IdSuffixDotThis -> []
        IdSuffixDotSuper exps -> List.concatMap getRefsInExpression exps
        IdSuffixDotNew { typeArgs, innerCreator } ->
            List.concatMap getRefsInTypeArgument typeArgs
              ++ getRefsInInnerCreator innerCreator

getRefsInInnerCreator : InnerCreator -> List String
getRefsInInnerCreator { typeArgs, rest } =
    List.concatMap getRefsInReferenceType typeArgs
      ++ getRefsInClassCreatorRest rest

-------------------
-------------------
-------------------
-------------------

getRefsInClassCreatorRest : ClassCreatorRest -> List String
getRefsInClassCreatorRest _ = []

getRefsInExplicitGenericInvocation : ExplicitGenericInvocation -> List String
getRefsInExplicitGenericInvocation _ = []

getRefsInCreator : Creator -> List String
getRefsInCreator _ = []

getRefsInSuperSuffix : SuperSuffix -> List String
getRefsInSuperSuffix _ = []

getRefsInExplicitGenericInvocationSuffix : ExplicitGenericInvocationSuffix
                                                -> List String
getRefsInExplicitGenericInvocationSuffix _ = []

getRefsInSelector : Selector -> List String
getRefsInSelector s = []

-------------------
-------------------
-------------------
-------------------
-------------------

getRefsInVariableDeclarator : VariableDeclarator -> List String
getRefsInVariableDeclarator (VariableDeclarator id rest) =
    getRefsInVariableDeclaratorRest rest

getRefsInMethodDeclaratorRest : MethodDeclaratorRest -> List String
getRefsInMethodDeclaratorRest { formalParams, throws, block } =
    getRefsInFormalParameters formalParams
      ++ throws
      ++ case block of
          Just b -> getRefsInBlock b
          Nothing -> []

getRefsInFormalParameters : FormalParameters -> List String
getRefsInFormalParameters _ = []

getRefsInVoidMethodDeclaratorRest : VoidMethodDeclaratorRest -> List String
getRefsInVoidMethodDeclaratorRest { formalParams, throws, block } =
    getRefsInFormalParameters formalParams
      ++ throws
      ++ case block of
          Just b -> getRefsInBlock b
          Nothing -> []

getRefsInConstructorDeclaratorRest : ConstructorDeclaratorRest -> List String
getRefsInConstructorDeclaratorRest { formalParams, throws, block } =
    getRefsInFormalParameters formalParams
      ++ throws
      ++ getRefsInBlock block

--------------
--------------
--------------
--------------

getRefsInGenericMethodOrConstructorDecl : GenericMethodOrConstructorDecl
                                                        -> List String
getRefsInGenericMethodOrConstructorDecl { typeParams, rest } =
    []

getRefsInClassDeclaration : ClassDeclaration -> List String
getRefsInClassDeclaration decl = []

getRefsInInterfaceDeclaration : InterfaceDeclaration -> List String
getRefsInInterfaceDeclaration decl = []

--------------
--------------
--------------
--------------

getRefsInBlock : Block -> List String
getRefsInBlock block = List.concatMap getRefsInBlockStatement block

getRefsInBlockStatement : BlockStatement -> List String
getRefsInBlockStatement s =
    case s of
        BlockVaraible v -> getRefsInLocalVariableDeclarationStatement v
        BlockClassOrInterface c -> getRefsInClassOrInterfaceDeclaration c
        BlockStatement { statement } -> getRefsInStatement statement

getRefsInLocalVariableDeclarationStatement : LocalVariableDeclarationStatement 
                                                -> List String
getRefsInLocalVariableDeclarationStatement { type_, declararators } =
    getRefsInType type_
      ++ List.concatMap getRefsInVariableDeclarator declararators

getRefsInClassOrInterfaceDeclaration : ClassOrInterfaceDeclaration -> List String
getRefsInClassOrInterfaceDeclaration _ = [] -- TODO

getRefsInStatement : Statement -> List String
getRefsInStatement s =
    case s of
        StatementBlock block -> getRefsInBlock block
        StatementSemicolon -> []
        StatementLabel { statement } -> getRefsInStatement statement
        StatementExpression expression -> getRefsInExpression expression
        _ -> []
        {-
        StatementIf
        { cond : ParExpression, if_ : Statement, else_ : Maybe Statement }
        StatementAssert
        { expression : Expression
        , throws : Maybe Expression
        }
        StatementSwitch
        { expression : ParExpression
        , groups : List SwitchBlockStatementGroup
        }
        StatementWhile
        { cond : ParExpression
        , statement : Statement
        }
        StatementDo
        { cond : ParExpression
        , statement : Statement
        }
        StatementFor
        { control : ForControl
        , statement : Statement
        }
        StatementBreak (Maybe String)
        StatementContinue (Maybe String)
        StatementReturn (Maybe Expression)
        StatementThrow Expression
        StatementSynchronized
        { expression : ParExpression
        , block : Block
        }
        StatementTry
        { block : Block
        , catches : List CatchClause
        , finally : Maybe Block
        }
        StatementTryResource
        { spec : ResourceSpecification
        , block : Block
        , catches : List CatchClause
        , finally : Maybe Block
        }
        -}
