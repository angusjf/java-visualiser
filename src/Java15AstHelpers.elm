module Java15AstHelpers exposing (..)

import Java15Parser exposing (..)

typeIdentifierToString : TypeIdentifier -> String
typeIdentifierToString (TypeIdentifier (Identifier i)) = Debug.log "_" i

-- GET REFS

getRefsInClassBody : ClassBody -> List String
getRefsInClassBody (ClassBody declarations) =
    List.concatMap getRefsInClassBodyDeclaration declarations

getRefsInClassBodyDeclaration : ClassBodyDeclaration -> List String
getRefsInClassBodyDeclaration declaration =
    case declaration of
        ClassBodyDeclaration_ClassMemberDeclaration d ->
            getRefsInClassMemberDeclaration d
        ClassBodyDeclaration_InstanceInitializer d ->
            getRefsInInstanceInitializer d
        ClassBodyDeclaration_StaticInitializer d ->
            getRefsInStaticInitializer d
        ClassBodyDeclaration_ConstructorDeclaration d ->
            getRefsInConstructorDeclaration d

getRefsInClassMemberDeclaration : ClassMemberDeclaration -> List String
getRefsInClassMemberDeclaration declaration =
    case declaration of
        ClassMemberDeclaration_Field d ->
            getRefsInFieldDeclaration d
        ClassMemberDeclaration_Method d ->
            getRefsInMethodDeclaration d
        ClassMemberDeclaration_Class d ->
            getRefsInClassDeclaration d
        ClassMemberDeclaration_Interface d ->
            getRefsInInterfaceDeclaration d
        ClassMemberDeclaration_Semi ->
            []

getRefsInInstanceInitializer : InstanceInitializer -> List String
getRefsInInstanceInitializer (InstanceInitializer block) =
    getRefsInBlock block

getRefsInStaticInitializer : StaticInitializer -> List String
getRefsInStaticInitializer (StaticInitializer block) =
    getRefsInBlock block

getRefsInConstructorDeclaration : ConstructorDeclaration -> List String
getRefsInConstructorDeclaration (ConstructorDeclaration _ declarator _ body) =
    [] -- TODO

getRefsInFieldDeclaration : FieldDeclaration -> List String
getRefsInFieldDeclaration (FieldDeclaration modifiers type_ declarators) =
    getRefsInVaraibleDeclatatorList declarators ++ getRefsInUnannType type_

getRefsInUnannType : UnannType -> List String
getRefsInUnannType t =
    case t of
        UnannType_Primitive _ ->
            []
        UnannType_Reference rt ->
            getRefsInUnannReferenceType rt

getRefsInUnannReferenceType : UnannReferenceType -> List String
getRefsInUnannReferenceType t =
    case t of
        UnannReferenceType_Class coi ->
            case coi of
                UnannClassOrInterfaceType_Class class ->
                    getRefsInUnannClassType class
                UnannClassOrInterfaceType_Interface interface ->
                    [] -- TODO
        UnannReferenceType_TypeVariable tv ->
            getRefsInUnannTypeVaraible tv
        UnannReferenceType_Array at ->
            [] -- TODO

getRefsInUnannTypeVaraible : UnannTypeVariable -> List String
getRefsInUnannTypeVaraible (UnannTypeVariable id) =
    [ typeIdentifierToString id ]

getRefsInUnannClassType : UnannClassType -> List String
getRefsInUnannClassType t =
    case t of
        UnannClassType_TypeIdentifer id args ->
            let
                argRefs =
                    args
                    |> Maybe.map getRefsInTypeArguments
                    |> Maybe.withDefault []
            in
                typeIdentifierToString id :: argRefs
        _ ->
            [] -- TODO

getRefsInTypeArguments : TypeArguments -> List String
getRefsInTypeArguments (TypeArguments_Brackets (TypeArgumentList args)) =
    List.concatMap getRefsInTypeArgument args

getRefsInTypeArgument : TypeArgument -> List String
getRefsInTypeArgument arg =
    case arg of
        TypeArgument_ReferenceType rt ->
            getRefsInReferenceType rt
        TypeArgument_Wildcard _ -> 
            []

getRefsInReferenceType : ReferenceType -> List String
getRefsInReferenceType rt =
    case rt of
        ReferenceType_ClassOrInterfaceType coi ->
            getRefsInClassOrInterfaceType coi
        ReferenceType_TypeVariable tv ->
            [] -- TODO
        ReferenceType_ArrayType arrayType ->
            [] -- TODO

getRefsInClassOrInterfaceType : ClassOrInterfaceType -> List String
getRefsInClassOrInterfaceType coi =
    case coi of
        ClassOrInterfaceType_ClassType class ->
            getRefsInClassType class
        ClassOrInterfaceType_InterfaceType interface ->
            [] -- TODO

getRefsInClassType : ClassType -> List String
getRefsInClassType t =
    case t of
        ClassType_NoPackage _ tid args ->
            let
                argRefs =
                    args
                    |> Maybe.map getRefsInTypeArguments
                    |> Maybe.withDefault []
            in
                typeIdentifierToString tid :: argRefs
        ClassType_Package pkg _ tid args ->
            [] -- TODO
        ClassType_ClassOrInterfaceType class _ tid args ->
            [] -- TODO

getRefsInVaraibleDeclatatorList : VariableDeclaratorList -> List String
getRefsInVaraibleDeclatatorList (VariableDeclaratorList declarators) =
    List.concatMap getRefsInVaraibleDeclatator declarators

getRefsInMethodDeclaration : MethodDeclaration -> List String
getRefsInMethodDeclaration (MethodDeclaration modifiers header body) =
    getRefsInMethodHeader header ++ getRefsInMethodBody body

getRefsInClassDeclaration : ClassDeclaration -> List String
getRefsInClassDeclaration _ = [] -- TODO

getRefsInInterfaceDeclaration : InterfaceDeclaration -> List String
getRefsInInterfaceDeclaration i = [] -- TODO

getRefsInVaraibleDeclatator : VariableDeclarator -> List String
getRefsInVaraibleDeclatator (VariableDeclarator id init) =
    getRefsInVariableDeclaratorId id ++
        Maybe.withDefault [] (Maybe.map getRefsInVariableInitializer init)

getRefsInVariableInitializer : VariableInitializer -> List String
getRefsInVariableInitializer init = [] -- TODO

getRefsInVariableDeclaratorId : VariableDeclaratorId -> List String
getRefsInVariableDeclaratorId (VariableDeclaratorId identifier _) =
    getRefsInIdentifier identifier

getRefsInIdentifier : Identifier -> List String
getRefsInIdentifier (Identifier str) = []

getRefsInMethodHeader : MethodHeader -> List String
getRefsInMethodHeader header = []
{-
    case header of
        MethodHeader_Result result MethodDeclarator (Maybe Throws)
        MethodHeader_TypeParameters TypeParameters (List Annotation) Result MethodDeclarator (Maybe Throws)
-}

getRefsInMethodBody : MethodBody -> List String
getRefsInMethodBody body =
    case body of
        MethodBody_Block block -> getRefsInBlock block
        MethodBody_Semi -> []

getRefsInBlock : Block -> List String
getRefsInBlock (Block stmts) =
    Maybe.withDefault [] (Maybe.map getRefsInBlockStatements stmts)

getRefsInBlockStatements : BlockStatements -> List String
getRefsInBlockStatements (BlockStatements x xs) =
    List.concatMap getRefsInBlockStatement (x :: xs)

getRefsInBlockStatement : BlockStatement -> List String
getRefsInBlockStatement stmt =
    case stmt of
        BlockStatement_LocalVariable lv ->
            getRefsInLocalVariableDeclarationStatement lv
        BlockStatement_Class c ->
            getRefsInClassDeclaration c
        BlockStatement_Statement s ->
            getRefsInStatement s

getRefsInLocalVariableDeclarationStatement : LocalVariableDeclarationStatement -> List String
getRefsInLocalVariableDeclarationStatement (LocalVariableDeclarationStatement s) =
    getRefsInLocalVariableDeclaration s

getRefsInLocalVariableDeclaration : LocalVariableDeclaration -> List String
getRefsInLocalVariableDeclaration (LocalVariableDeclaration _ type_ decls) =
    getRefsInLocalVariableType type_ ++ [] -- TODO

getRefsInLocalVariableType : LocalVariableType -> List String
getRefsInLocalVariableType t =
    case t of
        LocalVariableType_UnannType ut ->
            getRefsInUnannType ut
        LocalVariableType_Var ->
            []

getRefsInStatement : Statement -> List String
getRefsInStatement stmt =
    case stmt of
        Statement_Statement s ->
            getRefsInStatementWithoutTrailingSubstatement s
        Statement_Labeled s ->
            [] -- TODO
        Statement_If s ->
            getRefsInIfThenStatement s
        Statement_IfThenElse s ->
            [] -- TODO
        Statement_While s ->
            [] -- TODO
        Statement_For s ->
            [] -- TODO

getRefsInStatementWithoutTrailingSubstatement : StatementWithoutTrailingSubstatement -> List String
getRefsInStatementWithoutTrailingSubstatement s =
    case s of
        StatementWithoutTrailingSubstatement_Block b ->
            getRefsInBlock b
        StatementWithoutTrailingSubstatement_Empty _ ->
            []
        StatementWithoutTrailingSubstatement_Expression e ->
            getRefsInExpressionStatement e
        _ ->
            []
        --StatementWithoutTrailingSubstatement_Assert AssertStatement
        --StatementWithoutTrailingSubstatement_Switch SwitchStatement
        --StatementWithoutTrailingSubstatement_Do DoStatement
        --StatementWithoutTrailingSubstatement_Break BreakStatement
        --StatementWithoutTrailingSubstatement_Continue ContinueStatement
        --StatementWithoutTrailingSubstatement_Return ReturnStatement
        --StatementWithoutTrailingSubstatement_Synchronized SynchronizedStatement
        --StatementWithoutTrailingSubstatement_Throw ThrowStatement
        --StatementWithoutTrailingSubstatement_Try TryStatement
        --StatementWithoutTrailingSubstatement_Yield YieldStatement

getRefsInIfThenStatement : IfThenStatement -> List String
getRefsInIfThenStatement (IfThenStatement exp stmt) =
    getRefsInExpression exp ++ getRefsInStatement stmt

getRefsInExpression : Expression -> List String
getRefsInExpression exp =
    case exp of
        Expression_Lambda l ->
            [] --getRefsInLambdaExpression l
        Expression_Assignment a ->
            getRefsInAssignmentExpression a

getRefsInAssignmentExpression : AssignmentExpression -> List String
getRefsInAssignmentExpression exp =
    case exp of
        AssignmentExpression_Conditional c ->
            [] -- getRefsInConditionalExpression c
        AssignmentExpression_Assignment a ->
            getRefsInAssignment a

getRefsInAssignment : Assignment -> List String
getRefsInAssignment (Assignment lhs _ exp) =
    getRefsInLeftHandSide lhs ++ getRefsInExpression exp

getRefsInLeftHandSide : LeftHandSide -> List String
getRefsInLeftHandSide lhs =
    case lhs of
        LeftHandSide_Expression e ->
            getRefsInExpressionName e
        LeftHandSide_Field f ->
            [] --getRefsInFieldAccess
        LeftHandSide_Array a ->
            [] --ArrayAccess

getRefsInExpressionName : ExpressionName -> List String
getRefsInExpressionName = always []

getRefsInExpressionStatement : ExpressionStatement -> List String
getRefsInExpressionStatement (ExpressionStatement s) =
    getRefsInStatementExpression s

getRefsInStatementExpression : StatementExpression -> List String
getRefsInStatementExpression se =
    case se of
        StatementExpression_Assignment e ->
            getRefsInAssignment e
        _ ->
            []
        --StatementExpression_PreIncrement e ->
        --    getRefsInPreIncrementExpression
        --StatementExpression_PreDecrement e ->
        --    getRefsInPreDecrementExpression
        --StatementExpression_PostIncrement e ->
        --    getRefsInPostIncrementExpression
        --StatementExpression_PostDecrement e ->
        --    getRefsInPostDecrementExpression
        --StatementExpression_MethodInvocation e ->
        --    getRefsInMethodInvocation
        --StatementExpression_ClassCreation e ->
        --    getRefsInClassInstanceCreationExpression
