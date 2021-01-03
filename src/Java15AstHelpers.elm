module Java15AstHelpers exposing (..)

import Java15Parser exposing (..)

typeIdentifierToString : TypeIdentifier -> String
typeIdentifierToString (TypeIdentifier (Identifier i)) = i

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
            [] -- TODO
        UnannReferenceType_Array at ->
            [] -- TODO

getRefsInUnannClassType : UnannClassType -> List String
getRefsInUnannClassType t =
    case t of
        UnannClassType_TypeIdentifer id args ->
            typeIdentifierToString id :: [] -- TODO
        _ ->
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
getRefsInStatement stmt = [] -- TODO
