module Complexity exposing (..)

import Java15Parser exposing (..)

cClassDeclaration _ = 0 -- TODO

cMethodDeclaration _ = 0 -- TODO

cClassBody : ClassBody -> Float
cClassBody (ClassBody declarations) =
    declarations
    |> List.map cClassBodyDeclaration
    |> List.sum

cClassBodyDeclaration : ClassBodyDeclaration -> Float
cClassBodyDeclaration declaration =
    case declaration of
        ClassBodyDeclaration_ClassMemberDeclaration d ->
            cClassMemberDeclaration d
        ClassBodyDeclaration_InstanceInitializer d ->
            cInstanceInitializer d
        ClassBodyDeclaration_StaticInitializer d ->
            cStaticInitializer d
        ClassBodyDeclaration_ConstructorDeclaration d ->
            cConstructorDeclaration d

cClassMemberDeclaration : ClassMemberDeclaration -> Float
cClassMemberDeclaration declaration =
    case declaration of
        ClassMemberDeclaration_Field _ ->
            0
        ClassMemberDeclaration_Method d ->
            cMethodDeclaration d
        ClassMemberDeclaration_Class d ->
            cClassDeclaration d
        ClassMemberDeclaration_Interface d ->
            Debug.todo "_" --getRefsInInterfaceDeclaration d
        ClassMemberDeclaration_Semi ->
            0

cInstanceInitializer : InstanceInitializer -> Float
cInstanceInitializer (InstanceInitializer block) =
    cBlock block

cStaticInitializer : StaticInitializer -> Float
cStaticInitializer (StaticInitializer block) =
    cBlock block

cConstructorDeclaration : ConstructorDeclaration -> Float
cConstructorDeclaration (ConstructorDeclaration _ _ _ body) =
    0 -- TODO

cBlock : Block -> Float
cBlock (Block stmts) =
    stmts
    |> Maybe.map cBlockStatements
    |> Maybe.withDefault 0

cBlockStatements : BlockStatements -> Float
cBlockStatements (BlockStatements x xs) =
    List.sum <| List.map cBlockStatement (x :: xs)

cBlockStatement : BlockStatement -> Float
cBlockStatement stmt =
    case stmt of
        BlockStatement_LocalVariable lv ->
            0
        BlockStatement_Class c ->
            cClassDeclaration c
        BlockStatement_Statement s ->
            cStatement s

cStatement : Statement -> Float
cStatement stmt =
    case stmt of
        Statement_Statement s ->
            0 -- TODO getRefsInStatementWithoutTrailingSubstatement s
        Statement_Labeled s ->
            0 -- TODO
        Statement_If s ->
            cIfThenStatement s
        Statement_IfThenElse s ->
            cIfThenElse s
        Statement_While s ->
            cWhile 
        Statement_For s ->
            0 -- TODO

cIfThenStatement : IfThenStatement -> Float
cIfThenStatement (IfThenStatement _ stmt) =
   1 + cStatement stmt

