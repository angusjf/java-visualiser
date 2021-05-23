module Complexity exposing (..)

import Java15Parser exposing (..)

cClassDeclaration _ = 0 -- TODO

cMethodDeclaration : MethodDeclaration -> Float
cMethodDeclaration (MethodDeclaration modifiers header body) =
    case body of
        MethodBody_Block block -> cBlock block
        MethodBody_Semi -> 0

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
            1 + cMethodDeclaration d -- add one for every class method
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
            cStatementWithoutTrailingSubstatement s
        Statement_Labeled s ->
            0 -- TODO
        Statement_If s ->
            cIfThenStatement s
        Statement_IfThenElse s ->
            cIfThenElseStatement s
        Statement_While s ->
            cWhileStatement s
        Statement_For s ->
            cForStatement s

cIfThenStatement : IfThenStatement -> Float
cIfThenStatement (IfThenStatement _ stmt) =
   1 + cStatement stmt

cIfThenElseStatement : IfThenElseStatement -> Float
cIfThenElseStatement (IfThenElseStatement _ true false) =
   1 + cStatementNoShortIf true + cStatement false

cWhileStatement : WhileStatement -> Float
cWhileStatement (WhileStatement _ statement) = 1 + cStatement statement

cForStatement : ForStatement -> Float
cForStatement s =
    case s of
        ForStatement_Basic (BasicForStatement _ _ _ statement) ->
            1 + cStatement statement
        ForStatement_Enhanced (EnhancedForStatement _ _ _ _ statement) ->
            1 + cStatement statement
    
    

cStatementNoShortIf : StatementNoShortIf -> Float
cStatementNoShortIf s =
    case s of
        StatementNoShortIf_NoTrailing statementWithoutTrailingSubstatement ->
            0 -- TODO
        StatementNoShortIf_Labeled labeledStatementNoShortIf ->
            0 -- TODO
        StatementNoShortIf_IfThenElse ifThenElseStatementNoShortIf ->
            0 -- TODO
        StatementNoShortIf_While whileStatementNoShortIf ->
            0 -- TODO
        StatementNoShortIf_For forStatementNoShortIf ->
            0 -- TODO

cStatementWithoutTrailingSubstatement : StatementWithoutTrailingSubstatement -> Float
cStatementWithoutTrailingSubstatement st =
    case st of
        StatementWithoutTrailingSubstatement_Block s ->
            cBlock s
        StatementWithoutTrailingSubstatement_Empty s ->
            0
        StatementWithoutTrailingSubstatement_Expression s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Assert s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Switch s ->
            cSwitchStatment s
        StatementWithoutTrailingSubstatement_Do s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Break s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Continue s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Return s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Synchronized s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Throw s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Try s ->
            0 -- TODO
        StatementWithoutTrailingSubstatement_Yield s ->
            0 -- TODO

cSwitchStatment : SwitchStatement -> Float
cSwitchStatment (SwitchStatement _ switchBlock) =
    case switchBlock of
        SwitchBlock_Rule first more ->
            List.map cSwitchRule more
            |> List.sum
            |> \r -> (cSwitchRule first) + r
        SwitchBlock_Group switchBlockStatementGroups switchLabels ->
            List.map cSwitchBlockStatementGroup switchBlockStatementGroups
            |> List.sum
            |> \n -> n + toFloat (List.length switchBlockStatementGroups)
            
cSwitchRule : SwitchRule -> Float
cSwitchRule s =
    case s of
        SwitchRule_Expression _ exp ->
            0
        SwitchRule_Block _ block ->
            cBlock block
        SwitchRule_Throw label throwStatement ->
            0 -- TODO
    
cSwitchBlockStatementGroup : SwitchBlockStatementGroup -> Float
cSwitchBlockStatementGroup (SwitchBlockStatementGroup _ _ blockStatements) =
    cBlockStatements blockStatements
