module JavaParser exposing (..)

import Set exposing (Set)
import Parser as P exposing (Parser, (|=), (|.), Step)

---- ---- i

type alias CompilationUnit =
  { package : Maybe String
  , imports : List ImportDeclaration
  , types : List TypeDeclaration
  }

type alias ImportDeclaration =
  { static : Bool
  , identifier : String
  }

type TypeDeclaration
  = ClassOrInterface ClassOrInterfaceDeclaration
  | Semicolon

type ClassOrInterfaceDeclaration
  = Class (List Modifier) ClassDeclaration
  | Interface (List Modifier) InterfaceDeclaration

type ClassDeclaration
  = NormalClass NormalClassDeclaration
  | Enum EnumDeclaration

type InterfaceDeclaration
  = IDNormal NormalInterfaceDeclaration
  | IDAnnotation AnnotationTypeDeclaration

type alias NormalClassDeclaration =
  { identifier : String
  , typeArgs : List TypeArgument
  , extends : Maybe Type
  , implements : List Type
  , body : ClassBody
  }

type alias EnumDeclaration =
  { identifier : String
  , implements : List Type
  , body : EnumBody
  }

type alias NormalInterfaceDeclaration =
  { identifier : String
  , typeArgs : List TypeArgument
  , extends : List Type
  , body : InterfaceBody
  }

type alias AnnotationTypeDeclaration =
  { identifier : String
  , body : AnnotationTypeBody
  }

---- ---- ii

type Type
  = BasicType BasicType
  | RefType ReferenceType
  | ArrayType Type

type BasicType
  = Byte
  | Short
  | Char
  | Int
  | Long
  | Float
  | Double
  | Boolean

type alias ReferenceType =
  { name : String
  , typeArguments : List TypeArgument
  }

type TypeArgument
  = ReferenceTypeArgument ReferenceType
  | WildCardSuper ReferenceType
  | WildCardExtends ReferenceType

----_---- iii

{-
NonWildcardTypeArguments:
    < TypeList >

TypeList:  
    ReferenceType { , ReferenceType }

TypeArgumentsOrDiamond:
    < > 
    TypeArguments

NonWildcardTypeArgumentsOrDiamond:
    < >
    NonWildcardTypeArguments

TypeParameters:
    < TypeParameter { , TypeParameter } >

TypeParameter:
    Identifier [extends Bound]

Bound:
    ReferenceType { & ReferenceType }
-}

---- ---- iv

type Modifier
  = ModifierAnnotation Annotation
  | Public
  | Protected
  | Private
  | Static 
  | Abstract
  | Final
  | Native
  | Synchronized
  | Transient
  | Volatile
  | Strictfp

type alias Annotation =
  { identifier : String
  , element : Maybe AnnotationElement
  }

type AnnotationElement
  = AEPairs (List ElementValuePair)
  | AEValue ElementValue

type alias ElementValuePair =
  { identifier : String
  , value : ElementValue
  }
    
type ElementValue
  = ElementValueAnnotation Annotation
  | ElementValueExpression Expression1 
  | ElementValueArrayInitializer (List ElementValue)

---- ---- v

type alias ClassBody =
  { declarations : List ClassBodyDeclaration
  }

type ClassBodyDeclaration
  = CBSemicolon
  | CBMember
    { modifiers : List Modifier
    , decl : MemberDecl
    }
  | CBBlock
    { static : Bool 
    , block : Block
    }

type MemberDecl
  = MDMethodOrField MethodOrFieldDecl
  | MDVoidMethod
    { identifier : String
    , rest : VoidMethodDeclaratorRest
    }
  | MDConstructor 
    { identifier : String
    , rest : ConstructorDeclaratorRest
    }
  | MDGenericMethodOrConstructor GenericMethodOrConstructorDecl
  | MDClass ClassDeclaration
  | MDInterface InterfaceDeclaration

type alias MethodOrFieldDecl =
  { type_ : Type 
  , identifier : String
  , rest : MethodOrFieldRest
  }

type MethodOrFieldRest
  = FieldRest FieldDeclaratorsRest
  | MethodRest MethodDeclaratorRest

type alias FieldDeclaratorsRest =
  { varaibleRest : VariableDeclaratorRest
  , more : List VariableDeclarator
  }

type alias MethodDeclaratorRest =
  { formalParams : FormalParameters
  , arrays : Int
  , throws : List String
  , block : Maybe Block
  }

type alias VoidMethodDeclaratorRest =
  { formalParams : FormalParameters
  , throws : List String
  , block : Maybe Block
  }

type alias ConstructorDeclaratorRest =
  { formalParams : FormalParameters
  , throws : List String
  , block : Block
  }

type alias GenericMethodOrConstructorDecl =
  {
  }

type alias GenericMethodOrConstructorRest =
  {
  }

---- ---- vi

type alias InterfaceBody =
  { declarations : List InterfaceBodyDeclaration
  }

type InterfaceBodyDeclaration
  = InterfaceSemi 
  | InterfaceBodyMember
    { modifiers : List Modifier
    , decl : InterfaceMemberDecl
    }

type InterfaceMemberDecl
  = IMDMethodOrField InterfaceMethodOrFieldDecl
  | IMDGeneric InterfaceGenericMethodDecl
  | IMDClass ClassDeclaration
  | IMDInterface InterfaceDeclaration
  | IMDVoid 
    { identifier : String
    , rest : VoidInterfaceMethodDeclaratorRest
    }

type alias InterfaceMethodOrFieldDecl =
  { type_ : Type
  , identifier : String
  , rest : InterfaceMethodOrFieldRest
  }

type InterfaceMethodOrFieldRest
  = IMDOFRConstant (List ConstantDeclarator)
  | IMDOFR InterfaceMethodDeclaratorRest

type alias ConstantDeclaratorRest =
  { arrays : Int
  , initializer : VariableInitializer
  }

type alias ConstantDeclarator =
  { identifier : String
  , rest : ConstantDeclaratorRest
  }

type alias InterfaceMethodDeclaratorRest =
  { formalParams : FormalParameters
  , arrays : Int
  , throws : List String
  }

type alias VoidInterfaceMethodDeclaratorRest =
  { formalParams : FormalParameters
  , throws : List String
  }

type alias InterfaceGenericMethodDecl =
  { typeParams : Maybe Type
  , identifier : String
  , rest : InterfaceMethodDeclaratorRest
  }

---- ---- vii

type alias FormalParameters = Maybe FormalParameterDecls

type alias FormalParameterDecls =
  { modifiers : List VariableModifier
  , type_ : Type
  , rest : FormalParameterDeclsRest
  }

type VariableModifier
  = VMFinal
  | MVAnnotation Annotation

type FormalParameterDeclsRest
  = NormalParams
    { identifier : VariableDeclaratorId
    , more : Maybe FormalParameterDecls
    }
  | VarParams VariableDeclaratorId

type alias VariableDeclaratorId =
  { identifier : String
  , array : Int
  }

type VariableDeclarator =
  VariableDeclarator String VariableDeclaratorRest

type alias VariableDeclaratorRest =
  { arrayBrackets : Int
  , initializer : Maybe VariableInitializer
  }

type VariableInitializer
  = VIArray ArrayInitializer
  | VIExpression Expression

type alias ArrayInitializer = List VariableInitializer

---- ---- viii

type alias Block = List BlockStatement

type BlockStatement
  = BlockVaraible LocalVariableDeclarationStatement
  | BlockClassOrInterface ClassOrInterfaceDeclaration
  | BlockStatement
    { label : Maybe String
    , statement : Statement
    }

type alias LocalVariableDeclarationStatement =
  { modifiers : List VariableModifier
  , type_ : Type
  , declararators : List VariableDeclarator
  }

type Statement
  = StatementBlock Block
  | StatementSemicolon
  | StatementLabel
    { label : String, statement : Statement }
  | StatementExpression Expression
  | StatementIf
    { cond : ParExpression, if_ : Statement, else_ : Maybe Statement }
  | StatementAssert
    { expression : Expression
    , throws : Maybe Expression
    }
  | StatementSwitch
    { expression : ParExpression
    , groups : List SwitchBlockStatementGroups
    }
  | StatementWhile
    { cond : ParExpression
    , statement : Statement
    }
  | StatementDo
    { cond : ParExpression
    , statement : Statement
    }
  | StatementFor
    { control : ForControl
    , statement : Statement
    }
  | StatementBreak (Maybe String)
  | StatementContinue (Maybe String)
  | StatementReturn (Maybe Expression)
  | StatementThrow Expression
  | StatementSynchronized
    { expression : ParExpression
    , block : Block
    }
  {-
  | StatementTry
    { block : Block
    , (Catches | [Catches] Finally)
    }
  | StatementTryResource
    { spec : ResourceSpecification
    , block : Block
    , [Catches] [Finally]
    }
  -}

----_---- ix

--- catches ...

----_---- x

type alias SwitchBlockStatementGroups =
  List SwitchBlockStatementGroup

type alias SwitchBlockStatementGroup =
  { labels : List SwitchLabel
  --, blockStatements : BlockStatements
  }

type SwitchLabel
  = SwitchLabelExp Expression
  | SwitchLabelEnum String
  | SwitchLabelDefault

type ForControl = TODO1
{-
    ForVarControl
    ForInit ; [Expression] ; [ForUpdate]

ForVarControl:
    {VariableModifier} Type VariableDeclaratorId  ForVarControlRest

ForVarControlRest:
    ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
    : Expression

ForVariableDeclaratorsRest:
    [= VariableInitializer] { , VariableDeclarator }

ForInit: 
ForUpdate:
    StatementExpression { , StatementExpression }    
  -}

---- ---- xi

type Expression =
  Expression
    { exp : Expression1
    , rest : Maybe (AssignmentOperator, Expression1)
    }

type AssignmentOperator
  = Assign
  | PlusAssign
  | MinusAssign
  | MultiplyAssign
  | DivideAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | ModAssign
  | LShiftAssign
  | RShiftAssign
  | RShiftAssign2

type Expression1 =
  Expression1
    { exp2 : Expression2
    , rest : Maybe (Expression, Expression1)
    }

type Expression2 =
  Expression2
    { exp3 : Expression3 
    , rest : Expression2Rest
    }

type Expression2Rest
  = E2RInfixOp (List (InfixOp, Expression3))
  | E2RInstanceof Type

---- ---- xii

type InfixOp
  = LogicalOr
  | LogicalAnd
  | BitwizeOr
  | BitwizeXor
  | BitwizeAnd
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | LeftShift
  | RightShift
  | TripleRightShift
  | Plus
  | Minus
  | Multiply
  | Divide
  | Mod

type Expression3
  = E3Prefix
    { op : PrefixOp
    , exp3 : Expression3
    }
  | E3WHAT --( (Expression | Type) ) Expression3
  | E3Primary Primary --{ Selector } { PostfixOp }

type PrefixOp
  = PreIncrement
  | PreDecrement
  | LogicalNot
  | BitwizeNot
  | PrefixPlus
  | PrefixMinus

type PostfixOp
  = PostIncrement
  | PostDecrement

----_---- xiii

type Primary
  = PrimaryLiteral Literal
  | PrimaryParExpression ParExpression
  | PrimaryThis (List Expression)
  | PrimarySuper SuperSuffix
{-| new Creator
  | NonWildcardTypeArguments (ExplicitGenericInvocationSuffix | this (List Expression))
  | Identifier { . Identifier } [IdentifierSuffix]
  | BasicType {[]} . class
  | void . class
-}

type Literal
  = IntegerLiteral Int
  | FloatingPointLiteral Float
  | CharacterLiteral Char
  | StringLiteral String
  | BooleanLiteral Bool
  | NullLiteral

type ParExpression = Par Expression

type SuperSuffix
  = SuperSuffixArgs (List Expression) 
  | SuperSuffixDotArgs String (List Expression)

type ExplicitGenericInvocationSuffix
  = EGISSuper SuperSuffix
  | EGISIdentifier -- String (List Expression) TODO

----_---- xiv

--- creator

----_---- xv

type alias EnumBody =
  { constants : List EnumConstant
  , declarations : List ClassBodyDeclaration
  }

type alias EnumConstant =
  { annotations : List Annotation
  , identifier : String
  , arguments : List Expression
  , body : Maybe ClassBody
  }

type AnnotationTypeBody = TODO22

----

reserved : Set String
reserved = Set.fromList ["class", "package", "public", "static", "int"]

{-
CompilationUnit: 
    package imports types
-}
compilationUnit : Parser CompilationUnit
compilationUnit =
  P.succeed CompilationUnit
    |. P.spaces
    |= optional package
    |. P.spaces
    |= imports
    |. P.spaces
    |= types
    |. P.spaces
    |. P.end

{-
[[Annotations] package QualifiedIdentifier ;]
-}
package : Parser String
package =
  P.succeed identity
    |. P.keyword "package"
    |. P.spaces
    |= qualifiedIdentifier
    |. P.spaces
    |. P.symbol ";"

qualifiedIdentifier : Parser String
qualifiedIdentifier = P.map (String.join ".") (dotted identifier)

qualifiedIdentifierList : Parser (List String)
qualifiedIdentifierList =
  P.succeed (::)
  |= qualifiedIdentifier
  |= optionalList
     ( P.succeed identity
       |. P.keyword ","
       |= commas qualifiedIdentifier
     )
 
dotted : Parser a -> Parser (List a)
dotted item =
  P.sequence
    { start = ""
    , separator = "."
    , end = ""
    , spaces = P.succeed ()
    , item = item
    , trailing = P.Forbidden
    }

identifier : Parser String
identifier =
  P.variable
    { start = \c -> Char.isAlpha c || c == '_'
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = reserved
    }

{-
{ImportDeclaration}
ImportDeclaration: 
    import [static] Identifier { . Identifier } [. *] ;
-}
imports : Parser (List ImportDeclaration)
imports = list <|
  P.succeed ImportDeclaration
    |. P.keyword "import"
    |. P.spaces
    |= P.oneOf [P.map (always True) (P.keyword "static"), P.succeed False]
    |. P.spaces
    |= P.map (String.join ".") (dotted identifier)
    |. P.spaces
    |. P.symbol ";"

{-
{TypeDeclaration}
TypeDeclaration: 
    ClassOrInterfaceDeclaration
    ;
-}
types : Parser (List TypeDeclaration)
types = list <|
  P.oneOf
    [ P.map ClassOrInterface classOrInterfaceDeclaration
    , P.succeed Semicolon |. P.symbol ";"
    ]

{-
ClassOrInterfaceDeclaration: 
    {Modifier} (ClassDeclaration | InterfaceDeclaration)
-}
classOrInterfaceDeclaration : Parser ClassOrInterfaceDeclaration
classOrInterfaceDeclaration =
  P.succeed (\a b -> b a)
    |= list modifier
    |. P.spaces
    |= P.oneOf 
       [ P.map (\x y -> Class y x) classDeclaration
       , P.map (\x y -> Interface y x) interfaceDeclaration
       ]

modifier : Parser Modifier
modifier =
  P.oneOf
    [ P.succeed Public       |. P.keyword "public"
    , P.succeed Protected    |. P.keyword "protected"
    , P.succeed Private      |. P.keyword "private"
    , P.succeed Static       |. P.keyword "static"
    , P.succeed Abstract     |. P.keyword "abstract"
    , P.succeed Final        |. P.keyword "final"
    , P.succeed Native       |. P.keyword "native"
    , P.succeed Synchronized |. P.keyword "synchronized"
    , P.succeed Transient    |. P.keyword "transient"
    , P.succeed Volatile     |. P.keyword "volatile"
    , P.succeed Strictfp     |. P.keyword "strictfp"
    ]

classDeclaration : Parser ClassDeclaration
classDeclaration =
  P.oneOf
    [ P.map NormalClass normalClassDeclaration
    , P.map Enum enumDeclaration
    ]

interfaceDeclaration : Parser InterfaceDeclaration
interfaceDeclaration =
  P.oneOf
    [ P.map IDNormal normalInterfaceDeclaration
    ]

{-
NormalClassDeclaration: 
    class Identifier [TypeParameters]
                                [extends Type] [implements TypeList] ClassBody
-}
normalClassDeclaration : Parser NormalClassDeclaration
normalClassDeclaration =
  P.succeed NormalClassDeclaration
  |. P.keyword "class"
  |. P.spaces
  |= identifier
  |. P.spaces
  |= optionalList typeArguments
  |. P.spaces
  |= optional
     (P.succeed identity |. P.keyword "extends" |. P.spaces |= type_)
  |. P.spaces
  |= optionalList
     (P.succeed identity |. P.keyword "implements" |. P.spaces |= typeList)
  |. P.spaces
  |= classBody
 
normalInterfaceDeclaration : Parser NormalInterfaceDeclaration
normalInterfaceDeclaration =
  P.succeed NormalInterfaceDeclaration
  |. P.keyword "interface"
  |. P.spaces
  |= identifier
  |. P.spaces
  |= optionalList typeArguments
  |. P.spaces
  |= optionalList 
     (P.succeed identity |. P.keyword "extends" |. P.spaces |= typeList)
  |. P.spaces
  |= interfaceBody

interfaceBody : Parser InterfaceBody
interfaceBody =
  P.succeed InterfaceBody
  |. P.symbol "{"
  |. P.spaces
  |= list interfaceBodyDeclaration
  |. P.spaces
  |. P.symbol "}"

optional : Parser a -> Parser (Maybe a)
optional parser =
  P.oneOf
    [ P.map Just parser
    , P.succeed Nothing
    ]

optionalList : Parser (List a) -> Parser (List a)
optionalList parser = P.map (Maybe.withDefault []) (optional parser)

optionalBool : Parser a -> Parser Bool
optionalBool parser =
  (optional parser)
  |> P.map (\maybe ->
      case maybe of
        Just _ -> True
        Nothing -> False)

typeList : Parser (List Type)
typeList =
  commas type_

commas : Parser a -> Parser (List a)
commas parser =
  P.sequence
    { start = ""
    , separator = ","
    , end = ""
    , spaces = P.spaces
    , item = parser
    , trailing = P.Forbidden
    }

list : Parser a -> Parser (List a)
list p = 
  P.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = P.spaces
    , item = p
    , trailing = P.Mandatory
    }

repeated : Parser a -> Parser (List a)
repeated p = 
  P.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = P.succeed ()
    , item = p
    , trailing = P.Forbidden
    }

basicType : Parser BasicType
basicType =
  P.oneOf 
    [ P.succeed Byte    |. P.keyword "byte"
    , P.succeed Short   |. P.keyword "short"
    , P.succeed Char    |. P.keyword "char"
    , P.succeed Int     |. P.keyword "int"
    , P.succeed Long    |. P.keyword "long"
    , P.succeed Float   |. P.keyword "float"
    , P.succeed Double  |. P.keyword "double"
    , P.succeed Boolean |. P.keyword "boolean"
    ]

referenceType : Parser ReferenceType
referenceType =
  P.succeed ReferenceType
  |= P.map (String.join ".") (dotted identifier)
  |= optionalList typeArguments

{- TypeArguments: 
    < TypeArgument { , TypeArgument } > -}
typeArguments : Parser (List TypeArgument)
typeArguments =
  P.succeed identity
  |. P.symbol "<"
  |. P.spaces
  |= commas typeArgument
  |. P.spaces
  |. P.symbol ">"

typeArgument : Parser TypeArgument
typeArgument =
  P.map ReferenceTypeArgument (P.lazy (\_ -> referenceType))

type_ : Parser Type
type_ =
  P.succeed (nTimes ArrayType)
  |= P.oneOf
       [ P.map BasicType basicType
       , P.map RefType referenceType
       ]
  |= brackets

brackets : Parser Int
brackets =
  P.map List.length <| repeated (P.symbol "[]")

nTimes : (a -> a) -> a -> Int -> a
nTimes func x n =
  case n of
    0 -> x
    num -> nTimes func (func x) (num - 1)

{-
ClassBody: 
    { { ClassBodyDeclaration } } -}
classBody : Parser ClassBody
classBody =
  P.succeed ClassBody
  |. P.symbol "{"
  |. P.spaces
  |= list classBodyDeclaration
  |. P.spaces
  |. P.symbol "}"

{-
ClassBodyDeclaration:
    ; 
    {Modifier} MemberDecl
    [static] Block -}
classBodyDeclaration : Parser ClassBodyDeclaration
classBodyDeclaration =
  P.oneOf
    [ P.succeed CBSemicolon |. P.symbol ";"
    , P.succeed (\mods decl -> CBMember { modifiers = mods, decl = decl})
        |= list modifier
        |. P.spaces
        |= memberDecl
    , P.succeed (\static blok -> CBBlock { static = static, block = blok })
        |= optionalBool (P.keyword "static" |. P.spaces)
        |. P.spaces
        |= block
    ]

interfaceBodyDeclaration : Parser InterfaceBodyDeclaration
interfaceBodyDeclaration =
  P.oneOf
    [ P.succeed InterfaceSemi |. P.symbol ";"
    , P.succeed (\mods decl -> InterfaceBodyMember { modifiers = mods, decl = decl})
        |= list modifier
        |. P.spaces
        |= interfaceMemberDecl
    ]

interfaceMemberDecl = P.oneOf []

memberDecl : Parser MemberDecl -- TODO backtrack
memberDecl =
  P.oneOf
    [ P.map MDClass (P.lazy (\_ -> classDeclaration))
    , P.map MDInterface interfaceDeclaration
    , P.succeed (\id rest -> MDVoidMethod { identifier = id, rest = rest })
      |. P.keyword "void"
      |. P.spaces
      |= identifier
      |. P.spaces
      |= voidMethodDeclaratorRest
    , P.map MDGenericMethodOrConstructor genericMethodOrConstructorDecl
    , methodFieldOrConstructor
    ]

genericMethodOrConstructorDecl : Parser GenericMethodOrConstructorDecl
genericMethodOrConstructorDecl = P.oneOf []

voidMethodDeclaratorRest : Parser VoidMethodDeclaratorRest
voidMethodDeclaratorRest =
  P.succeed VoidMethodDeclaratorRest
    |= formalParameters
    |. P.spaces
    |= optionalList
       ( P.succeed identity
         |. P.keyword "throws"
         |. P.spaces
         |= qualifiedIdentifierList
       )
    |. P.spaces
    |= blockOrSemi

blockOrSemi : Parser (Maybe Block)
blockOrSemi =
  P.oneOf
    [ P.map Just block
    , P.succeed Nothing |. P.symbol ";"
    ]

methodFieldOrConstructor : Parser MemberDecl
methodFieldOrConstructor =
  P.oneOf -- TODO remove backtracking
    [ P.backtrackable <| P.map MDMethodOrField methodOrFieldDecl
    , P.succeed (\id rest -> MDConstructor { identifier = id, rest = rest })
        |= identifier
        |. P.spaces
        |= constructorDeclaratorRest
    ]

{-
MethodOrFieldDecl:
    Type Identifier MethodOrFieldRest -}
methodOrFieldDecl : Parser MethodOrFieldDecl
methodOrFieldDecl =
  P.succeed MethodOrFieldDecl
    |= type_
    |. P.spaces
    |= identifier
    |. P.spaces
    |= methodOrFieldRest

constructorDeclaratorRest : Parser ConstructorDeclaratorRest
constructorDeclaratorRest
  = P.succeed ConstructorDeclaratorRest
    |= formalParameters
    |. P.spaces
    |= optionalList
       ( P.succeed identity
         |. P.keyword "throws"
         |. P.spaces
         |= qualifiedIdentifierList
       )
    |. P.spaces
    |= block

{-
MethodOrFieldRest:  
    FieldDeclaratorsRest ;
    MethodDeclaratorRest -}
methodOrFieldRest : Parser MethodOrFieldRest
methodOrFieldRest =
  P.oneOf
    [ P.map FieldRest (fieldDeclaratorsRest |. P.spaces |. P.symbol ";")
    , P.map MethodRest methodDeclaratorsRest
    ]

{-
FieldDeclaratorsRest:  
    VariableDeclaratorRest { , VariableDeclarator } -}
fieldDeclaratorsRest : Parser FieldDeclaratorsRest
fieldDeclaratorsRest =
  P.succeed FieldDeclaratorsRest --(\vRest more -> { varaibleRest = vRest , more = more })
  |= variableDeclaratorRest
  |= optionalList
     ( P.succeed identity
       |. P.spaces
       |. P.symbol ","
       |. P.spaces
       |= list variableDeclarator
     )

methodDeclaratorsRest : Parser MethodDeclaratorRest
methodDeclaratorsRest =
  P.succeed MethodDeclaratorRest
  |= formalParameters
  |. P.spaces
  |= brackets
  |. P.spaces
  |= optionalList
     ( P.succeed identity
       |. P.keyword "throws"
       |. P.spaces
       |= qualifiedIdentifierList
     )
  |. P.spaces
  |= blockOrSemi

formalParameters : Parser FormalParameters
formalParameters =
  P.succeed identity
  |. P.symbol "("
  |= optional formalParameterDecls
  |. P.symbol ")"

formalParameterDecls : Parser FormalParameterDecls
formalParameterDecls =
  P.succeed (\mods t rest -> { modifiers = mods, type_ = t, rest = rest})
  |= optionalList (list variableModifier)
  |. P.spaces
  |= type_
  |. P.spaces
  |= formalParameterDeclsRest

variableModifier : Parser VariableModifier
variableModifier =
  P.oneOf
    [ P.succeed VMFinal |. P.keyword "final"
    , P.map MVAnnotation annotation
    ]

annotation : Parser Annotation
annotation = P.oneOf []

formalParameterDeclsRest : Parser FormalParameterDeclsRest
formalParameterDeclsRest =
  P.oneOf
    [ P.succeed (\id more -> NormalParams { identifier = id, more = more })
      |= variableDeclaratorId
      |= optional
         ( P.succeed identity
           |. P.spaces
           |. P.symbol ","
           |. P.spaces
           |= P.lazy (\_ -> formalParameterDecls)
         )
    , P.map VarParams
      ( P.succeed identity
        |. P.symbol "..."
        |. P.spaces
        |= variableDeclaratorId
      )
    ]

variableDeclaratorId : Parser VariableDeclaratorId
variableDeclaratorId =
  P.succeed VariableDeclaratorId
  |= identifier
  |. P.spaces
  |= brackets

{-
VariableDeclarator:
    Identifier VariableDeclaratorRest
-}
variableDeclarator : Parser VariableDeclarator
variableDeclarator =
  P.succeed VariableDeclarator
  |= identifier
  |= variableDeclaratorRest

{-
VariableDeclaratorRest:
    {[]} [ = VariableInitializer ]
-}
variableDeclaratorRest : Parser VariableDeclaratorRest
variableDeclaratorRest =
  P.succeed (\brkts init -> { arrayBrackets = brkts, initializer = init })
  |= brackets
  |= optional
     ( P.succeed identity
       |. P.spaces
       |. P.symbol "="
       |. P.spaces
       |= variableInitializer
     )

{-
VariableInitializer:
    ArrayInitializer
    Expression
-}
variableInitializer : Parser VariableInitializer
variableInitializer =
  P.oneOf
    [ P.map VIArray arrayInitializer
    , P.map VIExpression expression
    ]

-- TODO pick up here
{-
ArrayInitializer:
    { [ VariableInitializer { , VariableInitializer } [,] ] }
-}
arrayInitializer : Parser ArrayInitializer
arrayInitializer =
  P.succeed identity
  |. P.symbol "{"
  |= optionalList
      ( P.lazy
        (\_ -> commas variableInitializer
               |. optional (P.symbol ",")
        )
      )
  |. P.symbol "}"

{-
Expression: 
    Expression1 [AssignmentOperator Expression1]
-}
expression : Parser Expression
expression =
  P.succeed (\exp rest -> Expression { exp = exp, rest = rest })
  |= expression1
  |. P.spaces
  |= optional
     ( P.succeed Tuple.pair
       |= assignmentOperator
       |. P.spaces
       |= expression1
     )

{-
AssignmentOperator: 
    = += -= *= /= &= |= ^= %= <<= >>= >>>=
-}
assignmentOperator : Parser AssignmentOperator
assignmentOperator =
  P.oneOf
    [ P.succeed Assign         |. P.symbol "="
    , P.succeed PlusAssign     |. P.symbol "+="
    , P.succeed MinusAssign    |. P.symbol "-="
    , P.succeed MultiplyAssign |. P.symbol "*="
    , P.succeed DivideAssign   |. P.symbol "/="
    , P.succeed AndAssign      |. P.symbol "&="
    , P.succeed OrAssign       |. P.symbol "|="
    , P.succeed XorAssign      |. P.symbol "^="
    , P.succeed ModAssign      |. P.symbol "%="
    , P.succeed LShiftAssign   |. P.symbol "<<="
    , P.succeed RShiftAssign   |. P.symbol ">>="
    , P.succeed RShiftAssign2  |. P.symbol ">>>="
    ]

{-
Expression1: 
    Expression2 [Expression1Rest]
Expression1Rest: 
    ? Expression : Expression1
-}
expression1 : Parser Expression1
expression1 =
  P.succeed (\exp2 rest -> Expression1 { exp2 = exp2, rest = rest })
  |= expression2
  |. P.spaces
  |= optional expression1Rest

expression1Rest : Parser (Expression, Expression1)
expression1Rest =
  P.succeed Tuple.pair
  |. P.symbol "?"
  |. P.spaces
  |= (P.lazy (\_ -> expression))
  |. P.spaces
  |. P.symbol ":"
  |. P.spaces
  |= (P.lazy (\_ -> expression1))

{-
Expression2:
    Expression3 [Expression2Rest]
-}
expression2 : Parser Expression2
expression2 =
  P.succeed (\exp3 rest -> Expression2 { exp3 = exp3, rest = rest })
  |= expression3
  |. P.spaces
  |= expression2Rest

{-
Expression2Rest:
    { InfixOp Expression3 }
    instanceof Type
-}
expression2Rest : Parser Expression2Rest
expression2Rest = 
  let
    infix : Parser (InfixOp, Expression3)
    infix =
      P.succeed Tuple.pair
      |= infixOp
      |. P.spaces
      |= expression3
    instanceof : Parser Type
    instanceof =
      P.succeed identity
      |. P.keyword "instanceof"
      |= type_
  in
    P.oneOf
      [ P.map E2RInfixOp (list infix)
      , P.map E2RInstanceof instanceof
      ]

infixOp : Parser InfixOp
infixOp =
  P.oneOf
    [ P.succeed LogicalOr        |. P.keyword " |"
    , P.succeed LogicalAnd       |. P.keyword "&&"
    , P.succeed BitwizeOr        |. P.keyword "|"
    , P.succeed BitwizeXor       |. P.keyword "^"
    , P.succeed BitwizeAnd       |. P.keyword "&"
    , P.succeed Equal            |. P.keyword "=="
    , P.succeed NotEqual         |. P.keyword "!="
    , P.succeed LessThan         |. P.keyword "<"
    , P.succeed GreaterThan      |. P.keyword ">"
    , P.succeed LessThanEqual    |. P.keyword "<="
    , P.succeed GreaterThanEqual |. P.keyword ">="
    , P.succeed LeftShift        |. P.keyword "<<"
    , P.succeed RightShift       |. P.keyword ">>"
    , P.succeed TripleRightShift |. P.keyword ">>>"
    , P.succeed Plus             |. P.keyword "+"
    , P.succeed Minus            |. P.keyword "-"
    , P.succeed Multiply         |. P.keyword "*"
    , P.succeed Divide           |. P.keyword "/"
    , P.succeed Mod              |. P.keyword "%"
    ]

{- 
Expression3: 
    PrefixOp Expression3
    ( (Expression | Type) ) Expression3
    Primary { Selector } { PostfixOp } -}
expression3 : Parser Expression3
expression3 = -- TODO
  (P.succeed E3Primary) |= primary

prefixOp : Parser PrefixOp
prefixOp =
  P.oneOf
    [ P.succeed PreIncrement |. P.keyword "++"
    , P.succeed PreDecrement |. P.keyword "--"
    , P.succeed LogicalNot   |. P.keyword "!"
    , P.succeed BitwizeNot   |. P.keyword "~"
    , P.succeed PrefixPlus   |. P.keyword "+"
    , P.succeed PrefixMinus  |. P.keyword "-"
    ]

postfixOp : Parser PostfixOp
postfixOp =
  P.oneOf
    [ P.succeed PostIncrement |. P.keyword "++"
    , P.succeed PostDecrement |. P.keyword "--"
    ]

primary : Parser Primary
primary = -- TODO
  P.map PrimaryLiteral literal

superSuffix : Parser SuperSuffix
superSuffix = P.oneOf [] -- use `arguments` function

explicitGenericInvocationSuffix : Parser ExplicitGenericInvocationSuffix
explicitGenericInvocationSuffix = P.oneOf [] -- use `arguments`

arguments : Parser (List Expression)
arguments =
  P.succeed identity
  |. P.symbol "("
  |. P.spaces
  |= commas expression
  |. P.spaces
  |. P.symbol ")"

literal : Parser Literal
literal =
  P.oneOf
    [ P.map BooleanLiteral bool
    , P.succeed NullLiteral |. P.keyword "null"
    , P.map CharacterLiteral char
    , P.map StringLiteral stringLiteral
    , P.number
      { int = Just IntegerLiteral
      , hex = Nothing
      , octal = Nothing
      , binary = Nothing
      , float = Just FloatingPointLiteral
      }
    ]

char : Parser Char
char =
  P.succeed identity
  |. P.symbol "\'"
  |= P.map (Maybe.withDefault 'z' << Maybe.map Tuple.first << String.uncons)
        (P.getChompedString (P.chompIf (always True)))
  |. P.symbol "\'"

bool : Parser Bool
bool =
  P.oneOf
    [ P.succeed True  |. P.keyword "true"
    , P.succeed False |. P.keyword "false"
    ]

stringLiteral : Parser String
stringLiteral =
  P.succeed identity
  |. P.symbol "\""
  |= P.getChompedString (P.chompWhile (\c -> c /= '\"'))
  |. P.symbol "\""

block : Parser Block
block =
  P.succeed identity
  |. P.symbol "{"
  |. P.spaces
  |= list blockStatement
  |. P.spaces
  |. P.symbol "}"

blockStatement : Parser BlockStatement
blockStatement =
  P.oneOf
    [ P.map BlockClassOrInterface (P.lazy (\_ -> classOrInterfaceDeclaration))
    ]

enumDeclaration : Parser EnumDeclaration
enumDeclaration =
  P.succeed EnumDeclaration
  |. P.keyword "enum"
  |. P.spaces
  |= identifier
  |. P.spaces
  |= optionalList
     (P.succeed identity |. P.keyword "implements" |. P.spaces |= typeList)
  |. P.spaces
  |= enumBody

enumBody : Parser EnumBody
enumBody =
  P.succeed EnumBody
  |. P.symbol "{"
  |= commas enumConstant
  |. optional (P.symbol ",")
  |= commas classBodyDeclaration
  |. P.symbol "}"

{-
EnumConstant:
    [Annotations] Identifier [Arguments] [ClassBody] -}
enumConstant : Parser EnumConstant
enumConstant =
  P.succeed EnumConstant
  |= list annotation
  |. P.spaces
  |= identifier
  |. P.spaces
  |= optionalList arguments
  |. P.spaces
  |= optional classBody
