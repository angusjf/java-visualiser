module JavaParser exposing (..)

import Set exposing (Set)
import Parser as P exposing (Parser, (|=), (|.), Step)

type alias CompilationUnit
  = { package : Maybe String
    , imports : List ImportDeclaration
    , types : List TypeDeclaration
    }

type ImportDeclaration
  = ImportDeclaration Bool (List String) -- Bool: Static?

type TypeDeclaration
  = ClassOrInterface ClassOrInterfaceDeclaration
  | Semicolon

type Modifier
  = Annotation
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

type ClassOrInterfaceDeclaration
  = Class (List Modifier) ClassDeclaration
  | Interface (List Modifier) InterfaceDeclaration

type ClassDeclaration
  = NormalClass NormalClassDeclaration
  | Enum EnumDeclaration

type InterfaceDeclaration
  = InterfaceDec

type alias NormalClassDeclaration =
  { identifier : String
  , extends : Maybe Type
  , implements : List Type
  , body : ClassBody
  }

type Type
  = BasicType BasicType
  | ReferenceType ReferenceType
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

type ReferenceType = RT String (List TypeArgument) -- TODO can be dotted

type TypeArgument
  = ReferenceTypeArgument ReferenceType
  | WildCardSuper ReferenceType
  | WildCardExtends ReferenceType

type EnumDeclaration
  = EnumDec

type ClassBody = ClassBody (List ClassBodyDeclaration)

type ClassBodyDeclaration
  = CBSemicolon
  | CBMember (List Modifier) MemberDecl
  | CBBlock Bool Block -- Bool: Static?

type MemberDecl
  = MDMethodOrField MethodOrFieldDecl
  | MDVoidMethod String VoidMethodDeclaratorRest -- String: Identifier
  | MDConstructor ConstructorDeclaratorRest
  | MDGenericMethodOrConstructor GenericMethodOrConstructorDecl
  | MDClass ClassDeclaration
  | MDInterface InterfaceDeclaration

type MethodOrFieldDecl = MethodOrFieldDecl Type String MethodOrFieldRest

type MethodOrFieldRest
  = FieldRest FieldDeclaratorsRest
  | MethodRest MethodDeclaratorRest

type FieldDeclaratorsRest = TODO1

type MethodDeclaratorRest = TODO5

type VoidMethodDeclaratorRest = TODO2

type ConstructorDeclaratorRest = TODO3

type GenericMethodOrConstructorDecl = TODO4

type Block = Block

type VariableDeclarator =
  VariableDeclarator String VariableDeclaratorRest

type VariableDeclaratorRest
  = VariableDeclaratorRest (Int, (Maybe VariableInitializer))

type VariableInitializer
  = VIArray ArrayInitializer
  | VIExpression Expression

type ArrayInitializer =
    Maybe (VariableInitializer, (List VariableInitializer))

type Expression
  = Expression Expression1 (Maybe (AssignmentOperator, Expression1))

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

type Expression1 = Expression1 Expression2 (Maybe Expression1Rest)

type Expression1Rest = Expression1Rest Expression Expression1

type Expression2 = Expression2 Expression3 (Maybe Expression2Rest)

type Expression2Rest = Expression2Rest (List (InfixOp, Expression3)) Type

type Expression3 = TODO10

type InfixOp = TODO11

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
    , inner = \c -> Char.isAlphaNum c || c == 'c'
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
    |= dotted identifier -- TODO
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
  let
    modifiers = list modifier
    modifiersAndClass =
      P.succeed Class
        |= modifiers
        |. P.spaces
        |= classDeclaration
  in
    P.oneOf
      [ modifiersAndClass
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
classDeclaration = P.map NormalClass normalClassDeclaration

{-
NormalClassDeclaration: 
    class Identifier [TypeParameters]
                                [extends Type] [implements TypeList] ClassBody
-}
normalClassDeclaration : Parser NormalClassDeclaration
normalClassDeclaration =
  let
    ext : Parser Type
    ext = P.succeed identity
             |. P.keyword "extends"
             |. P.spaces
             |= type_
    impl : Parser (List Type)
    impl = P.succeed identity
             |. P.keyword "implements"
             |. P.spaces
             |= typeList
  in
    P.succeed NormalClassDeclaration
      |. P.keyword "class"
      |. P.spaces
      |= identifier
      |. P.spaces
      |= optional ext
      |. P.spaces
      |= P.map (Maybe.withDefault []) (optional impl)
      |= classBody

optional : Parser a -> Parser (Maybe a)
optional parser =
  P.oneOf
    [ P.map Just parser
    , P.succeed Nothing
    ]

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
  P.succeed RT
  |= identifier
  |= P.map (\x -> case x of
                    Just xs -> xs
                    Nothing -> []) (optional typeArguments)

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
  P.succeed (\i -> ReferenceTypeArgument (RT i [])) -- TODO fix for nesting
  |= identifier

type_ : Parser Type
type_ =
  P.succeed (nTimes ArrayType)
  |= P.oneOf
       [ P.map BasicType basicType
       , P.map ReferenceType referenceType
       ]
  |= brackets

brackets : Parser Int
brackets =
  P.map List.length <| repeated (P.symbol "[]")

nTimes : (a -> a) -> a -> Int -> a
nTimes f x n =
  case n of
    0 -> x
    num -> nTimes f (f x) (num - 1)

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
    , P.succeed CBMember
        |= list modifier
        |. P.spaces
        |= memberDecl
    ]

memberDecl : Parser MemberDecl
memberDecl =
  P.oneOf
    [ P.map MDMethodOrField methodOrFieldDecl
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
    |= methodOrFieldRest

{-
MethodOrFieldRest:  
    FieldDeclaratorsRest ;
    MethodDeclaratorRest -}
methodOrFieldRest : Parser MethodOrFieldRest
methodOrFieldRest =
  P.oneOf
    [ P.map FieldRest (fieldDeclaratorsRest |. P.spaces |. P.symbol ";")
    ]

{-
FieldDeclaratorsRest:  
    VariableDeclaratorRest { , VariableDeclarator } -}
fieldDeclaratorsRest : Parser FieldDeclaratorsRest
fieldDeclaratorsRest = P.succeed TODO1

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
variableDeclaratorRest = Debug.todo ""

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

{-
ArrayInitializer:
    { [ VariableInitializer { , VariableInitializer } [,] ] }
-}
arrayInitializer : Parser ArrayInitializer
arrayInitializer = Debug.todo ""

{-
Expression: 
    Expression1 [AssignmentOperator Expression1]
-}
expression : Parser Expression
expression = Debug.todo ""
{-
  P.succeed Expression
  |= expression1
  |. P.spaces
  |= optional (assignmentOperator |. P.spaces |= expression1)
-}

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
-}
expression1 : Parser Expression1
expression1 = Debug.todo ""
    {-
  P.succeed Expression1
  |= expression2
  |. P.spaces
  |= optional expression1Rest
  -}

{-
Expression1Rest: 
    ? Expression : Expression1
-}
expression1Rest : Parser Expression1
expression1Rest = Debug.todo ""
    {-
  P.succeed Expression1Rest
  |. P.symbol "?"
  |. P.spaces
  |= expression
  |. P.spaces
  |. P.symbol ":"
  |. P.spaces
  |= expression1
  -}

{-
Expression2:
    Expression3 [Expression2Rest]
-}
expression2 = Debug.todo ""

{-
Expression2Rest:
    { InfixOp Expression3 }
    instanceof Type
-}
expression2Rest = Debug.todo ""
