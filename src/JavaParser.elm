module JavaParser exposing (..)

import Set exposing (Set)
import Parser as P exposing (Parser, (|=), (|.))

type alias CompilationUnit
  = { package : Maybe String
    , imports : List ImportDeclaration
    , types : List TypeDeclaration
    }

type ImportDeclaration
  = ImportDeclaration Bool (List String)

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
  | BasicArrayType BasicType
  | ReferenceType ReferenceType
  | ReferenceArrayType ReferenceType

type BasicType
  = Byte
  | Short
  | Char
  | Int
  | Long
  | Float
  | Double
  | Boolean

type ReferenceType = RT String -- TODO

type EnumDeclaration
  = EnumDec

type ClassBody = CB () -- TODO

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
    |= package
    |. P.spaces
    |= imports
    |. P.spaces
    |= types
    |. P.spaces

{-
[[Annotations] package QualifiedIdentifier ;]
-}
package : Parser (Maybe String)
package = P.map Just <|
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
      |= orNot ext
      |. P.spaces
      |= P.map (Maybe.withDefault []) (orNot impl)
      |= classBody

classBody : Parser ClassBody
classBody = P.succeed (CB ())

orNot : Parser a -> Parser (Maybe a)
orNot parser =
  P.oneOf
    [ P.map Just parser
    , P.succeed Nothing
    ]

type_ : Parser Type
type_ =
  P.oneOf
    [ P.map BasicType basicType
    , P.map ReferenceType referenceType
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
referenceType = P.map (RT) identifier -- TODO

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
