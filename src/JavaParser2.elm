module JavaParser2 exposing (CompilationUnit, compilationUnit)

import Parser as P exposing (Parser, (|.), (|=))
import Set exposing (Set)
import Char exposing (Char)


--- {{{

-- MOCK TYPES
todo = Debug.todo "mock parser"
type InterfaceDeclaration = TODO
interfaceDeclaration = todo
type Annotation = TODO0
annotation = todo
type VariableInitializer = TODO1
variableInitializer = todo
type UnannType = TODO2
unannType = todo
type MethodDeclaration = TODO3
methodDeclaration = todo
type InstanceInitializer = TODO4
instanceInitializer = todo
type StaticInitializer = TODO5
staticInitializer = todo
type ConstructorDeclaration = TODO6
constructorDeclaration = todo
type EnumDeclaration = EnumDeclaration
enumDeclaration = todo

---}}}

type Either a b = Left a | Right b

-- helpers

or : (a -> x) -> (b -> x) -> Either a b -> x
or f g e =
    case e of
        Left a -> f a
        Right b -> g b

optional : Parser a -> Parser (Maybe a)
optional p =
  P.oneOf
    [ P.map Just p
    , P.succeed Nothing
    ]

nonEmptySep : String -> Parser a -> Parser (List a)
nonEmptySep sep p =
  P.succeed (::)
  |= p
  |. P.spaces
  |= sepBy sep p

sepBy : String -> Parser a -> Parser (List a)
sepBy sep p =
  P.sequence
    { start = ""
    , separator = sep
    , end = ""
    , spaces = P.spaces
    , item = p
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
    , trailing = P.Optional
    }

dotted : Parser a -> Parser (List a)
dotted = sepBy "."

-- Productions from §3 (Lexical Structure)

keywords : Set String
keywords =
  Set.fromList
    [ "abstract", "continue", "for", "new", "switch", "assert", "default"
    , "goto", "package", "synchronized", "boolean", "do", "if", "private"
    , "this", "break", "double", "implements", "protected", "throw", "byte"
    , "else", "import", "public", "throws", "case", "enum", "instanceof"
    , "return", "transient", "catch", "extends", "int", "short", "try", "char"
    , "final", "interface", "static", "void", "class", "finally", "long"
    , "strictfp", "volatile", "const", "float", "native", "super", "while"
    ]

type Identifier = Identifier String

identifier : Parser Identifier
identifier =
  P.map Identifier <|
    P.variable
      { start = javaLetter
      , inner = javaLetterOrDigit
      , reserved = Set.union keywords (Set.fromList ["true", "false", "null"])
      }

javaLetter : Char -> Bool
javaLetter c =  Char.isAlpha c || c == '_' || c == '$'

javaLetterOrDigit : Char -> Bool
javaLetterOrDigit c = javaLetter c || Char.isDigit c 


type TypeIdentifier = TypeIdentifier Identifier

typeIdentifier : Parser TypeIdentifier
typeIdentifier =
  P.andThen
    (\(Identifier str) ->
      if str == "var" || str == "yield"
        then P.problem <| str ++ " is not a valid type identifier"
        else P.succeed <| TypeIdentifier (Identifier str)
    )
    identifier


type UnqualifiedMethodIdentifier = UnqualifiedMethodIdentifier Identifier

unqualifiedMethodIdentifier : Parser UnqualifiedMethodIdentifier
unqualifiedMethodIdentifier = 
  P.andThen
    (\(Identifier str) ->
      if str == "yield"
        then P.problem "yield is not a valid type identifier"
        else P.succeed <| UnqualifiedMethodIdentifier (Identifier str)
    )
    identifier


type Literal
  = Literal_IntegerLiteral Int
  | Literal_FloatingPointLiteral Float
  | Literal_BooleanLiteral Bool
  | Literal_CharacterLiteral Char
  | Literal_StringLiteral String
  | Literal_TextBlock String
  | Literal_NullLiteral

literal : Parser Literal
literal =
  P.oneOf
    [-- P.succeed Literal_IntegerLiteral integerLiteral
    --, P.succeed Literal_FloatingPointLiteral floatingPointLiteral
    --, P.succeed Literal_BooleanLiteral booleanLiteral
    --, P.succeed Literal_CharacterLiteral characterLiteral
    --, P.succeed Literal_StringLiteral stringLiteral
    --, P.succeed Literal_TextBlock textBlock
    --, P.succeed Literal_NullLiteral nullLiteral
    ]


-- Productions from §4 (Types, Values, and Variables)

type Type
  = Type_PrimitiveType PrimitiveType
  | Type_ReferenceType ReferenceType

type_ : Parser Type
type_ =
  P.oneOf
    [ P.succeed Type_PrimitiveType
      |= primitiveType
    , P.succeed Type_ReferenceType
      |= referenceType
    ]


type PrimitiveType
  = PrimitiveType_Numeric (List Annotation) NumericType
  | PrimitiveType_Boolean (List Annotation)

primitiveType : Parser PrimitiveType
primitiveType =
  P.succeed
    (\x -> or (PrimitiveType_Numeric x) (always (PrimitiveType_Boolean x)))
  |= list annotation
  |. P.spaces
  |= P.oneOf
     [ P.map Left numericType
     , P.map Right (P.keyword "boolean")
     ]


type NumericType
  = NumericType_IntegralType IntegralType
  | NumericType_FloatingPointType FloatingPointType

numericType : Parser NumericType
numericType =
  P.oneOf
    [ P.succeed NumericType_IntegralType
      |= integralType
    , P.succeed NumericType_FloatingPointType
      |= floatingPointType
    ]


type IntegralType
  = IntegralType_Byte
  | IntegralType_Short
  | IntegralType_Int
  | IntegralType_Long
  | IntegralType_Char

integralType : Parser IntegralType
integralType =
  P.oneOf
    [ P.succeed IntegralType_Byte  |. P.keyword "byte"
    , P.succeed IntegralType_Short |. P.keyword "short"
    , P.succeed IntegralType_Int   |. P.keyword "int"
    , P.succeed IntegralType_Long  |. P.keyword "long"
    , P.succeed IntegralType_Char  |. P.keyword "char"
    ]


type FloatingPointType
  = FloatingPointType_Float
  | FloatingPointType_Double

floatingPointType : Parser FloatingPointType
floatingPointType =
  P.oneOf
    [ P.succeed FloatingPointType_Float  |. P.keyword "float"
    , P.succeed FloatingPointType_Double |. P.keyword "double"
    ]


type ReferenceType
  = ReferenceType_ClassOrInterfaceType ClassOrInterfaceType
  | ReferenceType_TypeVariable TypeVariable
  | ReferenceType_ArrayType ArrayType

referenceType : Parser ReferenceType
referenceType =
  P.oneOf
    [ P.lazy (\_ -> P.succeed ReferenceType_ClassOrInterfaceType
                    |= classOrInterfaceType
             )
    , P.succeed ReferenceType_TypeVariable
      |= typeVariable
    , P.succeed ReferenceType_ArrayType
      |= arrayType
    ]


type ClassOrInterfaceType
  = ClassOrInterfaceType_ClassType ClassType
  | ClassOrInterfaceType_InterfaceType InterfaceType

classOrInterfaceType : Parser ClassOrInterfaceType
classOrInterfaceType =
  P.oneOf
    [ P.succeed ClassOrInterfaceType_ClassType
      |= classType
    , P.succeed ClassOrInterfaceType_InterfaceType
      |= interfaceType
    ]


type ClassType
  = ClassType_NoPackage
      (List Annotation) TypeIdentifier (Maybe TypeArguments)
  | ClassType_Package
      PackageName (List Annotation) TypeIdentifier (Maybe TypeArguments)
  | ClassType_ClassOrInterfaceType
      ClassOrInterfaceType (List Annotation) TypeIdentifier (Maybe TypeArguments)

classType : Parser ClassType
classType =
  P.oneOf
    [ P.succeed ClassType_NoPackage
      |= list annotation
      |. P.spaces
      |= typeIdentifier
      |. P.spaces
      |= optional typeArguments
    , P.succeed ClassType_Package
      |= packageName
      |. P.spaces
      |. P.symbol "."
      |. P.spaces
      |= list annotation
      |. P.spaces
      |= typeIdentifier
      |. P.spaces
      |= optional typeArguments
    , P.succeed ClassType_ClassOrInterfaceType
      |= P.lazy (\_ -> classOrInterfaceType)
      |. P.spaces
      |. P.symbol "."
      |. P.spaces
      |= list annotation
      |. P.spaces
      |= typeIdentifier
      |. P.spaces
      |= optional typeArguments
    ]


type InterfaceType
  = InterfaceType_ClassType ClassType

interfaceType : Parser InterfaceType
interfaceType =
  P.succeed InterfaceType_ClassType
  |= classType


type TypeVariable = TypeVariable (List Annotation) TypeIdentifier

typeVariable : Parser TypeVariable
typeVariable =
  P.succeed TypeVariable
  |= list annotation
  |. P.spaces
  |= typeIdentifier


type ArrayType
  = ArrayType_PrimitiveType PrimitiveType Dims
  | ArrayType_ClassOrInterfaceType ClassOrInterfaceType Dims
  | ArrayType_TypeVariable TypeVariable Dims

arrayType : Parser ArrayType
arrayType =
  P.oneOf
    [ P.succeed ArrayType_PrimitiveType 
      |= primitiveType
      |. P.spaces
      |= dims
    , P.lazy
        (\_ ->
          P.succeed ArrayType_ClassOrInterfaceType
          |= classOrInterfaceType
          |. P.spaces
          |= dims
        )
    , P.succeed ArrayType_TypeVariable
      |= typeVariable
      |. P.spaces
      |= dims
    ]


type Dims = Dims (List (List Annotation))

dims : Parser Dims
dims =
  P.succeed Dims
  |= list
     ( P.succeed identity
       |= list annotation
       |. P.spaces
       |. P.symbol "["
       |. P.spaces
       |. P.symbol "]"
     )


type TypeParameter =
    TypeParameter (List TypeParameterModifier) TypeIdentifier (Maybe TypeBound)

typeParameter : Parser TypeParameter
typeParameter =
    P.succeed TypeParameter
    |= list typeParameterModifier
    |. P.spaces
    |= typeIdentifier
    |. P.spaces
    |= optional typeBound


type TypeParameterModifier
  = TypeParameterModifier Annotation

typeParameterModifier : Parser TypeParameterModifier
typeParameterModifier =
  P.succeed TypeParameterModifier
  |= annotation


type TypeBound
  = TypeBound_TypeVariable TypeVariable
  | TypeBound_ClassOrInterfaceType ClassOrInterfaceType (List AdditionalBound)

typeBound : Parser TypeBound
typeBound =
  P.succeed identity
  |. P.keyword "extends"
  |. P.spaces
  |= P.oneOf
     [ P.succeed TypeBound_TypeVariable
       |= typeVariable
     , P.succeed TypeBound_ClassOrInterfaceType
       |= classOrInterfaceType
       |. P.spaces
       |= list additionalBound
     ]


type AdditionalBound = AdditionalBound InterfaceType

additionalBound : Parser AdditionalBound
additionalBound =
  P.succeed AdditionalBound
    |. P.symbol "&"
    |. P.spaces
    |= interfaceType


type TypeArguments = TypeArguments_Brackets TypeArgumentList

typeArguments : Parser TypeArguments
typeArguments =
  P.succeed TypeArguments_Brackets
  |. P.symbol "<"
  |= typeArgumentList
  |. P.symbol ">"


type TypeArgumentList = TypeArgumentList (List TypeArgument)

typeArgumentList : Parser TypeArgumentList
typeArgumentList =
  P.succeed TypeArgumentList
  |= nonEmptySep "," typeArgument


type TypeArgument
  = TypeArgument_ReferenceType ReferenceType
  | TypeArgument_Wildcard Wildcard

typeArgument : Parser TypeArgument
typeArgument =
  P.oneOf
    [ P.succeed TypeArgument_ReferenceType
      |= referenceType
    , P.succeed TypeArgument_Wildcard
      |= wildcard
    ]


type Wildcard =
    Wildcard (List Annotation) (Maybe WildcardBounds)

wildcard : Parser Wildcard
wildcard =
  P.succeed Wildcard
  |= list annotation
  |. P.spaces
  |. P.symbol "?"
  |= optional wildcardBounds


type WildcardBounds
  = WildcardBounds_Extends ReferenceType
  | WildcardBounds_Super ReferenceType

wildcardBounds : Parser WildcardBounds
wildcardBounds =
  P.oneOf
    [ P.succeed WildcardBounds_Extends
      |. P.keyword "extends"
      |= referenceType
    , P.succeed WildcardBounds_Super
      |. P.keyword "super"
      |= referenceType
    ]


-- Productions from §6 (Names)

type ModuleName = ModuleName (List Identifier)

moduleName : Parser ModuleName
moduleName =
  P.succeed ModuleName
  |= dotted identifier


type PackageName = PackageName (List Identifier)

packageName : Parser PackageName
packageName =
  P.succeed PackageName
  |= dotted identifier


type TypeName = TypeName (List TypeIdentifier)

typeName : Parser TypeName
typeName =
  P.succeed TypeName
  |= dotted typeIdentifier


type ExpressionName
  = ExpressionName_Identifier Identifier
  | ExpressionName_AmbiguousDotIdentifier AmbiguousName Identifier

expresionName : Parser ExpressionName
expresionName =
  P.oneOf
    [ P.succeed ExpressionName_Identifier
      |= identifier
    , P.succeed ExpressionName_AmbiguousDotIdentifier
      |= ambiguousName
      |. P.spaces
      |. P.symbol "."
      |. P.spaces
      |= identifier
    ]


type MethodName = MethodName UnqualifiedMethodIdentifier

methodName : Parser MethodName
methodName =
  P.succeed MethodName
  |= unqualifiedMethodIdentifier


type PackageOrTypeName = PackageOrTypeName (List Identifier)

packageOrTypeName : Parser PackageOrTypeName
packageOrTypeName =
  P.succeed PackageOrTypeName
  |= dotted identifier


type AmbiguousName = AmbiguousName (List Identifier)

ambiguousName : Parser AmbiguousName
ambiguousName =
  P.succeed AmbiguousName
  |= dotted identifier


-- Productions from §7 (Packages and Modules)

type CompilationUnit
  = CompilationUnit_Ordinary OrdinaryCompilationUnit
  | CompilationUnit_Modular ModularCompilationUnit

compilationUnit : Parser CompilationUnit
compilationUnit =
  P.oneOf
    [ P.succeed CompilationUnit_Ordinary
      |= ordinaryCompilationUnit
    , P.succeed CompilationUnit_Modular
      |= modularCompilationUnit
    ]


type OrdinaryCompilationUnit =
    OrdinaryCompilationUnit (Maybe PackageDeclaration)
                            (List ImportDeclaration) (List TypeDeclaration)

ordinaryCompilationUnit : Parser OrdinaryCompilationUnit
ordinaryCompilationUnit =
  P.succeed OrdinaryCompilationUnit
  |= optional packageDeclaration
  |. P.spaces
  |= list importDeclaration
  |. P.spaces
  |= list typeDeclaration


type ModularCompilationUnit =
    ModularCompilationUnit (List ImportDeclaration) ModuleDeclaration

modularCompilationUnit : Parser ModularCompilationUnit
modularCompilationUnit =
  P.succeed ModularCompilationUnit
  |= list importDeclaration
  |. P.spaces
  |= moduleDeclaration


type PackageDeclaration =
    PackageDeclaration (List PackageModifier) (List Identifier)

packageDeclaration : Parser PackageDeclaration
packageDeclaration =
    P.succeed PackageDeclaration
    |= list packageModifier
    |. P.spaces
    |. P.keyword "package"
    |. P.spaces
    |= nonEmptySep "." identifier
    |. P.spaces
    |. P.keyword ";"


type PackageModifier = PackageModifier Annotation

packageModifier : Parser PackageModifier
packageModifier =
    P.succeed PackageModifier
    |= annotation


type ImportDeclaration
  = ImportDeclaration_SingleTypeImport     SingleTypeImportDeclaration
  | ImportDeclaration_TypeImportOnDemand   TypeImportOnDemandDeclaration
  | ImportDeclaration_SingleStaticImport   SingleStaticImportDeclaration
  | ImportDeclaration_StaticImportOnDemand StaticImportOnDemandDeclaration

importDeclaration : Parser ImportDeclaration
importDeclaration =
  P.oneOf
    [ P.succeed ImportDeclaration_SingleTypeImport
      |= singleTypeImportDeclaration
    , P.succeed ImportDeclaration_TypeImportOnDemand
      |= typeImportOnDemandDeclaration
    , P.succeed ImportDeclaration_SingleStaticImport
      |= singleStaticImportDeclaration
    , P.succeed ImportDeclaration_StaticImportOnDemand
      |= staticImportOnDemandDeclaration
    ]


type SingleTypeImportDeclaration = SingleTypeImportDeclaration TypeName

singleTypeImportDeclaration : Parser SingleTypeImportDeclaration
singleTypeImportDeclaration =
    P.succeed SingleTypeImportDeclaration
    |. P.keyword "import"
    |. P.spaces
    |= typeName
    |. P.spaces
    |. P.symbol ";"


type TypeImportOnDemandDeclaration =
    TypeImportOnDemandDeclaration PackageOrTypeName

typeImportOnDemandDeclaration : Parser TypeImportOnDemandDeclaration
typeImportOnDemandDeclaration =
    P.succeed TypeImportOnDemandDeclaration
    |. P.keyword "import"
    |. P.spaces
    |= packageOrTypeName
    |. P.symbol "."
    |. P.spaces
    |. P.symbol "*"
    |. P.spaces
    |. P.symbol ";"


type SingleStaticImportDeclaration =
    SingleStaticImportDeclaration TypeName Identifier

singleStaticImportDeclaration : Parser SingleStaticImportDeclaration
singleStaticImportDeclaration =
    P.succeed SingleStaticImportDeclaration
    |. P.keyword "import"
    |. P.spaces 
    |. P.keyword "static"
    |. P.spaces 
    |= typeName
    |. P.spaces 
    |. P.symbol "."
    |. P.spaces
    |= identifier
    |. P.spaces
    |. P.symbol ";"


type StaticImportOnDemandDeclaration = StaticImportOnDemandDeclaration TypeName

staticImportOnDemandDeclaration : Parser StaticImportOnDemandDeclaration
staticImportOnDemandDeclaration =
    P.succeed StaticImportOnDemandDeclaration
    |. P.keyword "import"
    |. P.spaces
    |. P.keyword "static"
    |. P.spaces
    |= typeName
    |. P.spaces
    |. P.symbol "."
    |. P.spaces
    |. P.symbol "*"
    |. P.spaces
    |. P.symbol ";"

type TypeDeclaration
  = TypeDeclaration_ClassDeclaration ClassDeclaration
  | TypeDeclaration_InterfaceDeclaration InterfaceDeclaration
  | TypeDeclaration_Semi

typeDeclaration : Parser TypeDeclaration
typeDeclaration =
  P.oneOf
    [ P.succeed TypeDeclaration_ClassDeclaration 
      |= classDeclaration
    , P.succeed TypeDeclaration_InterfaceDeclaration
      |= interfaceDeclaration
    , P.succeed TypeDeclaration_Semi
      |. P.symbol ";"
    ]

type ModuleDeclaration =
    ModuleDeclaration (List Annotation) (Maybe ()) (List Identifier)
                                                   (List ModuleDirective)

moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    P.succeed ModuleDeclaration
    |= list annotation
    |. P.spaces
    |= optional (P.keyword "open")
    |. P.spaces
    |. P.keyword "module "
    |. P.spaces
    |= nonEmptySep "." identifier
    |. P.spaces
    |. P.symbol "{"
    |. P.spaces
    |= list moduleDirective
    |. P.spaces
    |. P.symbol "}"


type ModuleDirective
  = ModuleDirective_Requires (List RequiresModifier) ModuleName
  | ModuleDirective_Exports PackageName (Maybe (List ModuleName))
  | ModuleDirective_Opens PackageName (Maybe (List ModuleName))
  | ModuleDirective_Uses TypeName
  | ModuleDirective_Provides TypeName (List TypeName)

moduleDirective : Parser ModuleDirective
moduleDirective =
  P.oneOf
    [ P.succeed ModuleDirective_Requires
      |. P.keyword "requires"
      |. P.spaces
      |= list requiresModifier
      |. P.spaces
      |= moduleName
      |. P.spaces
      |. P.symbol ";"
    , P.succeed ModuleDirective_Exports
      |. P.keyword "exports"
      |. P.spaces
      |= packageName
      |. P.spaces
      |= (optional <|
           P.succeed identity
           |. P.keyword "to"
           |. P.spaces
           |= nonEmptySep "," moduleName
         )
      |. P.spaces
      |. P.symbol ";"
    , P.succeed ModuleDirective_Opens
      |. P.keyword "opens"
      |. P.spaces
      |= packageName
      |. P.spaces
      |= (optional <|
           P.succeed identity
           |. P.keyword "to"
           |. P.spaces
           |= nonEmptySep "," moduleName
         )
      |. P.spaces
      |. P.symbol ";"
    , P.succeed ModuleDirective_Uses
      |. P.keyword "uses"
      |. P.spaces
      |= typeName
      |. P.spaces
      |. P.symbol ";"
    , P.succeed ModuleDirective_Provides
      |. P.keyword "provides"
      |. P.spaces
      |= typeName
      |. P.spaces
      |. P.keyword "with"
      |. P.spaces
      |= nonEmptySep "," typeName
      |. P.spaces
      |. P.symbol ";"
    ]

type RequiresModifier
  = RequiresModifier_Transitive
  | RequiresModifier_Static

requiresModifier : Parser RequiresModifier
requiresModifier =
  P.oneOf
    [ P.succeed RequiresModifier_Transitive |. P.keyword "transitive"
    , P.succeed RequiresModifier_Static     |. P.keyword "static"
    ]

-- Productions from §8 (Classes)

type ClassDeclaration
  = ClassDeclaration_Normal NormalClassDeclaration
  | ClassDeclaration_Enum EnumDeclaration

classDeclaration : Parser ClassDeclaration
classDeclaration =
  P.oneOf
    [ P.succeed ClassDeclaration_Normal
      |= normalClassDeclaration
    , P.succeed ClassDeclaration_Enum
      |= enumDeclaration
    ]


type NormalClassDeclaration =
    NormalClassDeclaration (List ClassModifier) TypeIdentifier
                           (Maybe TypeParameters) (Maybe Superclass)
                           (Maybe Superinterfaces) ClassBody

normalClassDeclaration : Parser NormalClassDeclaration
normalClassDeclaration =
    P.succeed NormalClassDeclaration
    |= list classModifier
    |. P.spaces
    |. P.keyword "class"
    |. P.spaces
    |= typeIdentifier
    |. P.spaces
    |= optional typeParameters
    |. P.spaces
    |= optional superclass
    |. P.spaces
    |= optional superinterfaces
    |. P.spaces
    |= classBody
    |. P.spaces


type ClassModifier
  = ClassModifier_Annotation Annotation
  | ClassModifier_Public
  | ClassModifier_Protected
  | ClassModifier_Private
  | ClassModifier_Abstract
  | ClassModifier_Static
  | ClassModifier_Final
  | ClassModifier_StrictFp

classModifier : Parser ClassModifier
classModifier =
  P.oneOf
    [ P.succeed ClassModifier_Annotation |= annotation
    , P.succeed ClassModifier_Public     |. P.keyword "public"
    , P.succeed ClassModifier_Protected  |. P.keyword "protected"
    , P.succeed ClassModifier_Private    |. P.keyword "private"
    , P.succeed ClassModifier_Abstract   |. P.keyword "abstract"
    , P.succeed ClassModifier_Static     |. P.keyword "static"
    , P.succeed ClassModifier_Final      |. P.keyword "final"
    , P.succeed ClassModifier_StrictFp   |. P.keyword "strictfp"
    ]


type TypeParameters = TypeParameters TypeParameterList

typeParameters : Parser TypeParameters
typeParameters =
    P.succeed TypeParameters
    |. P.symbol "<"
    |. P.spaces
    |= typeParameterList
    |. P.spaces
    |. P.symbol ">"


type TypeParameterList = TypeParameterList (List TypeParameter)

typeParameterList : Parser TypeParameterList
typeParameterList =
    P.succeed TypeParameterList
    |= nonEmptySep "," typeParameter


type Superclass = Superclass ClassType

superclass : Parser Superclass
superclass =
    P.succeed Superclass
    |. P.keyword "extends"
    |. P.spaces
    |= classType


type Superinterfaces = Superinterfaces InterfaceTypeList

superinterfaces : Parser Superinterfaces
superinterfaces =
    P.succeed Superinterfaces
    |. P.keyword "implements"
    |. P.spaces
    |= interfaceTypeList


type InterfaceTypeList = InterfaceTypeList (List InterfaceType)

interfaceTypeList : Parser InterfaceTypeList
interfaceTypeList =
    P.succeed InterfaceTypeList
    |= nonEmptySep "," interfaceType

type ClassBody = ClassBody (List ClassBodyDeclaration)

classBody : Parser ClassBody
classBody =
    P.succeed ClassBody
    |. P.symbol "{"
    |. P.spaces
    |= list classBodyDeclaration
    |. P.spaces
    |. P.symbol "}"

type ClassBodyDeclaration
  = ClassBodyDeclaration_ClassMemberDeclaration ClassMemberDeclaration
  | ClassBodyDeclaration_InstanceInitializer InstanceInitializer
  | ClassBodyDeclaration_StaticInitializer StaticInitializer
  | ClassBodyDeclaration_ConstructorDeclaration ConstructorDeclaration

classBodyDeclaration : Parser ClassBodyDeclaration
classBodyDeclaration =
  P.oneOf
    [ P.succeed ClassBodyDeclaration_ClassMemberDeclaration
      |= classMemberDeclaration
    , P.succeed ClassBodyDeclaration_InstanceInitializer
      |= instanceInitializer
    , P.succeed ClassBodyDeclaration_StaticInitializer
      |= staticInitializer
    , P.succeed ClassBodyDeclaration_ConstructorDeclaration
      |= constructorDeclaration
    ]


type ClassMemberDeclaration
  = ClassMemberDeclaration_Field FieldDeclaration
  | ClassMemberDeclaration_Method MethodDeclaration
  | ClassMemberDeclaration_Class ClassDeclaration
  | ClassMemberDeclaration_Interface InterfaceDeclaration
  | ClassMemberDeclaration_Semi

classMemberDeclaration : Parser ClassMemberDeclaration
classMemberDeclaration =
  P.oneOf
    [ P.succeed ClassMemberDeclaration_Field     |= fieldDeclaration
    , P.succeed ClassMemberDeclaration_Method    |= methodDeclaration
    , P.succeed ClassMemberDeclaration_Class     |= P.lazy 
                                                      (\_ -> classDeclaration)
    , P.succeed ClassMemberDeclaration_Interface |= interfaceDeclaration
    , P.succeed ClassMemberDeclaration_Semi      |. P.symbol ";"
    ]


type FieldDeclaration =
    FieldDeclaration (List FieldModifier) UnannType VariableDeclaratorList

fieldDeclaration : Parser FieldDeclaration
fieldDeclaration =
    P.succeed FieldDeclaration
    |= list fieldModifier
    |. P.spaces
    |= unannType
    |. P.spaces
    |= variableDeclaratorList
    |. P.spaces
    |. P.keyword ";"


type FieldModifier
  = FieldModifier_Annotation Annotation
  | FieldModifier_Public
  | FieldModifier_Protected
  | FieldModifier_Private
  | FieldModifier_Static
  | FieldModifier_Final
  | FieldModifier_Transient
  | FieldModifier_Volatile

fieldModifier : Parser FieldModifier
fieldModifier =
  P.oneOf
    [ P.succeed FieldModifier_Annotation |= annotation
    , P.succeed FieldModifier_Public    |. P.keyword "public"
    , P.succeed FieldModifier_Protected |. P.keyword "protected"
    , P.succeed FieldModifier_Private   |. P.keyword "private"
    , P.succeed FieldModifier_Static    |. P.keyword "static"
    , P.succeed FieldModifier_Final     |. P.keyword "final"
    , P.succeed FieldModifier_Transient |. P.keyword "transient"
    , P.succeed FieldModifier_Volatile  |. P.keyword "volatile"
    ]


type VariableDeclaratorList = VariableDeclaratorList (List VariableDeclarator)

variableDeclaratorList : Parser VariableDeclaratorList
variableDeclaratorList =
    P.succeed VariableDeclaratorList
    |= nonEmptySep "," variableDeclarator


type VariableDeclarator =
    VariableDeclarator VariableDeclaratorId (Maybe VariableInitializer)

variableDeclarator : Parser VariableDeclarator
variableDeclarator =
    P.succeed VariableDeclarator
    |= variableDeclaratorId
    |. P.spaces
    |= optional
       ( P.succeed identity
           |. P.symbol "="
           |. P.spaces
           |= variableInitializer
       )


type VariableDeclaratorId = VariableDeclaratorId Identifier (Maybe Dims)

variableDeclaratorId : Parser VariableDeclaratorId
variableDeclaratorId =
    P.succeed VariableDeclaratorId
    |= identifier
    |. P.spaces
    |= optional dims

type VariableInitializer
  = VariableInitializer_Expression Expression
  | VariableInitializer_ArrayInitializer ArrayInitializer

variableInitializer : Parser VariableInitializer
variableInitializer =
  P.oneOf
    [ P.succeed VariableInitializer_Expression
      |= expresion
    , P.succeed VariableInitializer_ArrayInitializer
      |= arrayInitializer
    ]

type UnannType
  = UnannType_Primitive UnannPrimitiveType
  | UnannType_Reference UnannReferenceType

unannType : Parser UnannType
unannType =
  P.oneOf
    [ P.succeed UnannType_Primitive
      |= unannPrimitiveType
    , P.succeed UnannType_Reference
      |= unannReferenceType
    ]


type UnannPrimitiveType
  = UnannPrimitiveType_Numeric NumericType
  | UnannPrimitiveType_Boolean Bool

unannPrimitiveType : Parser UnannPrimitiveType
unannPrimitiveType =
  P.oneOf
    [ P.succeed UnannPrimitiveType_Numeric
      |= numericType
    , P.succeed UnannPrimitiveType_Boolean
      |= boolean
    ]

type UnannReferenceType
  = UnannClassOrInterfaceType
  | UnannTypeVariable
  | UnannArrayType

unannReferenceType : Parser UnannReferenceType
unannReferenceType =
  P.oneOf
    [ P.succeed UnannClassOrInterfaceType
      |= unannClassOrInterfaceType
    , P.succeed UnannTypeVariable
      |= unannTypeVariable
    , P.succeed UnannArrayType
      |= unannArrayType
    ]

{-
UnannClassOrInterfaceType:
    UnannClassType
    UnannInterfaceType


UnannClassType:
    TypeIdentifier (Maybe TypeArguments)
    PackageName . (List Annotation) TypeIdentifier (Maybe TypeArguments)
    UnannClassOrInterfaceType . (List Annotation) TypeIdentifier (Maybe TypeArguments)


UnannInterfaceType:
    UnannClassType


UnannTypeVariable:
    TypeIdentifier


UnannArrayType:
    UnannPrimitiveType Dims
    UnannClassOrInterfaceType Dims
    UnannTypeVariable Dims


MethodDeclaration:
    (List MethodModifier) MethodHeader MethodBody

MethodModifier:
    (one of)
    Annotation public protected private
    abstract static final synchronized native strictfp

MethodHeader:
    Result MethodDeclarator (Maybe Throws)
    TypeParameters (List Annotation) Result MethodDeclarator (Maybe Throws)

Result:
    UnannType
    void

MethodDeclarator:
    Identifier ( [ReceiverParameter ,] (Maybe FormalParameterList) ) (Maybe Dims)

ReceiverParameter:
    (List Annotation) UnannType [Identifier .] this

FormalParameterList:
    FormalParameter {, FormalParameter}

FormalParameter:
    (List VariableModifier) UnannType VariableDeclaratorId
    VariableArityParameter

VariableArityParameter:
    (List VariableModifier) UnannType (List Annotation) ... Identifier

VariableModifier:
    Annotation
    final

Throws:
    throws ExceptionTypeList

ExceptionTypeList:
    ExceptionType {, ExceptionType}

!!
ExceptionType:
    ClassType
    TypeVariable

!!
MethodBody:
    Block
    ;

!!
InstanceInitializer:
    Block

StaticInitializer:
    static Block

ConstructorDeclaration:
    (List ConstructorModifier) ConstructorDeclarator (Maybe Throws) ConstructorBody

ConstructorModifier:
    (one of)
    Annotation public protected private

ConstructorDeclarator:
    (Maybe TypeParameters) SimpleTypeName ( [ReceiverParameter ,] (Maybe FormalParameterList) )

!!
SimpleTypeName:
    TypeIdentifier

ConstructorBody:
    { (Maybe ExplicitConstructorInvocation) (Maybe BlockStatements) }

ExplicitConstructorInvocation:
    (Maybe TypeArguments) this ( (Maybe ArgumentList) ) ;
    (Maybe TypeArguments) super ( (Maybe ArgumentList) ) ;
    ExpressionName . (Maybe TypeArguments) super ( (Maybe ArgumentList) ) ;
    Primary . (Maybe TypeArguments) super ( (Maybe ArgumentList) ) ;

EnumDeclaration:
    (List ClassModifier) enum TypeIdentifier (Maybe Superinterfaces) EnumBody

EnumBody:
    { (Maybe EnumConstantList) [,] (Maybe EnumBodyDeclarations) }

EnumConstantList:
    EnumConstant {, EnumConstant}

EnumConstant:
    (List EnumConstantModifier) Identifier [( (Maybe ArgumentList) )] (Maybe ClassBody)

!!
EnumConstantModifier:
    Annotation

EnumBodyDeclarations:
    ; (List ClassBodyDeclaration)

-- Productions from §9 (Interfaces)

InterfaceDeclaration:
    NormalInterfaceDeclaration
    AnnotationTypeDeclaration

NormalInterfaceDeclaration:
    (List InterfaceModifier) interface TypeIdentifier (Maybe TypeParameters) (Maybe ExtendsInterfaces) InterfaceBody

InterfaceModifier:
    (one of)
    Annotation public protected private
    abstract static strictfp

ExtendsInterfaces:
    extends InterfaceTypeList

InterfaceBody:
    { (List InterfaceMemberDeclaration) }

!!
InterfaceMemberDeclaration:
    ConstantDeclaration
    InterfaceMethodDeclaration
    ClassDeclaration
    InterfaceDeclaration
    ;

ConstantDeclaration:
    (List ConstantModifier) UnannType VariableDeclaratorList ;

ConstantModifier:
    (one of)
    Annotation public
    static final

InterfaceMethodDeclaration:
    (List InterfaceMethodModifier) MethodHeader MethodBody

InterfaceMethodModifier:
    (one of)
    Annotation public private
    abstract default static strictfp

AnnotationTypeDeclaration:
    (List InterfaceModifier) @ interface TypeIdentifier AnnotationTypeBody

AnnotationTypeBody:
    { (List AnnotationTypeMemberDeclaration) }

!!
AnnotationTypeMemberDeclaration:
    AnnotationTypeElementDeclaration
    ConstantDeclaration
    ClassDeclaration
    InterfaceDeclaration
    ;

AnnotationTypeElementDeclaration:
    (List AnnotationTypeElementModifier) UnannType Identifier ( ) (Maybe Dims) (Maybe DefaultValue) ;

AnnotationTypeElementModifier:
    (one of)
    Annotation public
    abstract

DefaultValue:
    default ElementValue

!!
type Annotation 
    NormalAnnotation
    MarkerAnnotation
    SingleElementAnnotation

NormalAnnotation:
    @ TypeName ( (Maybe ElementValuePairList) )

ElementValuePairList:
    ElementValuePair {, ElementValuePair}

ElementValuePair:
    Identifier = ElementValue

!!
ElementValue:
    ConditionalExpression
    ElementValueArrayInitializer
    Annotation

ElementValueArrayInitializer:
    { (Maybe ElementValueList) [,] }

ElementValueList:
    ElementValue {, ElementValue}

MarkerAnnotation:
    @ TypeName

SingleElementAnnotation:
    @ TypeName ( ElementValue )

-- Productions from §10 (Arrays)

ArrayInitializer:
    { (Maybe VariableInitializerList) [,] }

VariableInitializerList:
    VariableInitializer {, VariableInitializer}

-- Productions from §14 (Blocks and Statements)

Block:
    { (Maybe BlockStatements) }

BlockStatements:
    BlockStatement (List BlockStatement)

BlockStatement:
    LocalVariableDeclarationStatement
    ClassDeclaration
    Statement

LocalVariableDeclarationStatement:
    LocalVariableDeclaration ;

LocalVariableDeclaration:
    (List VariableModifier) LocalVariableType VariableDeclaratorList

LocalVariableType:
    UnannType
    var

!!
Statement:
    StatementWithoutTrailingSubstatement
    LabeledStatement
    IfThenStatement
    IfThenElseStatement
    WhileStatement
    ForStatement

!!
StatementNoShortIf:
    StatementWithoutTrailingSubstatement
    LabeledStatementNoShortIf
    IfThenElseStatementNoShortIf
    WhileStatementNoShortIf
    ForStatementNoShortIf

!!
StatementWithoutTrailingSubstatement:
    Block
    EmptyStatement
    ExpressionStatement
    AssertStatement
    SwitchStatement
    DoStatement
    BreakStatement
    ContinueStatement
    ReturnStatement
    SynchronizedStatement
    ThrowStatement
    TryStatement
    YieldStatement

!!
EmptyStatement:
    ;

LabeledStatement:
    Identifier : Statement

LabeledStatementNoShortIf:
    Identifier : StatementNoShortIf

ExpressionStatement:
    StatementExpression ;

!!
StatementExpression:
    Assignment
    PreIncrementExpression
    PreDecrementExpression
    PostIncrementExpression
    PostDecrementExpression
    MethodInvocation
    ClassInstanceCreationExpression

IfThenStatement:
    if ( Expression ) Statement

IfThenElseStatement:
    if ( Expression ) StatementNoShortIf else Statement

IfThenElseStatementNoShortIf:
    if ( Expression ) StatementNoShortIf else StatementNoShortIf

AssertStatement:
    assert Expression ;
    assert Expression : Expression ;

SwitchStatement:
    switch ( Expression ) SwitchBlock

SwitchBlock:
    { SwitchRule (List SwitchRule) }
    { (List SwitchBlockStatementGroup) {SwitchLabel :} }

SwitchRule:
    SwitchLabel -> Expression ;
    SwitchLabel -> Block
    SwitchLabel -> ThrowStatement

SwitchBlockStatementGroup:
    SwitchLabel : {SwitchLabel :} BlockStatements

SwitchLabel:
    case CaseConstant {, CaseConstant}
    default

CaseConstant:
    ConditionalExpression

WhileStatement:
    while ( Expression ) Statement

WhileStatementNoShortIf:
    while ( Expression ) StatementNoShortIf

DoStatement:
    do Statement while ( Expression ) ;

!!
ForStatement:
    BasicForStatement
    EnhancedForStatement

!!
ForStatementNoShortIf:
    BasicForStatementNoShortIf
    EnhancedForStatementNoShortIf

BasicForStatement:
    for ( (Maybe ForInit) ; (Maybe Expression) ; (Maybe ForUpdate) ) Statement

BasicForStatementNoShortIf:
    for ( (Maybe ForInit) ; (Maybe Expression) ; (Maybe ForUpdate) ) StatementNoShortIf

!!
ForInit:
    StatementExpressionList
    LocalVariableDeclaration

!!
ForUpdate:
    StatementExpressionList

StatementExpressionList:
    StatementExpression {, StatementExpression}

EnhancedForStatement:
    for ( (List VariableModifier) LocalVariableType VariableDeclaratorId : Expression ) Statement

EnhancedForStatementNoShortIf:
    for ( (List VariableModifier) LocalVariableType VariableDeclaratorId : Expression ) StatementNoShortIf
BreakStatement:
    break (Maybe Identifier) ;

YieldStatement:
    yield Expression ;

ContinueStatement:
    continue (Maybe Identifier) ;

ReturnStatement:
    return (Maybe Expression) ;

ThrowStatement:
    throw Expression ;

SynchronizedStatement:
    synchronized ( Expression ) Block

TryStatement:
    try Block Catches
    try Block (Maybe Catches) Finally
    TryWithResourcesStatement

Catches:
    CatchClause (List CatchClause)

CatchClause:
    catch ( CatchFormalParameter ) Block

CatchFormalParameter:
    (List VariableModifier) CatchType VariableDeclaratorId

CatchType:
    UnannClassType {| ClassType}

Finally:
    finally Block

TryWithResourcesStatement:
    try ResourceSpecification Block (Maybe Catches) (Maybe Finally)

ResourceSpecification:
    ( ResourceList [;] )

ResourceList:
    Resource {; Resource}

Resource:
    (List VariableModifier) LocalVariableType Identifier = Expression
    VariableAccess

-- Productions from §15 (Expressions)

!!
Primary:
    PrimaryNoNewArray
    ArrayCreationExpression

PrimaryNoNewArray:
    Literal
    ClassLiteral
    this
    TypeName . this
    ( Expression )
    ClassInstanceCreationExpression
    FieldAccess
    ArrayAccess
    MethodInvocation
    MethodReference

ClassLiteral:
    TypeName {[ ]} . class
    NumericType {[ ]} . class
    boolean {[ ]} . class
    void . class
    ClassInstanceCreationExpression:
    UnqualifiedClassInstanceCreationExpression
    ExpressionName . UnqualifiedClassInstanceCreationExpression
    Primary . UnqualifiedClassInstanceCreationExpression
    UnqualifiedClassInstanceCreationExpression:
    new (Maybe TypeArguments) ClassOrInterfaceTypeToInstantiate ( (Maybe ArgumentList) ) (Maybe ClassBody)
    ClassOrInterfaceTypeToInstantiate:
    (List Annotation) Identifier {. (List Annotation) Identifier} (Maybe TypeArgumentsOrDiamond)
    TypeArgumentsOrDiamond:
    TypeArguments
    <>

FieldAccess:
    Primary . Identifier
    super . Identifier
    TypeName . super . Identifier
    ArrayAccess:
    ExpressionName [ Expression ]
    PrimaryNoNewArray [ Expression ]

MethodInvocation:
    MethodName ( (Maybe ArgumentList) )
    TypeName . (Maybe TypeArguments) Identifier ( (Maybe ArgumentList) )
    ExpressionName . (Maybe TypeArguments) Identifier ( (Maybe ArgumentList) )
    Primary . (Maybe TypeArguments) Identifier ( (Maybe ArgumentList) )
    super . (Maybe TypeArguments) Identifier ( (Maybe ArgumentList) )
    TypeName . super . (Maybe TypeArguments) Identifier ( (Maybe ArgumentList) )

ArgumentList:
    Expression {, Expression}

MethodReference:
    ExpressionName :: (Maybe TypeArguments) Identifier
    Primary :: (Maybe TypeArguments) Identifier
    ReferenceType :: (Maybe TypeArguments) Identifier
    super :: (Maybe TypeArguments) Identifier
    TypeName . super :: (Maybe TypeArguments) Identifier
    ClassType :: (Maybe TypeArguments) new
    ArrayType :: new

ArrayCreationExpression:
    new PrimitiveType DimExprs (Maybe Dims)
    new ClassOrInterfaceType DimExprs (Maybe Dims)
    new PrimitiveType Dims ArrayInitializer
    new ClassOrInterfaceType Dims ArrayInitializer

DimExprs:
    DimExpr (List DimExpr)

DimExpr:
    (List Annotation) [ Expression ]

!!
Expression:
    LambdaExpression
    AssignmentExpression

LambdaExpression:
    LambdaParameters -> LambdaBody

LambdaParameters:
    ( (Maybe LambdaParameterList) )
    Identifier

LambdaParameterList:
    LambdaParameter {, LambdaParameter}
    Identifier {, Identifier}

LambdaParameter:
    (List VariableModifier) LambdaParameterType VariableDeclaratorId
    VariableArityParameter

LambdaParameterType:
    UnannType
    var

!!
LambdaBody:
    Expression
    Block

!!
AssignmentExpression:
    ConditionalExpression
    Assignment

Assignment:
    LeftHandSide AssignmentOperator Expression

!!
LeftHandSide:
    ExpressionName
    FieldAccess
    ArrayAccess

AssignmentOperator:
(one of)

    =  *=  /=  %=  +=  -=  <<=  >>=  >>>=  &=  ^=  |=


ConditionalExpression:
    ConditionalOrExpression
    ConditionalOrExpression ? Expression : ConditionalExpression
    ConditionalOrExpression ? Expression : LambdaExpression

ConditionalOrExpression:
    ConditionalAndExpression
    ConditionalOrExpression || ConditionalAndExpression

ConditionalAndExpression:
    InclusiveOrExpression
    ConditionalAndExpression && InclusiveOrExpression

InclusiveOrExpression:
    ExclusiveOrExpression
    InclusiveOrExpression | ExclusiveOrExpression

ExclusiveOrExpression:
    AndExpression
    ExclusiveOrExpression ^ AndExpression

AndExpression:
    EqualityExpression
    AndExpression & EqualityExpression

EqualityExpression:
    RelationalExpression
    EqualityExpression == RelationalExpression
    EqualityExpression != RelationalExpression

RelationalExpression:
    ShiftExpression
    RelationalExpression < ShiftExpression
    RelationalExpression > ShiftExpression
    RelationalExpression <= ShiftExpression
    RelationalExpression >= ShiftExpression
    RelationalExpression instanceof ReferenceType

ShiftExpression:
    AdditiveExpression
    ShiftExpression << AdditiveExpression
    ShiftExpression >> AdditiveExpression
    ShiftExpression >>> AdditiveExpression

AdditiveExpression:
    MultiplicativeExpression
    AdditiveExpression + MultiplicativeExpression
    AdditiveExpression - MultiplicativeExpression

MultiplicativeExpression:
    UnaryExpression
    MultiplicativeExpression * UnaryExpression
    MultiplicativeExpression / UnaryExpression
    MultiplicativeExpression % UnaryExpression

UnaryExpression:
    PreIncrementExpression
    PreDecrementExpression
    + UnaryExpression
    - UnaryExpression
    UnaryExpressionNotPlusMinus

PreIncrementExpression:
    ++ UnaryExpression

PreDecrementExpression:
    -- UnaryExpression

UnaryExpressionNotPlusMinus:
    PostfixExpression
    ~ UnaryExpression
    ! UnaryExpression
    CastExpression
    SwitchExpression

!!
PostfixExpression:
    Primary
    ExpressionName
    PostIncrementExpression
    PostDecrementExpression

PostIncrementExpression:
    PostfixExpression ++

PostDecrementExpression:
    PostfixExpression --

CastExpression:
    ( PrimitiveType ) UnaryExpression
    ( ReferenceType (List AdditionalBound) ) UnaryExpressionNotPlusMinus
    ( ReferenceType (List AdditionalBound) ) LambdaExpression

SwitchExpression:
    switch ( Expression ) SwitchBlock

!!
ConstantExpression:
    Expression
-}
