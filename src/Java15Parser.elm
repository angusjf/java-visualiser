module Java15Parser exposing (..)

import Char exposing (Char)
import Parser as P exposing ((|.), (|=), Parser)
import Regex
import Set exposing (Set)



-- {{{ helpers


parse : Parser a -> String -> Result (List P.DeadEnd) a
parse parser =
    P.run parser << removeCommentsAndTabs


removeCommentsAndTabs : String -> String
removeCommentsAndTabs src =
    let
        re =
            "(\\/\\*(.|\\n)*?\\*\\/|\\/\\/.*$|\\t)"
                |> Regex.fromStringWith
                    { caseInsensitive = False, multiline = True }
                |> Maybe.withDefault Regex.never
    in
    Regex.replace re (always "") src


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
        |= list
            (P.succeed identity
                |. P.symbol sep
                |. P.spaces
                |= p
            )


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
dotted =
    nonEmptySep "."


brackets : Parser Int
brackets =
    P.map List.length (list (P.symbol "[" |. P.spaces |. P.symbol "]"))



--}}}
-- {{{ Productions from §3 (Lexical Structure)


keywords : Set String
keywords =
    Set.fromList
        [ "abstract"
        , "continue"
        , "for"
        , "new"
        , "switch"
        , "assert"
        , "default"
        , "if"
        , "package"
        , "synchronized"
        , "boolean"
        , "do"
        , "goto"
        , "private"
        , "this"
        , "break"
        , "double"
        , "implements"
        , "protected"
        , "throw"
        , "byte"
        , "else"
        , "import"
        , "public"
        , "throws"
        , "case"
        , "enum"
        , "instanceof"
        , "return"
        , "transient"
        , "catch"
        , "extends"
        , "int"
        , "short"
        , "try"
        , "char"
        , "final"
        , "interface"
        , "static"
        , "void"
        , "class"
        , "finally"
        , "long"
        , "strictfp"
        , "volatile"
        , "const"
        , "float"
        , "native"
        , "super"
        , "while"
        , "_"
        ]


type Identifier
    = Identifier String


identifier : Parser Identifier
identifier =
    P.map Identifier <|
        P.variable
            { start = javaLetter
            , inner = javaLetterOrDigit
            , reserved = Set.union keywords (Set.fromList [ "true", "false", "null" ])
            }


javaLetter : Char -> Bool
javaLetter c =
    Char.isAlpha c || c == '_' || c == '$'


javaLetterOrDigit : Char -> Bool
javaLetterOrDigit c =
    javaLetter c || Char.isDigit c


type TypeIdentifier
    = TypeIdentifier Identifier


typeIdentifier : Parser TypeIdentifier
typeIdentifier =
    P.andThen
        (\(Identifier str) ->
            if str == "var" || str == "yield" then
                P.problem <| str ++ " is not a valid type identifier"

            else
                P.succeed <| TypeIdentifier (Identifier str)
        )
        identifier


type UnqualifiedMethodIdentifier
    = UnqualifiedMethodIdentifier Identifier


unqualifiedMethodIdentifier : Parser UnqualifiedMethodIdentifier
unqualifiedMethodIdentifier =
    P.andThen
        (\(Identifier str) ->
            if str == "yield" then
                P.problem "yield is not a valid type identifier"

            else
                P.succeed <| UnqualifiedMethodIdentifier (Identifier str)
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
        [ P.succeed Literal_NullLiteral
            |. nullLiteral
        , P.succeed Literal_BooleanLiteral
            |= booleanLiteral
        , P.succeed Literal_CharacterLiteral
            |= characterLiteral
        , P.succeed Literal_TextBlock
            |= textBlock
        , P.succeed Literal_StringLiteral
            |= stringLiteral
        , P.succeed Literal_IntegerLiteral
            |= integerLiteral
        , P.succeed Literal_FloatingPointLiteral
            |= floatingPointLiteral
        ]


nullLiteral : Parser ()
nullLiteral =
    P.keyword "null"


booleanLiteral : Parser Bool
booleanLiteral =
    P.oneOf
        [ P.succeed True
            |. P.keyword "true"
        , P.succeed False
            |. P.keyword "false"
        ]


characterLiteral : Parser Char
characterLiteral =
    P.succeed 'c'
        -- TODO
        |. P.symbol "'"


textBlock : Parser String
textBlock =
    P.succeed "TODO"
        -- TODO
        |. P.symbol "\""


stringLiteral : Parser String
stringLiteral =
    P.succeed "TODO"
        -- TODO
        |. P.symbol "\""


integerLiteral : Parser Int
integerLiteral =
    P.succeed 1
        -- TODO
        |. P.symbol "1"


floatingPointLiteral : Parser Float
floatingPointLiteral =
    P.succeed 1
        -- TODO
        |. P.symbol "1"



-- }}}
-- {{{ Productions from §4 (Types, Values, and Variables)


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
    P.succeed (\annotations f -> f annotations)
        |= list annotation
        |. P.spaces
        |= P.oneOf
            [ P.succeed (\num -> \ann -> PrimitiveType_Numeric ann num)
                |= numericType
            , P.succeed PrimitiveType_Boolean
                |. P.keyword "boolean"
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
        [ P.succeed IntegralType_Byte |. P.keyword "byte"
        , P.succeed IntegralType_Short |. P.keyword "short"
        , P.succeed IntegralType_Int |. P.keyword "int"
        , P.succeed IntegralType_Long |. P.keyword "long"
        , P.succeed IntegralType_Char |. P.keyword "char"
        ]


type FloatingPointType
    = FloatingPointType_Float
    | FloatingPointType_Double


floatingPointType : Parser FloatingPointType
floatingPointType =
    P.oneOf
        [ P.succeed FloatingPointType_Float |. P.keyword "float"
        , P.succeed FloatingPointType_Double |. P.keyword "double"
        ]


type ReferenceType
    = ReferenceType_ClassOrInterfaceType ClassOrInterfaceType
    | ReferenceType_TypeVariable TypeVariable
    | ReferenceType_ArrayType ArrayType


referenceType : Parser ReferenceType
referenceType =
    P.oneOf
        [ P.succeed ReferenceType_ClassOrInterfaceType
            |= P.lazy (\_ -> classOrInterfaceType)
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
    = ClassType_NoPackage (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | ClassType_Package PackageName (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | ClassType_ClassOrInterfaceType ClassOrInterfaceType (List Annotation) TypeIdentifier (Maybe TypeArguments)


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


type TypeVariable
    = TypeVariable (List Annotation) TypeIdentifier


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


type Dims
    = Dims (List (List Annotation))


dims : Parser Dims
dims =
    P.succeed Dims
        |= list
            (P.succeed identity
                |= list annotation
                |. P.spaces
                |. P.symbol "["
                |. P.spaces
                |. P.symbol "]"
            )


type TypeParameter
    = TypeParameter (List TypeParameterModifier) TypeIdentifier (Maybe TypeBound)


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


type AdditionalBound
    = AdditionalBound InterfaceType


additionalBound : Parser AdditionalBound
additionalBound =
    P.succeed AdditionalBound
        |. P.symbol "&"
        |. P.spaces
        |= interfaceType


type TypeArguments
    = TypeArguments_Brackets TypeArgumentList


typeArguments : Parser TypeArguments
typeArguments =
    P.succeed TypeArguments_Brackets
        |. P.symbol "<"
        |= typeArgumentList
        |. P.symbol ">"


type TypeArgumentList
    = TypeArgumentList (List TypeArgument)


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


type Wildcard
    = Wildcard (List Annotation) (Maybe WildcardBounds)


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



-- }}}
-- {{{ Productions from §6 (Names)


type ModuleName
    = ModuleName (List Identifier)


moduleName : Parser ModuleName
moduleName =
    P.succeed ModuleName
        |= dotted identifier


type PackageName
    = PackageName (List Identifier)


packageName : Parser PackageName
packageName =
    P.succeed PackageName
        |= dotted identifier


type TypeName
    = TypeName (List TypeIdentifier)


typeName : Parser TypeName
typeName =
    P.succeed TypeName
        |= dotted typeIdentifier


type ExpressionName
    = ExpressionName_Identifier Identifier
    | ExpressionName_AmbiguousDotIdentifier AmbiguousName Identifier


expressionName : Parser ExpressionName
expressionName =
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


type MethodName
    = MethodName UnqualifiedMethodIdentifier


methodName : Parser MethodName
methodName =
    P.succeed MethodName
        |= unqualifiedMethodIdentifier


type PackageOrTypeName
    = PackageOrTypeName (List Identifier)


packageOrTypeName : Parser PackageOrTypeName
packageOrTypeName =
    P.succeed PackageOrTypeName
        |= dotted identifier


type AmbiguousName
    = AmbiguousName (List Identifier)


ambiguousName : Parser AmbiguousName
ambiguousName =
    P.succeed AmbiguousName
        |= dotted identifier



-- }}}
-- {{{ Productions from §7 (Packages and Modules)


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


type OrdinaryCompilationUnit
    = OrdinaryCompilationUnit (Maybe PackageDeclaration) (List ImportDeclaration) (List TypeDeclaration)


ordinaryCompilationUnit : Parser OrdinaryCompilationUnit
ordinaryCompilationUnit =
    P.succeed OrdinaryCompilationUnit
        |= optional packageDeclaration
        |. P.spaces
        |= list importDeclaration
        |. P.spaces
        |= list typeDeclaration


type ModularCompilationUnit
    = ModularCompilationUnit (List ImportDeclaration) ModuleDeclaration


modularCompilationUnit : Parser ModularCompilationUnit
modularCompilationUnit =
    P.succeed ModularCompilationUnit
        |= list importDeclaration
        |. P.spaces
        |= moduleDeclaration


type PackageDeclaration
    = PackageDeclaration (List PackageModifier) (List Identifier)


packageDeclaration : Parser PackageDeclaration
packageDeclaration =
    P.succeed PackageDeclaration
        |= list packageModifier
        |. P.spaces
        |. P.keyword "package"
        |. P.spaces
        |= nonEmptySep "." identifier
        |. P.spaces
        |. P.symbol ";"


type PackageModifier
    = PackageModifier Annotation


packageModifier : Parser PackageModifier
packageModifier =
    P.succeed PackageModifier
        |= annotation


type ImportDeclaration
    = ImportDeclaration_SingleTypeImport SingleTypeImportDeclaration
    | ImportDeclaration_TypeImportOnDemand TypeImportOnDemandDeclaration
    | ImportDeclaration_SingleStaticImport SingleStaticImportDeclaration
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


type SingleTypeImportDeclaration
    = SingleTypeImportDeclaration TypeName


singleTypeImportDeclaration : Parser SingleTypeImportDeclaration
singleTypeImportDeclaration =
    P.succeed SingleTypeImportDeclaration
        |. P.keyword "import"
        |. P.spaces
        |= typeName
        |. P.spaces
        |. P.symbol ";"


type TypeImportOnDemandDeclaration
    = TypeImportOnDemandDeclaration PackageOrTypeName


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


type SingleStaticImportDeclaration
    = SingleStaticImportDeclaration TypeName Identifier


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


type StaticImportOnDemandDeclaration
    = StaticImportOnDemandDeclaration TypeName


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


type ModuleDeclaration
    = ModuleDeclaration (List Annotation) (Maybe ()) (List Identifier) (List ModuleDirective)


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
        , P.succeed RequiresModifier_Static |. P.keyword "static"
        ]



-- }}}
-- {{{ Productions from §8 (Classes)


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


type NormalClassDeclaration
    = NormalClassDeclaration (List ClassModifier) TypeIdentifier (Maybe TypeParameters) (Maybe Superclass) (Maybe Superinterfaces) ClassBody


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
        , P.succeed ClassModifier_Public |. P.keyword "public"
        , P.succeed ClassModifier_Protected |. P.keyword "protected"
        , P.succeed ClassModifier_Private |. P.keyword "private"
        , P.succeed ClassModifier_Abstract |. P.keyword "abstract"
        , P.succeed ClassModifier_Static |. P.keyword "static"
        , P.succeed ClassModifier_Final |. P.keyword "final"
        , P.succeed ClassModifier_StrictFp |. P.keyword "strictfp"
        ]


type TypeParameters
    = TypeParameters TypeParameterList


typeParameters : Parser TypeParameters
typeParameters =
    P.succeed TypeParameters
        |. P.symbol "<"
        |. P.spaces
        |= typeParameterList
        |. P.spaces
        |. P.symbol ">"


type TypeParameterList
    = TypeParameterList (List TypeParameter)


typeParameterList : Parser TypeParameterList
typeParameterList =
    P.succeed TypeParameterList
        |= nonEmptySep "," typeParameter


type Superclass
    = Superclass ClassType


superclass : Parser Superclass
superclass =
    P.succeed Superclass
        |. P.keyword "extends"
        |. P.spaces
        |= classType


type Superinterfaces
    = Superinterfaces InterfaceTypeList


superinterfaces : Parser Superinterfaces
superinterfaces =
    P.succeed Superinterfaces
        |. P.keyword "implements"
        |. P.spaces
        |= interfaceTypeList


type InterfaceTypeList
    = InterfaceTypeList (List InterfaceType)


interfaceTypeList : Parser InterfaceTypeList
interfaceTypeList =
    P.succeed InterfaceTypeList
        |= nonEmptySep "," interfaceType


type ClassBody
    = ClassBody (List ClassBodyDeclaration)


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
        [ P.succeed ClassMemberDeclaration_Field |= fieldDeclaration
        , P.succeed ClassMemberDeclaration_Method |= methodDeclaration
        , P.succeed ClassMemberDeclaration_Class
            |= P.lazy
                (\_ -> classDeclaration)
        , P.succeed ClassMemberDeclaration_Interface |= interfaceDeclaration
        , P.succeed ClassMemberDeclaration_Semi |. P.symbol ";"
        ]


type FieldDeclaration
    = FieldDeclaration (List FieldModifier) UnannType VariableDeclaratorList


fieldDeclaration : Parser FieldDeclaration
fieldDeclaration =
    P.succeed FieldDeclaration
        |= list fieldModifier
        |. P.spaces
        |= unannType
        |. P.spaces
        |= variableDeclaratorList
        |. P.spaces
        |. P.symbol ";"


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
        , P.succeed FieldModifier_Public |. P.keyword "public"
        , P.succeed FieldModifier_Protected |. P.keyword "protected"
        , P.succeed FieldModifier_Private |. P.keyword "private"
        , P.succeed FieldModifier_Static |. P.keyword "static"
        , P.succeed FieldModifier_Final |. P.keyword "final"
        , P.succeed FieldModifier_Transient |. P.keyword "transient"
        , P.succeed FieldModifier_Volatile |. P.keyword "volatile"
        ]


type VariableDeclaratorList
    = VariableDeclaratorList (List VariableDeclarator)


variableDeclaratorList : Parser VariableDeclaratorList
variableDeclaratorList =
    P.succeed VariableDeclaratorList
        |= nonEmptySep "," variableDeclarator


type VariableDeclarator
    = VariableDeclarator VariableDeclaratorId (Maybe VariableInitializer)


variableDeclarator : Parser VariableDeclarator
variableDeclarator =
    P.succeed VariableDeclarator
        |= variableDeclaratorId
        |. P.spaces
        |= optional
            (P.succeed identity
                |. P.symbol "="
                |. P.spaces
                |= variableInitializer
            )


type VariableDeclaratorId
    = VariableDeclaratorId Identifier (Maybe Dims)


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
            |= expression
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
    | UnannPrimitiveType_Boolean


unannPrimitiveType : Parser UnannPrimitiveType
unannPrimitiveType =
    P.oneOf
        [ P.succeed UnannPrimitiveType_Numeric
            |= numericType
        , P.succeed UnannPrimitiveType_Boolean
            |. P.keyword "boolean"
        ]


type UnannReferenceType
    = UnannReferenceType_Class UnannClassOrInterfaceType
    | UnannReferenceType_TypeVariable UnannTypeVariable
    | UnannReferenceType_Array UnannArrayType


unannReferenceType : Parser UnannReferenceType
unannReferenceType =
    P.oneOf
        [ P.succeed UnannReferenceType_Class
            |= unannClassOrInterfaceType
        , P.succeed UnannReferenceType_TypeVariable
            |= unannTypeVariable
        , P.succeed UnannReferenceType_Array
            |= unannArrayType
        ]


type UnannClassOrInterfaceType
    = UnannClassOrInterfaceType_Class UnannClassType
    | UnannClassOrInterfaceType_Interface UnannInterfaceType


unannClassOrInterfaceType : Parser UnannClassOrInterfaceType
unannClassOrInterfaceType =
    P.oneOf
        [ P.succeed UnannClassOrInterfaceType_Class
            |= unannClassType
        , P.succeed UnannClassOrInterfaceType_Interface
            |= unannInterfaceType
        ]


type UnannClassType
    = UnannClassType_TypeIdentifer TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Package PackageName (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Class UnannClassOrInterfaceType (List Annotation) TypeIdentifier (Maybe TypeArguments)


unannClassType : Parser UnannClassType
unannClassType =
    P.oneOf
        [ P.succeed UnannClassType_TypeIdentifer
            |= typeIdentifier
            |. P.spaces
            |= optional typeArguments
        , P.succeed UnannClassType_Package
            |= packageName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= list annotation
            |. P.spaces
            |= typeIdentifier
            |. P.spaces
            |= optional typeArguments
        , P.succeed UnannClassType_Class
            |= P.lazy (\_ -> unannClassOrInterfaceType)
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= list annotation
            |. P.spaces
            |= typeIdentifier
            |. P.spaces
            |= optional typeArguments
        ]


type UnannInterfaceType
    = UnannInterfaceType UnannClassType


unannInterfaceType : Parser UnannInterfaceType
unannInterfaceType =
    P.succeed UnannInterfaceType
        |= unannClassType


type UnannTypeVariable
    = UnannTypeVariable TypeIdentifier


unannTypeVariable : Parser UnannTypeVariable
unannTypeVariable =
    P.succeed UnannTypeVariable
        |= typeIdentifier


type UnannArrayType
    = UnannArrayType_Primitive UnannPrimitiveType Dims
    | UnannArrayType_Class UnannClassOrInterfaceType Dims
    | UnannArrayType_TypeVariable UnannTypeVariable Dims


unannArrayType : Parser UnannArrayType
unannArrayType =
    P.oneOf
        [ P.succeed UnannArrayType_Primitive
            |= unannPrimitiveType
            |. P.spaces
            |= dims
        , P.succeed UnannArrayType_Class
            |= unannClassOrInterfaceType
            |. P.spaces
            |= dims
        , P.succeed UnannArrayType_TypeVariable
            |= unannTypeVariable
            |. P.spaces
            |= dims
        ]


type MethodDeclaration
    = MethodDeclaration (List MethodModifier) MethodHeader MethodBody


methodDeclaration : Parser MethodDeclaration
methodDeclaration =
    P.succeed MethodDeclaration
        |= list methodModifier
        |. P.spaces
        |= methodHeader
        |. P.spaces
        |= methodBody


type MethodModifier
    = MethodModifier_Annotation Annotation
    | MethodModifier_Public
    | MethodModifier_Protected
    | MethodModifier_Private
    | MethodModifier_Abstract
    | MethodModifier_Static
    | MethodModifier_Final
    | MethodModifier_Synchronized
    | MethodModifier_Native
    | MethodModifier_Strictfp


methodModifier : Parser MethodModifier
methodModifier =
    P.oneOf
        [ P.succeed MethodModifier_Annotation |= annotation
        , P.succeed MethodModifier_Public |. P.keyword "public"
        , P.succeed MethodModifier_Protected |. P.keyword "protected"
        , P.succeed MethodModifier_Private |. P.keyword "private"
        , P.succeed MethodModifier_Abstract |. P.keyword "abstract"
        , P.succeed MethodModifier_Static |. P.keyword "static"
        , P.succeed MethodModifier_Final |. P.keyword "final"
        , P.succeed MethodModifier_Synchronized |. P.keyword "synchronized"
        , P.succeed MethodModifier_Native |. P.keyword "native"
        , P.succeed MethodModifier_Static |. P.keyword "strictfp"
        ]


type MethodHeader
    = MethodHeader_Result Result MethodDeclarator (Maybe Throws)
    | MethodHeader_TypeParameters TypeParameters (List Annotation) Result MethodDeclarator (Maybe Throws)


methodHeader : Parser MethodHeader
methodHeader =
    P.oneOf
        [ P.succeed MethodHeader_Result
            |= result
            |. P.spaces
            |= methodDeclarator
            |. P.spaces
            |= optional throws
        , P.succeed MethodHeader_TypeParameters
            |= typeParameters
            |. P.spaces
            |= list annotation
            |. P.spaces
            |= result
            |. P.spaces
            |= methodDeclarator
            |. P.spaces
            |= optional throws
        ]


type Result
    = Result_UnannType UnannType
    | Result_Void


result : Parser Result
result =
    P.oneOf
        [ P.succeed Result_UnannType
            |= unannType
        , P.succeed Result_Void
            |. P.keyword "void"
        ]


type MethodDeclarator
    = MethodDeclarator Identifier (Maybe ReceiverParameter) (Maybe FormalParameterList) (Maybe Dims)


methodDeclarator : Parser MethodDeclarator
methodDeclarator =
    P.succeed MethodDeclarator
        |= identifier
        |. P.spaces
        |. P.symbol "("
        |= optional
            (P.succeed identity
                |= receiverParameter
                |. P.symbol ","
            )
        |= optional formalParameterList
        |. P.symbol ")"
        |. P.spaces
        |= optional dims


type ReceiverParameter
    = ReceiverParameter (List Annotation) UnannType (Maybe Identifier)


receiverParameter : Parser ReceiverParameter
receiverParameter =
    P.succeed ReceiverParameter
        |= list annotation
        |. P.spaces
        |= unannType
        |. P.spaces
        |= optional
            (P.succeed identity
                |= identifier
                |. P.symbol "."
            )
        |. P.spaces
        |. P.keyword "this"


type FormalParameterList
    = FormalParameterList (List FormalParameter)


formalParameterList : Parser FormalParameterList
formalParameterList =
    P.succeed FormalParameterList
        |= nonEmptySep "," formalParameter


type FormalParameter
    = FormalParameter_Normal (List VariableModifier) UnannType VariableDeclaratorId
    | FormalParameter_Arity VariableArityParameter


formalParameter : Parser FormalParameter
formalParameter =
    P.oneOf
        [ P.succeed FormalParameter_Normal
            |= list variableModifier
            |. P.spaces
            |= unannType
            |. P.spaces
            |= variableDeclaratorId
        , P.succeed FormalParameter_Arity
            |= variableArityParameter
        ]


type VariableArityParameter
    = VariableArityParameter (List VariableModifier) UnannType (List Annotation) Identifier


variableArityParameter : Parser VariableArityParameter
variableArityParameter =
    P.succeed VariableArityParameter
        |= list variableModifier
        |. P.spaces
        |= unannType
        |. P.spaces
        |= list annotation
        |. P.spaces
        |. P.keyword "..."
        |. P.spaces
        |= identifier


type VariableModifier
    = VariableModifier_Annotation Annotation
    | VariableModifier_Final


variableModifier : Parser VariableModifier
variableModifier =
    P.oneOf
        [ P.succeed VariableModifier_Annotation |= annotation
        , P.succeed VariableModifier_Final |. P.keyword "final"
        ]


type Throws
    = Throws ExceptionTypeList


throws : Parser Throws
throws =
    P.succeed Throws
        |. P.keyword "throws"
        |. P.spaces
        |= exceptionTypeList


type ExceptionTypeList
    = ExceptionTypeList (List ExceptionType)


exceptionTypeList : Parser ExceptionTypeList
exceptionTypeList =
    P.succeed ExceptionTypeList
        |= nonEmptySep "," exceptionType


type ExceptionType
    = ExceptionType_Class ClassType
    | ExceptionType_TypeVariable TypeVariable


exceptionType : Parser ExceptionType
exceptionType =
    P.oneOf
        [ P.succeed ExceptionType_Class
            |= classType
        , P.succeed ExceptionType_TypeVariable
            |= typeVariable
        ]


type MethodBody
    = MethodBody_Block Block
    | MethodBody_Semi


methodBody : Parser MethodBody
methodBody =
    P.oneOf
        [ P.succeed MethodBody_Block
            |= block
        , P.succeed MethodBody_Semi
            |. P.symbol ";"
        ]


type InstanceInitializer
    = InstanceInitializer Block


instanceInitializer : Parser InstanceInitializer
instanceInitializer =
    P.succeed InstanceInitializer
        |= block


type StaticInitializer
    = StaticInitializer Block


staticInitializer : Parser StaticInitializer
staticInitializer =
    P.succeed StaticInitializer
        |. P.keyword "static"
        |. P.spaces
        |= block


type ConstructorDeclaration
    = ConstructorDeclaration (List ConstructorModifier) ConstructorDeclarator (Maybe Throws) ConstructorBody


constructorDeclaration : Parser ConstructorDeclaration
constructorDeclaration =
    P.succeed ConstructorDeclaration
        |= list constructorModifier
        |. P.spaces
        |= constructorDeclarator
        |. P.spaces
        |= optional throws
        |. P.spaces
        |= constructorBody


type ConstructorModifier
    = ConstructorModifier_Annotation Annotation
    | ConstructorModifier_Public
    | ConstructorModifier_Protected
    | ConstructorModifier_Private


constructorModifier : Parser ConstructorModifier
constructorModifier =
    P.oneOf
        [ P.succeed ConstructorModifier_Annotation
            |= annotation
        , P.succeed ConstructorModifier_Public
            |. P.keyword "public"
        , P.succeed ConstructorModifier_Protected
            |. P.keyword "protected"
        , P.succeed ConstructorModifier_Private
            |. P.keyword "private"
        ]


type ConstructorDeclarator
    = ConstructorDeclarator (Maybe TypeParameters) SimpleTypeName (Maybe ReceiverParameter) (Maybe FormalParameterList)


constructorDeclarator : Parser ConstructorDeclarator
constructorDeclarator =
    P.succeed ConstructorDeclarator
        |= optional typeParameters
        |. P.spaces
        |= simpleTypeName
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= optional
            (P.succeed identity
                |= receiverParameter
                |. P.spaces
                |. P.symbol ","
            )
        |. P.spaces
        |= optional formalParameterList
        |. P.spaces
        |. P.symbol ")"


type SimpleTypeName
    = SimpleTypeName TypeIdentifier


simpleTypeName : Parser SimpleTypeName
simpleTypeName =
    P.succeed SimpleTypeName
        |= typeIdentifier


type ConstructorBody
    = ConstructorBody (Maybe ExplicitConstructorInvocation) (Maybe BlockStatements)


constructorBody : Parser ConstructorBody
constructorBody =
    P.succeed ConstructorBody
        |. P.symbol "{"
        |. P.spaces
        |= optional explicitConstructorInvocation
        |. P.spaces
        |= optional blockStatements
        |. P.spaces
        |. P.symbol "}"


type ExplicitConstructorInvocation
    = ExplicitConstructorInvocation_This (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_Super (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_ExpressionSuper ExpressionName (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_PrimarySuper Primary (Maybe TypeArguments) (Maybe ArgumentList)


explicitConstructorInvocation : Parser ExplicitConstructorInvocation
explicitConstructorInvocation =
    P.oneOf
        [ P.succeed ExplicitConstructorInvocation_This
            |= optional typeArguments
            |. P.spaces
            |. P.keyword "this"
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |. P.symbol ";"
        , P.succeed ExplicitConstructorInvocation_Super
            |= optional typeArguments
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |. P.symbol ";"
        , P.succeed ExplicitConstructorInvocation_ExpressionSuper
            |= expressionName
            |. P.spaces
            |. P.keyword "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |. P.symbol ";"
        , P.succeed ExplicitConstructorInvocation_PrimarySuper
            |= primary
            |. P.spaces
            |. P.keyword "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |. P.symbol ";"
        ]


type EnumDeclaration
    = EnumDeclaration (List ClassModifier) TypeIdentifier (Maybe Superinterfaces) EnumBody


enumDeclaration : Parser EnumDeclaration
enumDeclaration =
    P.succeed EnumDeclaration
        |= list classModifier
        |. P.spaces
        |. P.keyword "enum"
        |. P.spaces
        |= typeIdentifier
        |. P.spaces
        |= optional superinterfaces
        |. P.spaces
        |= enumBody


type EnumBody
    = EnumBody (Maybe EnumConstantList) (Maybe EnumBodyDeclarations)


enumBody : Parser EnumBody
enumBody =
    P.succeed EnumBody
        |. P.symbol "{"
        |. P.spaces
        |= optional enumConstantList
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= optional enumBodyDeclarations
        |. P.spaces
        |. P.symbol "{"


type EnumConstantList
    = EnumConstantList EnumConstant (List EnumConstant)


enumConstantList : Parser EnumConstantList
enumConstantList =
    P.succeed EnumConstantList
        |= enumConstant
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= enumConstant
            )


type EnumConstant
    = EnumConstant (List EnumConstantModifier) Identifier (Maybe (Maybe ArgumentList)) (Maybe ClassBody)


enumConstant =
    P.succeed EnumConstant
        |= list enumConstantModifier
        |. P.spaces
        |= identifier
        |. P.spaces
        |= optional
            (P.succeed identity
                |. P.symbol "("
                |. P.spaces
                |= optional argumentList
                |. P.spaces
                |. P.symbol ")"
            )
        |. P.spaces
        |= optional classBody


type EnumConstantModifier
    = EnumConstantModifier Annotation


enumConstantModifier : Parser EnumConstantModifier
enumConstantModifier =
    P.succeed EnumConstantModifier
        |= annotation


type EnumBodyDeclarations
    = EnumBodyDeclarations (List ClassBodyDeclaration)


enumBodyDeclarations : Parser EnumBodyDeclarations
enumBodyDeclarations =
    P.succeed EnumBodyDeclarations
        |. P.symbol ";"
        |. P.spaces
        |= list classBodyDeclaration



-- }}}
-- {{{ Productions from §9 (Interfaces)


type InterfaceDeclaration
    = InterfaceDeclaration_Normal NormalInterfaceDeclaration
    | InterfaceDeclaration_Annotation AnnotationTypeDeclaration


interfaceDeclaration : Parser InterfaceDeclaration
interfaceDeclaration =
    P.oneOf
        [ P.succeed InterfaceDeclaration_Normal
            |= normalInterfaceDeclaration
        , P.succeed InterfaceDeclaration_Annotation
            |= annotationTypeDeclaration
        ]


type NormalInterfaceDeclaration
    = NormalInterfaceDeclaration (List InterfaceModifier) TypeIdentifier (Maybe TypeParameters) (Maybe ExtendsInterfaces) InterfaceBody


normalInterfaceDeclaration : Parser NormalInterfaceDeclaration
normalInterfaceDeclaration =
    P.succeed NormalInterfaceDeclaration
        |= list interfaceModifier
        |. P.spaces
        |. P.keyword "interface"
        |. P.spaces
        |= typeIdentifier
        |. P.spaces
        |= optional typeParameters
        |. P.spaces
        |= optional extendsInterfaces
        |. P.spaces
        |= interfaceBody


type InterfaceModifier
    = InterfaceModifier_Annotation Annotation
    | InterfaceModifier_Public
    | InterfaceModifier_Protected
    | InterfaceModifier_Private
    | InterfaceModifier_Abstract
    | InterfaceModifier_Static
    | InterfaceModifier_Strictfp


interfaceModifier : Parser InterfaceModifier
interfaceModifier =
    P.oneOf
        [ P.succeed InterfaceModifier_Annotation
            |= annotation
        , P.succeed InterfaceModifier_Public
            |. P.keyword "public"
        , P.succeed InterfaceModifier_Protected
            |. P.keyword "protected"
        , P.succeed InterfaceModifier_Private
            |. P.keyword "private"
        , P.succeed InterfaceModifier_Abstract
            |. P.keyword "abstract"
        , P.succeed InterfaceModifier_Static
            |. P.keyword "static"
        , P.succeed InterfaceModifier_Strictfp
            |. P.keyword "strictfp"
        ]


type ExtendsInterfaces
    = ExtendsInterfaces InterfaceTypeList


extendsInterfaces : Parser ExtendsInterfaces
extendsInterfaces =
    P.succeed ExtendsInterfaces
        |. P.keyword "extends"
        |. P.spaces
        |= interfaceTypeList


type InterfaceBody
    = InterfaceBody (List InterfaceMemberDeclaration)


interfaceBody : Parser InterfaceBody
interfaceBody =
    P.succeed InterfaceBody
        |. P.keyword "{"
        |. P.spaces
        |= list interfaceMemberDeclaration
        |. P.spaces
        |. P.keyword "}"


type InterfaceMemberDeclaration
    = InterfaceMemberDeclaration_Constant ConstantDeclaration
    | InterfaceMemberDeclaration_Method InterfaceMethodDeclaration
    | InterfaceMemberDeclaration_Class ClassDeclaration
    | InterfaceMemberDeclaration_Interface InterfaceDeclaration
    | InterfaceMemberDeclaration_Semi


interfaceMemberDeclaration : Parser InterfaceMemberDeclaration
interfaceMemberDeclaration =
    P.oneOf
        [ P.succeed InterfaceMemberDeclaration_Constant
            |= constantDeclaration
        , P.succeed InterfaceMemberDeclaration_Method
            |= interfaceMethodDeclaration
        , P.succeed InterfaceMemberDeclaration_Class
            |= P.lazy (\_ -> classDeclaration)
        , P.succeed InterfaceMemberDeclaration_Interface
            |= P.lazy (\_ -> interfaceDeclaration)
        , P.succeed InterfaceMemberDeclaration_Semi
            |. P.symbol ","
        ]


type ConstantDeclaration
    = ConstantDeclaration (List ConstantModifier) UnannType VariableDeclaratorList


constantDeclaration : Parser ConstantDeclaration
constantDeclaration =
    P.succeed ConstantDeclaration
        |= list constantModifier
        |. P.spaces
        |= unannType
        |. P.spaces
        |= variableDeclaratorList
        |. P.spaces
        |. P.symbol ";"


type ConstantModifier
    = ConstantModifier_Annotation Annotation
    | ConstantModifier_Public
    | ConstantModifier_Static
    | ConstantModifier_Final


constantModifier : Parser ConstantModifier
constantModifier =
    P.oneOf
        [ P.succeed ConstantModifier_Annotation
            |= annotation
        , P.succeed ConstantModifier_Public
            |. P.keyword "public"
        , P.succeed ConstantModifier_Static
            |. P.keyword "static"
        , P.succeed ConstantModifier_Final
            |. P.keyword "final"
        ]


type InterfaceMethodDeclaration
    = InterfaceMethodDeclaration (List InterfaceMethodModifier) MethodHeader MethodBody


interfaceMethodDeclaration : Parser InterfaceMethodDeclaration
interfaceMethodDeclaration =
    P.succeed InterfaceMethodDeclaration
        |= list interfaceMethodModifier
        |. P.spaces
        |= methodHeader
        |. P.spaces
        |= methodBody


type InterfaceMethodModifier
    = InterfaceMethodModifier_Annotation Annotation
    | InterfaceMethodModifier_Public
    | InterfaceMethodModifier_Private
    | InterfaceMethodModifier_Abstract
    | InterfaceMethodModifier_Default
    | InterfaceMethodModifier_Static
    | InterfaceMethodModifier_Strictfp


interfaceMethodModifier : Parser InterfaceMethodModifier
interfaceMethodModifier =
    P.oneOf
        [ P.succeed InterfaceMethodModifier_Annotation
            |= annotation
        , P.succeed InterfaceMethodModifier_Public
            |. P.keyword "public"
        , P.succeed InterfaceMethodModifier_Private
            |. P.keyword "private"
        , P.succeed InterfaceMethodModifier_Abstract
            |. P.keyword "abstract"
        , P.succeed InterfaceMethodModifier_Default
            |. P.keyword "default"
        , P.succeed InterfaceMethodModifier_Static
            |. P.keyword "static"
        , P.succeed InterfaceMethodModifier_Strictfp
            |. P.keyword "strictfp"
        ]


type AnnotationTypeDeclaration
    = AnnotationTypeDeclaration (List InterfaceModifier) TypeIdentifier AnnotationTypeBody


annotationTypeDeclaration : Parser AnnotationTypeDeclaration
annotationTypeDeclaration =
    P.succeed AnnotationTypeDeclaration
        |= list interfaceModifier
        |. P.spaces
        |. P.symbol "@"
        |. P.spaces
        |. P.keyword "interface"
        |. P.spaces
        |= typeIdentifier
        |. P.spaces
        |= annotationTypeBody


type AnnotationTypeBody
    = AnnotationTypeBody (List AnnotationTypeMemberDeclaration)


annotationTypeBody : Parser AnnotationTypeBody
annotationTypeBody =
    P.succeed AnnotationTypeBody
        |. P.symbol "{"
        |. P.spaces
        |= list annotationTypeMemberDeclaration
        |. P.spaces
        |. P.symbol "}"


type AnnotationTypeMemberDeclaration
    = AnnotationTypeMemberDeclaration_Element AnnotationTypeElementDeclaration
    | AnnotationTypeMemberDeclaration_Constant ConstantDeclaration
    | AnnotationTypeMemberDeclaration_Class ClassDeclaration
    | AnnotationTypeMemberDeclaration_Interface InterfaceDeclaration
    | AnnotationTypeMemberDeclaration_Semi


annotationTypeMemberDeclaration : Parser AnnotationTypeMemberDeclaration
annotationTypeMemberDeclaration =
    P.oneOf
        [ P.succeed AnnotationTypeMemberDeclaration_Element
            |= annotationTypeElementDeclaration
        , P.succeed AnnotationTypeMemberDeclaration_Constant
            |= constantDeclaration
        , P.succeed AnnotationTypeMemberDeclaration_Class
            |= P.lazy (\_ -> classDeclaration)
        , P.succeed AnnotationTypeMemberDeclaration_Interface
            |= P.lazy (\_ -> interfaceDeclaration)
        , P.succeed AnnotationTypeMemberDeclaration_Semi
            |. P.symbol ";"
        ]


type AnnotationTypeElementDeclaration
    = AnnotationTypeElementDeclaration (List AnnotationTypeElementModifier) UnannType Identifier (Maybe Dims) (Maybe DefaultValue)


annotationTypeElementDeclaration : Parser AnnotationTypeElementDeclaration
annotationTypeElementDeclaration =
    P.succeed AnnotationTypeElementDeclaration
        |= list annotationTypeElementModifier
        |. P.spaces
        |= unannType
        |. P.spaces
        |= identifier
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= optional dims
        |. P.spaces
        |= optional defaultValue
        |. P.spaces
        |. P.symbol ";"


type AnnotationTypeElementModifier
    = AnnotationTypeElementModifier_Annotation Annotation
    | AnnotationTypeElementModifier_Public
    | AnnotationTypeElementModifier_Abstract


annotationTypeElementModifier : Parser AnnotationTypeElementModifier
annotationTypeElementModifier =
    P.oneOf
        [ P.succeed AnnotationTypeElementModifier_Annotation
            |= annotation
        , P.succeed AnnotationTypeElementModifier_Public
            |. P.keyword "public"
        , P.succeed AnnotationTypeElementModifier_Abstract
            |. P.keyword "abstract"
        ]


type DefaultValue
    = DefaultValue ElementValue


defaultValue : Parser DefaultValue
defaultValue =
    P.succeed DefaultValue
        |. P.keyword "default"
        |. P.spaces
        |= elementValue


type Annotation
    = Annotation_Normal NormalAnnotation
    | Annotation_Marker MarkerAnnotation
    | Annotation_SingleElement SingleElementAnnotation


annotation : Parser Annotation
annotation =
    P.oneOf
        [ P.succeed Annotation_Normal
            |= normalAnnotation
        , P.succeed Annotation_Marker
            |= markerAnnotation
        , P.succeed Annotation_SingleElement
            |= singleElementAnnotation
        ]


type NormalAnnotation
    = NormalAnnotation TypeName (Maybe ElementValuePairList)


normalAnnotation : Parser NormalAnnotation
normalAnnotation =
    P.succeed NormalAnnotation
        |. P.symbol "@"
        |. P.spaces
        |= typeName
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= optional elementValuePairList
        |. P.spaces
        |. P.symbol ")"


type ElementValuePairList
    = ElementValuePairList ElementValuePair (List ElementValuePair)


elementValuePairList : Parser ElementValuePairList
elementValuePairList =
    P.succeed ElementValuePairList
        |= elementValuePair
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= elementValuePair
            )


type ElementValuePair
    = ElementValuePair Identifier ElementValue


elementValuePair : Parser ElementValuePair
elementValuePair =
    P.succeed ElementValuePair
        |= identifier
        |. P.spaces
        |. P.symbol "="
        |. P.spaces
        |= elementValue


type ElementValue
    = ElementValue_Conditional ConditionalExpression
    | ElementValue_ArrayInitializer ElementValueArrayInitializer
    | ElementValue_Annotation Annotation


elementValue : Parser ElementValue
elementValue =
    P.oneOf
        [ P.succeed ElementValue_Conditional
            |= conditionalExpression
        , P.succeed ElementValue_ArrayInitializer
            |= elementValueArrayInitializer
        , P.succeed ElementValue_Annotation
            |= P.lazy (\_ -> annotation)
        ]


type ElementValueArrayInitializer
    = ElementValueArrayInitializer (Maybe ElementValueList)


elementValueArrayInitializer : Parser ElementValueArrayInitializer
elementValueArrayInitializer =
    P.succeed ElementValueArrayInitializer
        |. P.symbol "{"
        |. P.spaces
        |= optional (P.lazy (\_ -> elementValueList))
        |. P.spaces
        |. optional (P.symbol ",")
        |. P.spaces
        |. P.symbol "}"


type ElementValueList
    = ElementValueList ElementValue (List ElementValue)


elementValueList : Parser ElementValueList
elementValueList =
    P.succeed ElementValueList
        |= elementValue
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= elementValue
            )


type MarkerAnnotation
    = MarkerAnnotation TypeName


markerAnnotation : Parser MarkerAnnotation
markerAnnotation =
    P.succeed MarkerAnnotation
        |. P.symbol "@"
        |= typeName


type SingleElementAnnotation
    = SingleElementAnnotation TypeName ElementValue


singleElementAnnotation : Parser SingleElementAnnotation
singleElementAnnotation =
    P.succeed SingleElementAnnotation
        |. P.symbol "@"
        |. P.spaces
        |= typeName
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= P.lazy (\_ -> elementValue)
        |. P.spaces
        |. P.symbol ")"



-- }}}
-- {{{ Productions from §10 (Arrays)


type ArrayInitializer
    = ArrayInitializer (Maybe VariableInitializerList)


arrayInitializer : Parser ArrayInitializer
arrayInitializer =
    P.succeed ArrayInitializer
        |. P.symbol "{"
        |= optional (P.lazy (\_ -> variableInitializerList))
        |. P.spaces
        |. optional (P.symbol ",")
        |. P.symbol "}"


type VariableInitializerList
    = VariableInitializerList VariableInitializer (List VariableInitializer)


variableInitializerList : Parser VariableInitializerList
variableInitializerList =
    P.succeed VariableInitializerList
        |= variableInitializer
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= variableInitializer
            )



-- }}}
-- {{{ Productions from §14 (Blocks and Statements)


type Block
    = Block (Maybe BlockStatements)


block : Parser Block
block =
    P.succeed Block
        |. P.symbol "{"
        |= optional blockStatements
        |. P.symbol "}"


type BlockStatements
    = BlockStatements BlockStatement (List BlockStatement)


blockStatements : Parser BlockStatements
blockStatements =
    P.succeed BlockStatements
        |= blockStatement
        |. P.spaces
        |= list blockStatement
        |. P.spaces


type BlockStatement
    = BlockStatement_LocalVariable LocalVariableDeclarationStatement
    | BlockStatement_Class ClassDeclaration
    | BlockStatement_Statement Statement


blockStatement : Parser BlockStatement
blockStatement =
    P.oneOf
        [ P.succeed BlockStatement_LocalVariable
            |= localVariableDeclarationStatement
        , P.succeed BlockStatement_Class
            |= P.lazy (\_ -> classDeclaration)
        , P.succeed BlockStatement_Statement
            |= statement
        ]


type LocalVariableDeclarationStatement
    = LocalVariableDeclarationStatement LocalVariableDeclaration


localVariableDeclarationStatement : Parser LocalVariableDeclarationStatement
localVariableDeclarationStatement =
    P.succeed LocalVariableDeclarationStatement
        |= localVariableDeclaration
        |. P.spaces
        |. P.symbol ";"


type LocalVariableDeclaration
    = LocalVariableDeclaration (List VariableModifier) LocalVariableType VariableDeclaratorList


localVariableDeclaration : Parser LocalVariableDeclaration
localVariableDeclaration =
    P.succeed LocalVariableDeclaration
        |= list variableModifier
        |. P.spaces
        |= localVariableType
        |. P.spaces
        |= variableDeclaratorList


type LocalVariableType
    = LocalVariableType_UnannType UnannType
    | LocalVariableType_Var


localVariableType : Parser LocalVariableType
localVariableType =
    P.oneOf
        [ P.succeed LocalVariableType_UnannType
            |= unannType
        , P.succeed LocalVariableType_Var
            |. P.keyword "var"
        ]


type Statement
    = Statement_Statement StatementWithoutTrailingSubstatement
    | Statement_Labeled LabeledStatement
    | Statement_If IfThenStatement
    | Statement_IfThenElse IfThenElseStatement
    | Statement_While WhileStatement
    | Statement_For ForStatement


statement : Parser Statement
statement =
    P.oneOf
        [ P.succeed Statement_Statement
            |= statementWithoutTrailingSubstatement
        , P.succeed Statement_Labeled
            |= labeledStatement
        , P.succeed Statement_If
            |= ifThenStatement
        , P.succeed Statement_IfThenElse
            |= ifThenElseStatement
        , P.succeed Statement_While
            |= whileStatement
        , P.succeed Statement_For
            |= forStatement
        ]


type StatementNoShortIf
    = StatementNoShortIf_NoTrailing StatementWithoutTrailingSubstatement
    | StatementNoShortIf_Labeled LabeledStatementNoShortIf
    | StatementNoShortIf_IfThenElse IfThenElseStatementNoShortIf
    | StatementNoShortIf_While WhileStatementNoShortIf
    | StatementNoShortIf_For ForStatementNoShortIf


statementNoShortIf : Parser StatementNoShortIf
statementNoShortIf =
    P.oneOf
        [ P.succeed StatementNoShortIf_NoTrailing
            |= statementWithoutTrailingSubstatement
        , P.succeed StatementNoShortIf_Labeled
            |= labeledStatementNoShortIf
        , P.succeed StatementNoShortIf_IfThenElse
            |= ifThenElseStatementNoShortIf
        , P.succeed StatementNoShortIf_While
            |= whileStatementNoShortIf
        , P.succeed StatementNoShortIf_For
            |= forStatementNoShortIf
        ]


type StatementWithoutTrailingSubstatement
    = StatementWithoutTrailingSubstatement_Block Block
    | StatementWithoutTrailingSubstatement_Empty EmptyStatement
    | StatementWithoutTrailingSubstatement_Expression ExpressionStatement
    | StatementWithoutTrailingSubstatement_Assert AssertStatement
    | StatementWithoutTrailingSubstatement_Switch SwitchStatement
    | StatementWithoutTrailingSubstatement_Do DoStatement
    | StatementWithoutTrailingSubstatement_Break BreakStatement
    | StatementWithoutTrailingSubstatement_Continue ContinueStatement
    | StatementWithoutTrailingSubstatement_Return ReturnStatement
    | StatementWithoutTrailingSubstatement_Synchronized SynchronizedStatement
    | StatementWithoutTrailingSubstatement_Throw ThrowStatement
    | StatementWithoutTrailingSubstatement_Try TryStatement
    | StatementWithoutTrailingSubstatement_Yield YieldStatement


statementWithoutTrailingSubstatement : Parser StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatement =
    P.oneOf
        [ P.succeed StatementWithoutTrailingSubstatement_Block
            |= P.lazy (\_ -> block)
        , P.succeed StatementWithoutTrailingSubstatement_Empty
            |= emptyStatement
        , P.succeed StatementWithoutTrailingSubstatement_Expression
            |= expressionStatement
        , P.succeed StatementWithoutTrailingSubstatement_Assert
            |= assertStatement
        , P.succeed StatementWithoutTrailingSubstatement_Switch
            |= switchStatement
        , P.succeed StatementWithoutTrailingSubstatement_Do
            |= doStatement
        , P.succeed StatementWithoutTrailingSubstatement_Break
            |= breakStatement
        , P.succeed StatementWithoutTrailingSubstatement_Continue
            |= continueStatement
        , P.succeed StatementWithoutTrailingSubstatement_Return
            |= returnStatement
        , P.succeed StatementWithoutTrailingSubstatement_Synchronized
            |= synchronizedStatement
        , P.succeed StatementWithoutTrailingSubstatement_Throw
            |= throwStatement
        , P.succeed StatementWithoutTrailingSubstatement_Try
            |= tryStatement
        , P.succeed StatementWithoutTrailingSubstatement_Yield
            |= yieldStatement
        ]


type EmptyStatement
    = EmptyStatement


emptyStatement : Parser EmptyStatement
emptyStatement =
    P.succeed EmptyStatement
        |. P.symbol ";"


type LabeledStatement
    = LabeledStatement Identifier Statement


labeledStatement : Parser LabeledStatement
labeledStatement =
    P.succeed LabeledStatement
        |= identifier
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type LabeledStatementNoShortIf
    = LabeledStatementNoShortIf Identifier StatementNoShortIf


labeledStatementNoShortIf : Parser LabeledStatementNoShortIf
labeledStatementNoShortIf =
    P.succeed LabeledStatementNoShortIf
        |= identifier
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)


type ExpressionStatement
    = ExpressionStatement StatementExpression


expressionStatement : Parser ExpressionStatement
expressionStatement =
    P.succeed ExpressionStatement
        |= statementExpression
        |. P.spaces
        |. P.symbol ";"


type StatementExpression
    = StatementExpression_Assignment Assignment
    | StatementExpression_PreIncrement PreIncrementExpression
    | StatementExpression_PreDecrement PreDecrementExpression
    | StatementExpression_PostIncrement PostIncrementExpression
    | StatementExpression_PostDecrement PostDecrementExpression
    | StatementExpression_MethodInvocation MethodInvocation
    | StatementExpression_ClassCreation ClassInstanceCreationExpression


statementExpression : Parser StatementExpression
statementExpression =
    P.oneOf
        [ P.succeed StatementExpression_Assignment
            |= assignment
        , P.succeed StatementExpression_PreIncrement
            |= preIncrementExpression
        , P.succeed StatementExpression_PreDecrement
            |= preDecrementExpression
        , P.succeed StatementExpression_PostIncrement
            |= postIncrementExpression
        , P.succeed StatementExpression_PostDecrement
            |= postDecrementExpression
        , P.succeed StatementExpression_MethodInvocation
            |= methodInvocation
        , P.succeed StatementExpression_ClassCreation
            |= classInstanceCreationExpression
        ]


type IfThenStatement
    = IfThenStatement Expression Statement


ifThenStatement : Parser IfThenStatement
ifThenStatement =
    P.succeed IfThenStatement
        |. P.keyword "if"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type IfThenElseStatement
    = IfThenElseStatement Expression StatementNoShortIf Statement


ifThenElseStatement : Parser IfThenElseStatement
ifThenElseStatement =
    P.succeed IfThenElseStatement
        |. P.keyword "if"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= statementNoShortIf
        |. P.spaces
        |. P.keyword "else"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type IfThenElseStatementNoShortIf
    = IfThenElseStatementNoShortIf Expression StatementNoShortIf StatementNoShortIf


ifThenElseStatementNoShortIf : Parser IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf =
    P.succeed IfThenElseStatementNoShortIf
        |. P.keyword "if"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)
        |. P.spaces
        |. P.keyword "else"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)


type AssertStatement
    = AssertStatement_Expression Expression
    | AssertStatement_WithError Expression Expression


assertStatement : Parser AssertStatement
assertStatement =
    P.oneOf
        [ P.succeed AssertStatement_Expression
            |. P.keyword "assert"
            |. P.spaces
            |= expression
            |. P.spaces
            |. P.symbol ";"
        , P.succeed AssertStatement_WithError
            |. P.keyword "assert"
            |. P.spaces
            |= expression
            |. P.spaces
            |. P.symbol ":"
            |. P.spaces
            |= expression
            |. P.spaces
            |. P.symbol ";"
        ]


type SwitchStatement
    = SwitchStatement Expression SwitchBlock


switchStatement : Parser SwitchStatement
switchStatement =
    P.succeed SwitchStatement
        |. P.keyword "switch"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= switchBlock


type SwitchBlock
    = SwitchBlock_Rule SwitchRule (List SwitchRule)
    | SwitchBlock_Group (List SwitchBlockStatementGroup) (List SwitchLabel)


switchBlock : Parser SwitchBlock
switchBlock =
    P.succeed identity
        |. P.symbol "{"
        |. P.spaces
        |= P.oneOf
            [ P.succeed SwitchBlock_Rule
                |= switchRule
                |. P.spaces
                |= list switchRule
            , P.succeed SwitchBlock_Group
                |= list switchBlockStatementGroup
                |. P.spaces
                |= list
                    (P.succeed identity
                        |= switchLabel
                        |. P.spaces
                        |. P.symbol ":"
                    )
            ]
        |. P.spaces
        |. P.symbol "}"


type SwitchRule
    = SwitchRule_Expression SwitchLabel Expression
    | SwitchRule_Block SwitchLabel Block
    | SwitchRule_Throw SwitchLabel ThrowStatement


switchRule : Parser SwitchRule
switchRule =
    P.oneOf
        [ P.succeed SwitchRule_Expression
            |= switchLabel
            |. P.spaces
            |. P.symbol "->"
            |. P.spaces
            |= expression
            |. P.spaces
            |. P.symbol ";"
        , P.succeed SwitchRule_Block
            |= switchLabel
            |. P.spaces
            |. P.symbol "->"
            |. P.spaces
            |= P.lazy (\_ -> block)
            |. P.spaces
            |. P.symbol ";"
        , P.succeed SwitchRule_Throw
            |= switchLabel
            |. P.spaces
            |. P.symbol "->"
            |. P.spaces
            |= throwStatement
            |. P.spaces
            |. P.symbol ";"
        ]


type SwitchBlockStatementGroup
    = SwitchBlockStatementGroup SwitchLabel (List SwitchLabel) BlockStatements


switchBlockStatementGroup : Parser SwitchBlockStatementGroup
switchBlockStatementGroup =
    P.succeed SwitchBlockStatementGroup
        |= switchLabel
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= list
            (P.succeed identity
                |= switchLabel
                |. P.spaces
                |. P.keyword ":"
            )
        |. P.spaces
        |= P.lazy (\_ -> blockStatements)


type SwitchLabel
    = SwitchLabel_Case CaseConstant (List CaseConstant)
    | SwitchLabel_Default


switchLabel : Parser SwitchLabel
switchLabel =
    P.oneOf
        [ P.succeed SwitchLabel_Case
            |. P.keyword "case"
            |. P.spaces
            |= caseConstant
            |= list
                (P.succeed identity
                    |. P.symbol ","
                    |. P.spaces
                    |= caseConstant
                )
        , P.succeed SwitchLabel_Default
            |. P.keyword "default"
        ]


type CaseConstant
    = CaseConstant ConditionalExpression


caseConstant : Parser CaseConstant
caseConstant =
    P.succeed CaseConstant
        |= conditionalExpression


type WhileStatement
    = WhileStatement Expression Statement


whileStatement : Parser WhileStatement
whileStatement =
    P.succeed WhileStatement
        |. P.keyword "while"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type WhileStatementNoShortIf
    = WhileStatementNoShortIf Expression StatementNoShortIf


whileStatementNoShortIf : Parser WhileStatementNoShortIf
whileStatementNoShortIf =
    P.succeed WhileStatementNoShortIf
        |. P.keyword "while"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)


type DoStatement
    = DoStatement Statement Expression


doStatement : Parser DoStatement
doStatement =
    P.succeed DoStatement
        |. P.keyword "do"
        |. P.spaces
        |= P.lazy (\_ -> statement)
        |. P.spaces
        |. P.keyword "while"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |. P.symbol ";"


type ForStatement
    = ForStatement_Basic BasicForStatement
    | ForStatement_Enhanced EnhancedForStatement


forStatement : Parser ForStatement
forStatement =
    P.oneOf
        [ P.succeed ForStatement_Basic
            |= basicForStatement
        , P.succeed ForStatement_Enhanced
            |= enhancedForStatement
        ]


type ForStatementNoShortIf
    = ForStatementNoShortIf_Basic BasicForStatementNoShortIf
    | ForStatementNoShortIf_Enhanced EnhancedForStatementNoShortIf


forStatementNoShortIf : Parser ForStatementNoShortIf
forStatementNoShortIf =
    P.oneOf
        [ P.succeed ForStatementNoShortIf_Basic
            |= basicForStatementNoShortIf
        , P.succeed ForStatementNoShortIf_Enhanced
            |= enhancedForStatementNoShortIf
        ]


type BasicForStatement
    = BasicForStatement (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) Statement


basicForStatement : Parser BasicForStatement
basicForStatement =
    P.succeed BasicForStatement
        |. P.keyword "for"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= optional forInit
        |. P.spaces
        |. P.symbol ";"
        |. P.spaces
        |= optional expression
        |. P.spaces
        |. P.symbol ";"
        |. P.spaces
        |= optional forUpdate
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type BasicForStatementNoShortIf
    = BasicForStatementNoShortIf (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) StatementNoShortIf


basicForStatementNoShortIf : Parser BasicForStatementNoShortIf
basicForStatementNoShortIf =
    P.succeed BasicForStatementNoShortIf
        |. P.keyword "for"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= optional forInit
        |. P.spaces
        |. P.symbol ";"
        |. P.spaces
        |= optional expression
        |. P.spaces
        |. P.symbol ";"
        |. P.spaces
        |= optional forUpdate
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)


type ForInit
    = ForInit_StatementList StatementExpressionList
    | ForInit_Variable LocalVariableDeclaration


forInit : Parser ForInit
forInit =
    P.oneOf
        [ P.succeed ForInit_StatementList
            |= statementExpressionList
        , P.succeed ForInit_Variable
            |= localVariableDeclaration
        ]


type ForUpdate
    = ForUpdate StatementExpressionList


forUpdate : Parser ForUpdate
forUpdate =
    P.succeed ForUpdate
        |= statementExpressionList


type StatementExpressionList
    = StatementExpressionList StatementExpression (List StatementExpression)


statementExpressionList : Parser StatementExpressionList
statementExpressionList =
    P.succeed StatementExpressionList
        |= statementExpression
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= statementExpression
            )


type EnhancedForStatement
    = EnhancedForStatement (List VariableModifier) LocalVariableType VariableDeclaratorId Expression Statement


enhancedForStatement : Parser EnhancedForStatement
enhancedForStatement =
    P.succeed EnhancedForStatement
        |. P.keyword "for"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= list variableModifier
        |. P.spaces
        |= localVariableType
        |. P.spaces
        |= variableDeclaratorId
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statement)


type EnhancedForStatementNoShortIf
    = EnhancedForStatementNoShortIf (List VariableModifier) LocalVariableType VariableDeclaratorId Expression StatementNoShortIf


enhancedForStatementNoShortIf : Parser EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf =
    P.succeed EnhancedForStatementNoShortIf
        |. P.keyword "for"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= list variableModifier
        |. P.spaces
        |= localVariableType
        |. P.spaces
        |= variableDeclaratorId
        |. P.spaces
        |. P.symbol ":"
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> statementNoShortIf)


type BreakStatement
    = BreakStatement (Maybe Identifier)


breakStatement : Parser BreakStatement
breakStatement =
    P.succeed BreakStatement
        |. P.keyword "break"
        |. P.spaces
        |= optional identifier
        |. P.spaces
        |. P.symbol ";"


type YieldStatement
    = YieldStatement Expression


yieldStatement : Parser YieldStatement
yieldStatement =
    P.succeed YieldStatement
        |. P.keyword "yield"
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ";"


type ContinueStatement
    = ContinueStatement (Maybe Identifier)


continueStatement : Parser ContinueStatement
continueStatement =
    P.succeed ContinueStatement
        |. P.keyword "continue"
        |. P.spaces
        |= optional identifier
        |. P.spaces
        |. P.symbol ";"


type ReturnStatement
    = ReturnStatement (Maybe Expression)


returnStatement : Parser ReturnStatement
returnStatement =
    P.succeed ReturnStatement
        |. P.keyword "return"
        |. P.spaces
        |= optional expression
        |. P.spaces
        |. P.symbol ";"


type ThrowStatement
    = ThrowStatement Expression


throwStatement : Parser ThrowStatement
throwStatement =
    P.succeed ThrowStatement
        |. P.keyword "throw"
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ";"


type SynchronizedStatement
    = SynchronizedStatement Expression Block


synchronizedStatement : Parser SynchronizedStatement
synchronizedStatement =
    P.succeed SynchronizedStatement
        |. P.keyword "synchronized"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= P.lazy (\_ -> block)


type TryStatement
    = TryStatement_Normal Block Catches
    | TryStatement_Finally Block (Maybe Catches) Finally
    | TryStatement_With TryWithResourcesStatement


tryStatement : Parser TryStatement
tryStatement =
    P.oneOf
        [ P.succeed TryStatement_Normal
            |. P.keyword "try"
            |. P.spaces
            |= P.lazy (\_ -> block)
            |. P.spaces
            |= catches
        , P.succeed TryStatement_Finally
            |. P.keyword "try"
            |. P.spaces
            |= P.lazy (\_ -> block)
            |. P.spaces
            |= optional catches
            |. P.spaces
            |= finally
        , P.succeed TryStatement_With
            |= tryWithResourcesStatement
        ]


type Catches
    = Catches CatchClause (List CatchClause)


catches : Parser Catches
catches =
    P.succeed Catches
        |= catchClause
        |. P.spaces
        |= list catchClause


type CatchClause
    = CatchClause CatchFormalParameter Block


catchClause : Parser CatchClause
catchClause =
    P.succeed CatchClause
        |. P.keyword "catch"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= catchFormalParameter
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= P.lazy (\_ -> block)


type CatchFormalParameter
    = CatchFormalParameter (List VariableModifier) CatchType VariableDeclaratorId


catchFormalParameter : Parser CatchFormalParameter
catchFormalParameter =
    P.succeed CatchFormalParameter
        |= list variableModifier
        |. P.spaces
        |= catchType
        |. P.spaces
        |= variableDeclaratorId


type CatchType
    = CatchType UnannClassType (List ClassType)


catchType : Parser CatchType
catchType =
    P.succeed CatchType
        |= unannClassType
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol "|"
                |. P.spaces
                |= classType
            )


type Finally
    = Finally Block


finally : Parser Finally
finally =
    P.succeed Finally
        |. P.succeed "finally"
        |. P.spaces
        |= P.lazy (\_ -> block)


type TryWithResourcesStatement
    = TryWithResourcesStatement ResourceSpecification Block (Maybe Catches) (Maybe Finally)


tryWithResourcesStatement : Parser TryWithResourcesStatement
tryWithResourcesStatement =
    P.succeed TryWithResourcesStatement
        |. P.keyword "try"
        |. P.spaces
        |= resourceSpecification
        |. P.spaces
        |= P.lazy (\_ -> block)
        |. P.spaces
        |= optional catches
        |. P.spaces
        |= optional finally


type ResourceSpecification
    = ResourceSpecification ResourceList


resourceSpecification : Parser ResourceSpecification
resourceSpecification =
    P.succeed ResourceSpecification
        |. P.symbol "("
        |. P.spaces
        |= resourceList
        |. P.spaces
        |. optional (P.symbol ";")
        |. P.spaces
        |. P.symbol ")"


type ResourceList
    = ResourceList Resource (List Resource)


resourceList : Parser ResourceList
resourceList =
    P.succeed ResourceList
        |= resource
        |. P.spaces
        |= list
            (P.succeed identity
                |. P.symbol ";"
                |. P.spaces
                |= resource
            )


type Resource
    = Resource_Declaration (List VariableModifier) LocalVariableType Identifier Expression
    | Resource_VariableAccess VariableAccess


resource : Parser Resource
resource =
    P.oneOf
        [ P.succeed Resource_Declaration
            |= list variableModifier
            |. P.spaces
            |= localVariableType
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "="
            |. P.spaces
            |= expression
        , P.succeed Resource_VariableAccess
            |= variableAccess
        ]


type VariableAccess
    = VariableAccess_Expression ExpressionName
    | VariableAccess_Field FieldAccess


variableAccess : Parser VariableAccess
variableAccess =
    P.oneOf
        [ P.succeed VariableAccess_Expression
            |= expressionName
        , P.succeed VariableAccess_Field
            |= fieldAccess
        ]



-- }}}
-- {{{ Productions from §15 (Expressions)


type Primary
    = Primary_NoNewArray PrimaryNoNewArray
    | Primary_Creation ArrayCreationExpression


primary : Parser Primary
primary =
    P.oneOf
        [ P.succeed Primary_NoNewArray
            |= primaryNoNewArray
        , P.succeed Primary_Creation
            |= arrayCreationExpression
        ]


type PrimaryNoNewArray
    = PrimaryNoNewArray_Literal Literal
    | PrimaryNoNewArray_ClassLiteral ClassLiteral
    | PrimaryNoNewArray_This
    | PrimaryNoNewArray_TypeThis TypeName
    | PrimaryNoNewArray_BracketsExpression Expression
    | PrimaryNoNewArray_ClassCreation ClassInstanceCreationExpression
    | PrimaryNoNewArray_FieldAccess FieldAccess
    | PrimaryNoNewArray_ArrayAccess ArrayAccess
    | PrimaryNoNewArray_MethodInvocation MethodInvocation
    | PrimaryNoNewArray_MethodReference MethodReference


primaryNoNewArray : Parser PrimaryNoNewArray
primaryNoNewArray =
    P.oneOf
        [ P.succeed PrimaryNoNewArray_Literal
            |= literal
        , P.succeed PrimaryNoNewArray_ClassLiteral
            |= classLiteral
        , P.succeed PrimaryNoNewArray_This
            |. P.keyword "this"
        , P.succeed PrimaryNoNewArray_TypeThis
            |= typeName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "this"
        , P.succeed PrimaryNoNewArray_BracketsExpression
            |. P.symbol "("
            |. P.spaces
            |= expression
            |. P.spaces
            |. P.symbol ")"
        , P.succeed PrimaryNoNewArray_ClassCreation
            |= P.lazy (\_ -> classInstanceCreationExpression)
        , P.succeed PrimaryNoNewArray_FieldAccess
            |= fieldAccess
        , P.succeed PrimaryNoNewArray_ArrayAccess
            |= arrayAccess
        , P.succeed PrimaryNoNewArray_MethodInvocation
            |= methodInvocation
        , P.succeed PrimaryNoNewArray_MethodReference
            |= methodReference
        ]


type ClassLiteral
    = ClassLiteral_TypeName TypeName Int
    | ClassLiteral_Numeric NumericType Int
    | ClassLiteral_Boolean Int
    | ClassLiteral_Void


classLiteral : Parser ClassLiteral
classLiteral =
    P.oneOf
        [ P.succeed ClassLiteral_TypeName
            |= typeName
            |. P.spaces
            |= brackets
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "class"
        , P.succeed ClassLiteral_Numeric
            |= numericType
            |. P.spaces
            |= brackets
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "class"
        , P.succeed ClassLiteral_Boolean
            |. P.keyword "boolean"
            |. P.spaces
            |= brackets
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "class"
        , P.succeed ClassLiteral_Void
            |. P.keyword "void"
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "class"
        ]


type ClassInstanceCreationExpression
    = ClassInstanceCreationExpression_Normal UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Expression ExpressionName UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Primary Primary UnqualifiedClassInstanceCreationExpression


classInstanceCreationExpression : Parser ClassInstanceCreationExpression
classInstanceCreationExpression =
    P.oneOf
        [ P.succeed ClassInstanceCreationExpression_Normal
            |= unqualifiedClassInstanceCreationExpression
        , P.succeed ClassInstanceCreationExpression_Expression
            |= expressionName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= unqualifiedClassInstanceCreationExpression
        , P.succeed ClassInstanceCreationExpression_Primary
            |= P.lazy (\_ -> primary)
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= unqualifiedClassInstanceCreationExpression
        ]


type UnqualifiedClassInstanceCreationExpression
    = UnqualifiedClassInstanceCreationExpression (Maybe TypeArguments) ClassOrInterfaceTypeToInstantiate (Maybe ArgumentList) (Maybe ClassBody)


unqualifiedClassInstanceCreationExpression : Parser UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression =
    P.succeed UnqualifiedClassInstanceCreationExpression
        |. P.keyword "new"
        |. P.spaces
        |= optional typeArguments
        |. P.spaces
        |= classOrInterfaceTypeToInstantiate
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= optional argumentList
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= optional (P.lazy (\_ -> classBody))


type ClassOrInterfaceTypeToInstantiate
    = ClassOrInterfaceTypeToInstantiate (List Annotation) Identifier (List ( List Annotation, Identifier )) (Maybe TypeArgumentsOrDiamond)


classOrInterfaceTypeToInstantiate : Parser ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate =
    P.succeed ClassOrInterfaceTypeToInstantiate
        |= list annotation
        |. P.spaces
        |= identifier
        |. P.spaces
        |= list
            (P.succeed Tuple.pair
                |. P.symbol "."
                |. P.spaces
                |= list annotation
                |. P.spaces
                |= identifier
            )
        |. P.spaces
        |= optional typeArgumentsOrDiamond


type TypeArgumentsOrDiamond
    = TypeArguments_TypeArguments TypeArguments
    | TypeArguments_Diamond


typeArgumentsOrDiamond : Parser TypeArgumentsOrDiamond
typeArgumentsOrDiamond =
    P.oneOf
        [ P.succeed TypeArguments_TypeArguments
            |= typeArguments
        , P.succeed TypeArguments_Diamond
            |. P.symbol "<>"
        ]


type FieldAccess
    = FieldAccess_Primary Primary Identifier
    | FieldAccess_Super Identifier
    | FieldAccess_TypeNameSuper TypeName Identifier


fieldAccess : Parser FieldAccess
fieldAccess =
    P.oneOf
        [ P.succeed FieldAccess_Primary
            |= P.lazy (\_ -> primary)
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= identifier
        , P.succeed FieldAccess_Super
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= identifier
        , P.succeed FieldAccess_TypeNameSuper
            |= typeName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= identifier
        ]


type ArrayAccess
    = ArrayAccess_Expression ExpressionName Expression
    | ArrayAccess_Primary PrimaryNoNewArray Expression


arrayAccess : Parser ArrayAccess
arrayAccess =
    P.oneOf
        [ P.succeed ArrayAccess_Expression
            |= expressionName
            |. P.spaces
            |. P.symbol "["
            |. P.spaces
            |= P.lazy (\_ -> expression)
            |. P.spaces
            |. P.symbol "]"
        , P.succeed ArrayAccess_Primary
            |= P.lazy (\_ -> primaryNoNewArray)
            |. P.spaces
            |. P.symbol "["
            |. P.spaces
            |= P.lazy (\_ -> expression)
            |. P.spaces
            |. P.symbol "]"
        ]


type MethodInvocation
    = MethodInvocation_Name MethodName (Maybe ArgumentList)
    | MethodInvocation_Type TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Expression ExpressionName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Primary Primary (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Super (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_TypeSuper TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)


methodInvocation : Parser MethodInvocation
methodInvocation =
    P.oneOf
        [ P.succeed MethodInvocation_Name
            |= methodName
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed MethodInvocation_Type
            |= typeName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed MethodInvocation_Expression
            |= expressionName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed MethodInvocation_Primary
            |= P.lazy (\_ -> primary)
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed MethodInvocation_Super
            |. P.symbol "super"
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed MethodInvocation_TypeSuper
            |= typeName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
            |. P.spaces
            |. P.symbol "("
            |. P.spaces
            |= optional argumentList
            |. P.spaces
            |. P.symbol ")"
        ]


type ArgumentList
    = ArgumentList Expression (List Expression)


argumentList : Parser ArgumentList
argumentList =
    P.succeed ArgumentList
        |= expression
        |= list
            (P.succeed identity
                |. P.symbol ","
                |. P.spaces
                |= expression
            )


type MethodReference
    = MethodReference_Expression ExpressionName (Maybe TypeArguments) Identifier
    | MethodReference_Primary Primary (Maybe TypeArguments) Identifier
    | MethodReference_Reference ReferenceType (Maybe TypeArguments) Identifier
    | MethodReference_Super (Maybe TypeArguments) Identifier
    | MethodReference_TypeSuper TypeName (Maybe TypeArguments) Identifier
    | MethodReference_ClassNew ClassType (Maybe TypeArguments)
    | MethodReference_ArrayNew ArrayType


methodReference : Parser MethodReference
methodReference =
    P.oneOf
        [ P.succeed MethodReference_Expression
            |= expressionName
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
        , P.succeed MethodReference_Primary
            |= P.lazy (\_ -> primary)
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
        , P.succeed MethodReference_Reference
            |= referenceType
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
        , P.succeed MethodReference_Super
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
        , P.succeed MethodReference_TypeSuper
            |. P.spaces
            |= typeName
            |. P.spaces
            |. P.symbol "."
            |. P.spaces
            |. P.keyword "super"
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |= identifier
        , P.succeed MethodReference_ClassNew
            |= classType
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |= optional typeArguments
            |. P.spaces
            |. P.keyword "new"
        , P.succeed MethodReference_ArrayNew
            |= arrayType
            |. P.spaces
            |. P.symbol "::"
            |. P.spaces
            |. P.keyword "new"
        ]


type ArrayCreationExpression
    = ArrayCreationExpression_Primitive PrimitiveType DimExprs (Maybe Dims)
    | ArrayCreationExpression_Class ClassOrInterfaceType DimExprs (Maybe Dims)
    | ArrayCreationExpression_PrimitiveArrayInit PrimitiveType Dims ArrayInitializer
    | ArrayCreationExpression_ClassArrayInit ClassOrInterfaceType Dims ArrayInitializer


arrayCreationExpression : Parser ArrayCreationExpression
arrayCreationExpression =
    P.oneOf
        [ P.succeed ArrayCreationExpression_Primitive
            |. P.keyword "new"
            |. P.spaces
            |= primitiveType
            |. P.spaces
            |= dimExprs
            |. P.spaces
            |= optional dims
        , P.succeed ArrayCreationExpression_Class
            |. P.keyword "new"
            |. P.spaces
            |= classOrInterfaceType
            |. P.spaces
            |= dimExprs
            |. P.spaces
            |= optional dims
        , P.succeed ArrayCreationExpression_PrimitiveArrayInit
            |. P.keyword "new"
            |. P.spaces
            |= primitiveType
            |. P.spaces
            |= dims
            |. P.spaces
            |= arrayInitializer
        , P.succeed ArrayCreationExpression_ClassArrayInit
            |. P.keyword "new"
            |. P.spaces
            |= classOrInterfaceType
            |. P.spaces
            |= dims
            |. P.spaces
            |= arrayInitializer
        ]


type DimExprs
    = DimExprs DimExpr (List DimExpr)


dimExprs : Parser DimExprs
dimExprs =
    P.succeed DimExprs
        |= dimExpr
        |. P.spaces
        |= list dimExpr


type DimExpr
    = DimExpr (List Annotation) Expression


dimExpr : Parser DimExpr
dimExpr =
    P.succeed DimExpr
        |= list annotation
        |. P.spaces
        |. P.symbol "["
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol "]"


type Expression
    = Expression_Lambda LambdaExpression
    | Expression_Assignment AssignmentExpression


expression : Parser Expression
expression =
    P.oneOf
        [ P.succeed Expression_Lambda
            |= lambdaExpression
        , P.succeed Expression_Assignment
            |= assignmentExpression
        ]


type LambdaExpression
    = LambdaExpression LambdaParameters LambdaBody


lambdaExpression : Parser LambdaExpression
lambdaExpression =
    P.succeed LambdaExpression
        |= lambdaParameters
        |. P.spaces
        |. P.symbol "->"
        |. P.spaces
        |= lambdaBody


type LambdaParameters
    = LambdaParameters_List (Maybe LambdaParameterList)
    | LambdaParameters_Identifier Identifier


lambdaParameters : Parser LambdaParameters
lambdaParameters =
    P.oneOf
        [ P.succeed LambdaParameters_List
            |. P.symbol "("
            |. P.spaces
            |= optional lambdaParameterList
            |. P.spaces
            |. P.symbol ")"
        , P.succeed LambdaParameters_Identifier
            |= identifier
        ]


type LambdaParameterList
    = LambdaParameterList_Parameters LambdaParameter (List LambdaParameter)
    | LambdaParameterList_Identifiers Identifier (List Identifier)


lambdaParameterList : Parser LambdaParameterList
lambdaParameterList =
    P.oneOf
        [ P.succeed LambdaParameterList_Parameters
            |= lambdaParameter
            |. P.spaces
            |= list
                (P.succeed identity
                    |. P.symbol ","
                    |. P.spaces
                    |= lambdaParameter
                )
        , P.succeed LambdaParameterList_Identifiers
            |= identifier
            |. P.spaces
            |= list
                (P.succeed identity
                    |. P.symbol ","
                    |. P.spaces
                    |= identifier
                )
        ]


type LambdaParameter
    = LambdaParameter_Normal (List VariableModifier) LambdaParameterType VariableDeclaratorId
    | LambdaParameter_Arity VariableArityParameter


lambdaParameter : Parser LambdaParameter
lambdaParameter =
    P.oneOf
        [ P.succeed LambdaParameter_Normal
            |= list variableModifier
            |. P.spaces
            |= lambdaParameterType
            |. P.spaces
            |= variableDeclaratorId
        , P.succeed LambdaParameter_Arity
            |= variableArityParameter
        ]


type LambdaParameterType
    = LambdaParameterType_Unann UnannType
    | LambdaParameterType_Var


lambdaParameterType : Parser LambdaParameterType
lambdaParameterType =
    P.oneOf
        [ P.succeed LambdaParameterType_Unann
            |= P.lazy (\_ -> unannType)
        , P.succeed LambdaParameterType_Var
            |. P.keyword "var"
        ]


type LambdaBody
    = LambdaBody_Expression Expression
    | LambdaBody_Block Block


lambdaBody : Parser LambdaBody
lambdaBody =
    P.oneOf
        [ P.succeed LambdaBody_Expression
            |= P.lazy (\_ -> expression)
        , P.succeed LambdaBody_Block
            |= P.lazy (\_ -> block)
        ]


type AssignmentExpression
    = AssignmentExpression_Conditional ConditionalExpression
    | AssignmentExpression_Assignment Assignment


assignmentExpression : Parser AssignmentExpression
assignmentExpression =
    P.oneOf
        [ P.succeed AssignmentExpression_Conditional
            |= conditionalExpression
        , P.succeed AssignmentExpression_Assignment
            |= assignment
        ]


type Assignment
    = Assignment LeftHandSide AssignmentOperator Expression


assignment : Parser Assignment
assignment =
    P.succeed Assignment
        |= leftHandSide
        |. P.spaces
        |= assignmentOperator
        |. P.spaces
        |= P.lazy (\_ -> expression)


type LeftHandSide
    = LeftHandSide_Expression ExpressionName
    | LeftHandSide_Field FieldAccess
    | LeftHandSide_Array ArrayAccess


leftHandSide : Parser LeftHandSide
leftHandSide =
    P.oneOf
        [ P.succeed LeftHandSide_Expression
            |= expressionName
        , P.succeed LeftHandSide_Field
            |= fieldAccess
        , P.succeed LeftHandSide_Array
            |= arrayAccess
        ]


type AssignmentOperator
    = AssignmentOperator_Normal
    | AssignmentOperator_Multiply
    | AssignmentOperator_Divide
    | AssignmentOperator_Modulus
    | AssignmentOperator_Add
    | AssignmentOperator_Subtract
    | AssignmentOperator_LeftShift
    | AssignmentOperator_RightShift
    | AssignmentOperator_RightShift3
    | AssignmentOperator_And
    | AssignmentOperator_Xor
    | AssignmentOperator_Or


assignmentOperator : Parser AssignmentOperator
assignmentOperator =
    P.oneOf
        [ P.succeed AssignmentOperator_Normal
            |. P.symbol "="
        , P.succeed AssignmentOperator_Multiply
            |. P.symbol "*="
        , P.succeed AssignmentOperator_Divide
            |. P.symbol "/="
        , P.succeed AssignmentOperator_Modulus
            |. P.symbol "%="
        , P.succeed AssignmentOperator_Add
            |. P.symbol "+="
        , P.succeed AssignmentOperator_Subtract
            |. P.symbol "-="
        , P.succeed AssignmentOperator_LeftShift
            |. P.symbol "<<="
        , P.succeed AssignmentOperator_RightShift3
            |. P.symbol ">>>="
        , P.succeed AssignmentOperator_RightShift
            |. P.symbol ">>="
        , P.succeed AssignmentOperator_And
            |. P.symbol "&="
        , P.succeed AssignmentOperator_Xor
            |. P.symbol "^="
        , P.succeed AssignmentOperator_Or
            |. P.symbol "|="
        ]


type ConditionalExpression
    = ConditionalExpression_Or ConditionalOrExpression
    | ConditionalExpression_TernaryConditional ConditionalOrExpression Expression ConditionalExpression
    | ConditionalExpression_TernaryLambda ConditionalOrExpression Expression LambdaExpression


conditionalExpression : Parser ConditionalExpression
conditionalExpression =
    P.oneOf
        [ P.succeed ConditionalExpression_Or
            |= conditionalOrExpression
        , P.succeed ConditionalExpression_TernaryConditional
            |= conditionalOrExpression
            |. P.spaces
            |. P.symbol "?"
            |. P.spaces
            |= P.lazy (\_ -> expression)
            |. P.spaces
            |. P.symbol ":"
            |. P.spaces
            |= P.lazy (\_ -> conditionalExpression)
        , P.succeed ConditionalExpression_TernaryLambda
            |= conditionalOrExpression
            |. P.spaces
            |. P.symbol "?"
            |. P.spaces
            |= P.lazy (\_ -> expression)
            |. P.spaces
            |. P.symbol ":"
            |. P.spaces
            |= P.lazy (\_ -> lambdaExpression)
        ]


type ConditionalOrExpression
    = ConditionalOrExpression_And ConditionalAndExpression
    | ConditionalOrExpression_Or ConditionalOrExpression ConditionalAndExpression


conditionalOrExpression : Parser ConditionalOrExpression
conditionalOrExpression =
    P.oneOf
        [ P.succeed ConditionalOrExpression_And
            |= conditionalAndExpression
        , P.succeed ConditionalOrExpression_Or
            |= P.lazy (\_ -> conditionalOrExpression)
            |. P.spaces
            |. P.symbol "||"
            |. P.spaces
            |= conditionalAndExpression
        ]


type ConditionalAndExpression
    = ConditionalAndExpression_Or InclusiveOrExpression
    | ConditionalAndExpression_And ConditionalAndExpression InclusiveOrExpression


conditionalAndExpression : Parser ConditionalAndExpression
conditionalAndExpression =
    P.oneOf
        [ P.succeed ConditionalAndExpression_Or
            |= inclusiveOrExpression
        , P.succeed ConditionalAndExpression_And
            |= P.lazy (\_ -> conditionalAndExpression)
            |. P.spaces
            |. P.symbol "&&"
            |. P.spaces
            |= inclusiveOrExpression
        ]


type InclusiveOrExpression
    = InclusiveOrExpression_Xor ExclusiveOrExpression
    | InclusiveOrExpression_Or InclusiveOrExpression ExclusiveOrExpression


inclusiveOrExpression : Parser InclusiveOrExpression
inclusiveOrExpression =
    P.oneOf
        [ P.succeed InclusiveOrExpression_Xor
            |= exclusiveOrExpression
        , P.succeed InclusiveOrExpression_Or
            |= P.lazy (\_ -> inclusiveOrExpression)
            |. P.spaces
            |. P.symbol "|"
            |. P.spaces
            |= exclusiveOrExpression
        ]


type ExclusiveOrExpression
    = ExclusiveOrExpression_And AndExpression
    | ExclusiveOrExpression_Xor ExclusiveOrExpression AndExpression


exclusiveOrExpression : Parser ExclusiveOrExpression
exclusiveOrExpression =
    P.oneOf
        [ P.succeed ExclusiveOrExpression_And
            |= andExpression
        , P.succeed ExclusiveOrExpression_Xor
            |= P.lazy (\_ -> exclusiveOrExpression)
            |. P.spaces
            |. P.symbol "^"
            |. P.spaces
            |= andExpression
        ]


type AndExpression
    = AndExpression_Equality EqualityExpression
    | AndExpression_And AndExpression EqualityExpression


andExpression : Parser AndExpression
andExpression =
    P.oneOf
        [ P.succeed AndExpression_Equality
            |= equalityExpression
        , P.succeed AndExpression_And
            |= P.lazy (\_ -> andExpression)
            |. P.spaces
            |. P.symbol "&"
            |. P.spaces
            |= equalityExpression
        ]


type EqualityExpression
    = EqualityExpression_Relational RelationalExpression
    | EqualityExpression_Equals EqualityExpression RelationalExpression
    | EqualityExpression_NotEquals EqualityExpression RelationalExpression


equalityExpression : Parser EqualityExpression
equalityExpression =
    P.oneOf
        [ P.succeed EqualityExpression_Relational
            |= P.lazy (\_ -> relationalExpression)
        , P.succeed EqualityExpression_Equals
            |= P.lazy (\_ -> equalityExpression)
            |. P.spaces
            |. P.symbol "=="
            |. P.spaces
            |= P.lazy (\_ -> relationalExpression)
        , P.succeed EqualityExpression_NotEquals
            |= P.lazy (\_ -> equalityExpression)
            |. P.spaces
            |. P.symbol "!="
            |. P.spaces
            |= P.lazy (\_ -> relationalExpression)
        ]


type RelationalExpression
    = RelationalExpression_Shift ShiftExpression
    | RelationalExpression_Less RelationalExpression ShiftExpression
    | RelationalExpression_Greater RelationalExpression ShiftExpression
    | RelationalExpression_LessEqual RelationalExpression ShiftExpression
    | RelationalExpression_GreaterEqual RelationalExpression ShiftExpression
    | RelationalExpression_Instanceof RelationalExpression ReferenceType


relationalExpression : Parser RelationalExpression
relationalExpression =
    P.oneOf
        [ P.succeed RelationalExpression_Shift
            |= shiftExpression
        , P.succeed RelationalExpression_Less
            |= P.lazy (\_ -> relationalExpression)
            |. P.spaces
            |. P.symbol "<"
            |. P.spaces
            |= shiftExpression
        , P.succeed RelationalExpression_Greater
            |= P.lazy (\_ -> relationalExpression)
            |. P.spaces
            |. P.symbol ">"
            |. P.spaces
            |= shiftExpression
        , P.succeed RelationalExpression_LessEqual
            |= P.lazy (\_ -> relationalExpression)
            |. P.spaces
            |. P.symbol "<="
            |. P.spaces
            |= shiftExpression
        , P.succeed RelationalExpression_GreaterEqual
            |= P.lazy (\_ -> relationalExpression)
            |. P.spaces
            |. P.symbol ">="
            |. P.spaces
            |= shiftExpression
        , P.succeed RelationalExpression_Instanceof
            |= P.lazy (\_ -> relationalExpression)
            |. P.spaces
            |. P.keyword "instanceof"
            |. P.spaces
            |= referenceType
        ]


type ShiftExpression
    = ShiftExpression_Additive AdditiveExpression
    | ShiftExpression_Left ShiftExpression AdditiveExpression
    | ShiftExpression_Right ShiftExpression AdditiveExpression
    | ShiftExpression_Right2 ShiftExpression AdditiveExpression


shiftExpression : Parser ShiftExpression
shiftExpression =
    P.oneOf
        [ P.succeed ShiftExpression_Additive
            |= additiveExpression
        , P.succeed ShiftExpression_Left
            |= P.lazy (\_ -> shiftExpression)
            |. P.spaces
            |. P.symbol "<<"
            |. P.spaces
            |= additiveExpression
        , P.succeed ShiftExpression_Right
            |= P.lazy (\_ -> shiftExpression)
            |. P.spaces
            |. P.symbol ">>"
            |. P.spaces
            |= additiveExpression
        , P.succeed ShiftExpression_Right2
            |= P.lazy (\_ -> shiftExpression)
            |. P.spaces
            |. P.symbol ">>>"
            |. P.spaces
            |= additiveExpression
        ]


type AdditiveExpression
    = AdditiveExpression_Multiplicative MultiplicativeExpression
    | AdditiveExpression_Plus AdditiveExpression MultiplicativeExpression
    | AdditiveExpression_Minus AdditiveExpression MultiplicativeExpression


additiveExpression : Parser AdditiveExpression
additiveExpression =
    P.oneOf
        [ P.succeed AdditiveExpression_Multiplicative
            |= multiplicativeExpression
        , P.succeed AdditiveExpression_Plus
            |= P.lazy (\_ -> additiveExpression)
            |. P.spaces
            |. P.symbol "+"
            |. P.spaces
            |= multiplicativeExpression
        , P.succeed AdditiveExpression_Minus
            |= P.lazy (\_ -> additiveExpression)
            |. P.spaces
            |. P.symbol "-"
            |. P.spaces
            |= multiplicativeExpression
        ]


type MultiplicativeExpression
    = MultiplicativeExpression_Unary UnaryExpression
    | MultiplicativeExpression_Multiply MultiplicativeExpression UnaryExpression
    | MultiplicativeExpression_Divide MultiplicativeExpression UnaryExpression
    | MultiplicativeExpression_Mod MultiplicativeExpression UnaryExpression


multiplicativeExpression : Parser MultiplicativeExpression
multiplicativeExpression =
    P.oneOf
        [ P.succeed MultiplicativeExpression_Unary
            |= unaryExpression
        , P.succeed MultiplicativeExpression_Multiply
            |= P.lazy (\_ -> multiplicativeExpression)
            |. P.spaces
            |. P.symbol "*"
            |. P.spaces
            |= unaryExpression
        , P.succeed MultiplicativeExpression_Divide
            |= P.lazy (\_ -> multiplicativeExpression)
            |. P.spaces
            |. P.symbol "/"
            |. P.spaces
            |= unaryExpression
        , P.succeed MultiplicativeExpression_Mod
            |= P.lazy (\_ -> multiplicativeExpression)
            |. P.spaces
            |. P.symbol "%"
            |. P.spaces
            |= unaryExpression
        ]


type UnaryExpression
    = UnaryExpression_PreIncrement PreIncrementExpression
    | UnaryExpression_PreDecrement PreDecrementExpression
    | UnaryExpression_Plus UnaryExpression
    | UnaryExpression_Minus UnaryExpression
    | UnaryExpression_NotPlusMinus UnaryExpressionNotPlusMinus


unaryExpression : Parser UnaryExpression
unaryExpression =
    P.oneOf
        [ P.succeed UnaryExpression_PreIncrement
            |= preIncrementExpression
        , P.succeed UnaryExpression_PreDecrement
            |= preDecrementExpression
        , P.succeed UnaryExpression_Plus
            |. P.symbol "+"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpression)
        , P.succeed UnaryExpression_Minus
            |. P.symbol "-"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpression)
        , P.succeed UnaryExpression_NotPlusMinus
            |= unaryExpressionNotPlusMinus
        ]


type PreIncrementExpression
    = PreIncrementExpression UnaryExpression


preIncrementExpression : Parser PreIncrementExpression
preIncrementExpression =
    P.succeed PreIncrementExpression
        |. P.symbol "++"
        |. P.spaces
        |= P.lazy (\_ -> unaryExpression)


type PreDecrementExpression
    = PreDecrementExpression UnaryExpression


preDecrementExpression : Parser PreDecrementExpression
preDecrementExpression =
    P.succeed PreDecrementExpression
        |. P.symbol "--"
        |. P.spaces
        |= P.lazy (\_ -> unaryExpression)


type UnaryExpressionNotPlusMinus
    = UnaryExpressionNotPlusMinus_Postfix PostfixExpression
    | UnaryExpressionNotPlusMinus_BitwiseNot UnaryExpression
    | UnaryExpressionNotPlusMinus_LogicalNot UnaryExpression
    | UnaryExpressionNotPlusMinus_Cast CastExpression
    | UnaryExpressionNotPlusMinus_Switch SwitchExpression


unaryExpressionNotPlusMinus : Parser UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinus =
    P.oneOf
        [ P.succeed UnaryExpressionNotPlusMinus_Postfix
            |= postfixExpression
        , P.succeed UnaryExpressionNotPlusMinus_BitwiseNot
            |. P.symbol "~"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpression)
        , P.succeed UnaryExpressionNotPlusMinus_LogicalNot
            |. P.symbol "!"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpression)
        , P.succeed UnaryExpressionNotPlusMinus_Cast
            |= P.lazy (\_ -> castExpression)
        , P.succeed UnaryExpressionNotPlusMinus_Switch
            |= P.lazy (\_ -> switchExpression)
        ]


type PostfixExpression
    = PostfixExpression_Primary Primary
    | PostfixExpression_Name ExpressionName
    | PostfixExpression_Increment PostIncrementExpression
    | PostfixExpression_Decrement PostDecrementExpression


postfixExpression : Parser PostfixExpression
postfixExpression =
    P.oneOf
        [ P.succeed PostfixExpression_Primary
            |= P.lazy (\_ -> primary)
        , P.succeed PostfixExpression_Name
            |= expressionName
        , P.succeed PostfixExpression_Increment
            |= postIncrementExpression
        , P.succeed PostfixExpression_Decrement
            |= postDecrementExpression
        ]


type PostIncrementExpression
    = PostIncrementExpression PostfixExpression


postIncrementExpression : Parser PostIncrementExpression
postIncrementExpression =
    P.succeed PostIncrementExpression
        |= P.lazy (\_ -> postfixExpression)
        |. P.spaces
        |. P.symbol "++"


type PostDecrementExpression
    = PostDecrementExpression PostfixExpression


postDecrementExpression : Parser PostDecrementExpression
postDecrementExpression =
    P.succeed PostDecrementExpression
        |= P.lazy (\_ -> postfixExpression)
        |. P.spaces
        |. P.symbol "--"


type CastExpression
    = CastExpression_Unary PrimitiveType UnaryExpression
    | CastExpression_UnaryAdditional ReferenceType (List AdditionalBound) UnaryExpressionNotPlusMinus
    | CastExpression_Lambda ReferenceType (List AdditionalBound) LambdaExpression


castExpression : Parser CastExpression
castExpression =
    P.oneOf
        [ P.succeed CastExpression_Unary
            |. P.symbol "("
            |. P.spaces
            |= primitiveType
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpression)
        , P.succeed CastExpression_UnaryAdditional
            |. P.symbol "("
            |. P.spaces
            |= referenceType
            |. P.spaces
            |= list additionalBound
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |= P.lazy (\_ -> unaryExpressionNotPlusMinus)
        , P.succeed CastExpression_Lambda
            |. P.symbol "("
            |. P.spaces
            |= referenceType
            |. P.spaces
            |= list additionalBound
            |. P.spaces
            |. P.symbol ")"
            |. P.spaces
            |= lambdaExpression
        ]


type SwitchExpression
    = SwitchExpression Expression SwitchBlock


switchExpression : Parser SwitchExpression
switchExpression =
    P.succeed SwitchExpression
        |. P.keyword "switch"
        |. P.spaces
        |. P.symbol "("
        |. P.spaces
        |= expression
        |. P.spaces
        |. P.symbol ")"
        |. P.spaces
        |= switchBlock


type ConstantExpression
    = ConstantExpression Expression


constantExpression : Parser ConstantExpression
constantExpression =
    P.succeed ConstantExpression
        |= expression



-- }}}
