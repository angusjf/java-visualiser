module Java15Parser exposing (..)

import Char exposing (Char)
import CustomParser exposing (..)
import Regex
import Set exposing (Set)
import Result



parse : Parser a -> String -> Result.Result String a
parse parser =
    removeCommentsAndTabs
    >> replaceEscapeChars
    >> (kiimap identity parser spaces eof)
    >> Result.map Tuple.first



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



replaceEscapeChars : String -> String
replaceEscapeChars src = 
    let
        re =
            "(\\\\'|\\\\\")"
                |> Regex.fromStringWith
                    { caseInsensitive = False, multiline = True }
                |> Maybe.withDefault Regex.never
    in
    Regex.replace re (always "_") src


-- {{{ Productions from §3 (Lexical Structure)


keywords : List String
keywords =
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
  \str ->
    let
      ident =
          case String.uncons str of
              Nothing -> ""
              Just (c, cs) ->
                if javaLetter c
                    then String.cons c (stringTakeWhile javaLetterOrDigit cs)
                    else ""
    in
      if String.length ident > 0 then
         Ok (Identifier ident, String.dropLeft (String.length ident) str)
      else
         Err str


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
    kmap TypeIdentifier identifier


type UnqualifiedMethodIdentifier
    = UnqualifiedMethodIdentifier Identifier
    


unqualifiedMethodIdentifier : Parser UnqualifiedMethodIdentifier
unqualifiedMethodIdentifier =
    kmap UnqualifiedMethodIdentifier identifier


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
    oneOf
        [ imap Literal_NullLiteral nullLiteral
        , kmap Literal_BooleanLiteral booleanLiteral
        , kmap Literal_CharacterLiteral characterLiteral
        , kmap Literal_TextBlock textBlock
        , kmap Literal_StringLiteral stringLiteral
        , kmap Literal_IntegerLiteral integerLiteral
        , kmap Literal_FloatingPointLiteral floatingPointLiteral
        ]


nullLiteral : Parser ()
nullLiteral = keyword "null"


booleanLiteral : Parser Bool
booleanLiteral =
    oneOf
        [ imap True (keyword "true")
        , imap False (keyword "false")
        ]


characterLiteral : Parser Char
characterLiteral =
    ikimap identity
        (symbol "'")
        (\str -> Result.fromMaybe str (String.uncons str)) -- TODO escaped chars
        (symbol "'")


textBlock : Parser String
textBlock =
    ignorer (succeed "TODO") (keyword "\"\"\"TODO\"\"\"")
        -- TODO


stringLiteral : Parser String
stringLiteral =
    \str ->
        case String.uncons str of
            Just ('"', rest) ->
                let
                    contents = stringTakeWhile (\c -> c /= '"') rest
                    after = String.dropLeft ((String.length contents) + 1) rest
                in
                    Ok (contents, after)
            _ ->
                Err str


integerLiteral : Parser Int
integerLiteral = intParser


floatingPointLiteral : Parser Float
floatingPointLiteral = floatParser


-- }}}
-- {{{ Productions from §4 (Types, Values, and Variables)


type Type
    = Type_PrimitiveType PrimitiveType
    | Type_ReferenceType ReferenceType
    


type_ : Parser Type
type_ =
    oneOf
        [ kmap Type_PrimitiveType primitiveType
        , kmap Type_ReferenceType referenceType
        ]


type PrimitiveType
    = PrimitiveType_Numeric (List Annotation) NumericType
    | PrimitiveType_Boolean (List Annotation)
    


primitiveType : Parser PrimitiveType
primitiveType =
    keeper (
    ignorer (
    keeper (
    succeed (\annotations f -> f annotations) ) <|
    list annotation ) <|
    spaces ) <|
    oneOf
      [ kmap (\num -> \ann -> PrimitiveType_Numeric ann num) numericType
      , imap PrimitiveType_Boolean (keyword "boolean")
      ]


type NumericType
    = NumericType_IntegralType IntegralType
    | NumericType_FloatingPointType FloatingPointType
    


numericType : Parser NumericType
numericType =
    oneOf
        [ kmap NumericType_IntegralType integralType
        , kmap NumericType_FloatingPointType floatingPointType
        ]


type IntegralType
    = IntegralType_Byte
    | IntegralType_Short
    | IntegralType_Int
    | IntegralType_Long
    | IntegralType_Char
    


integralType : Parser IntegralType
integralType =
    oneOf
        [ imap IntegralType_Byte (keyword "byte")
        , imap IntegralType_Short (keyword "short")
        , imap IntegralType_Int (keyword "int")
        , imap IntegralType_Long (keyword "long")
        , imap IntegralType_Char (keyword "char")
        ]


type FloatingPointType
    = FloatingPointType_Float
    | FloatingPointType_Double
    


floatingPointType : Parser FloatingPointType
floatingPointType =
    oneOf
        [ imap FloatingPointType_Float (keyword "float")
        , imap FloatingPointType_Double (keyword "double")
        ]


type ReferenceType
    = ReferenceType_ClassOrInterfaceType ClassOrInterfaceType
    | ReferenceType_TypeVariable TypeVariable
    | ReferenceType_ArrayType ArrayType
    


referenceType : Parser ReferenceType
referenceType =
    lazy (\_ ->
        oneOf
            [ kmap ReferenceType_ClassOrInterfaceType classOrInterfaceType
            , kmap ReferenceType_TypeVariable typeVariable
            , kmap ReferenceType_ArrayType arrayType
            ]
    )


type ClassOrInterfaceType
    = ClassOrInterfaceType_ClassType ClassType
    | ClassOrInterfaceType_InterfaceType InterfaceType
    


classOrInterfaceType : Parser ClassOrInterfaceType
classOrInterfaceType =
    oneOf
        [ kmap ClassOrInterfaceType_ClassType classType
        , kmap ClassOrInterfaceType_InterfaceType interfaceType
        ]


type ClassType
    = ClassType_NoPackage (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | ClassType_Package PackageName (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | ClassType_ClassOrInterfaceType ClassOrInterfaceType (List Annotation) TypeIdentifier (Maybe TypeArguments)
    


classType : Parser ClassType
classType =
    lazy (\_ ->
        oneOf
            [ keeper (
              ignorer (
              keeper (
              ignorer (
              keeper (
              succeed ClassType_NoPackage ) <|
              list annotation ) <|
              spaces ) <|
              typeIdentifier ) <|
              spaces ) <|
              optional typeArguments
            , keeper (
              ignorer (
              keeper (
              ignorer (
              keeper (
              ignorer (
              ignorer (
              ignorer (
              keeper (
              succeed ClassType_Package ) <|
              packageName ) <|
              spaces ) <|
              (symbol ".") ) <|
              spaces ) <|
              list annotation ) <|
              spaces ) <|
              typeIdentifier ) <|
              spaces ) <|
              optional typeArguments
            , keeper (
              ignorer (
              keeper (
              ignorer (
              keeper (
              ignorer (
              ignorer (
              ignorer (
              keeper (
              succeed ClassType_ClassOrInterfaceType ) <|
              classOrInterfaceType ) <|
              spaces ) <|
              (symbol ".") ) <|
              spaces ) <|
              list annotation ) <|
              spaces ) <|
              typeIdentifier ) <|
              spaces ) <|
              optional typeArguments
            ]
    )


type InterfaceType
    = InterfaceType_ClassType ClassType
    


interfaceType : Parser InterfaceType
interfaceType =
    kmap InterfaceType_ClassType classType


type TypeVariable
    = TypeVariable (List Annotation) TypeIdentifier
    


typeVariable : Parser TypeVariable
typeVariable =
    keeper (
    ignorer (
    keeper (
    succeed TypeVariable ) <|
    list annotation ) <|
    spaces ) <|
    typeIdentifier


type ArrayType
    = ArrayType_PrimitiveType PrimitiveType Dims
    | ArrayType_ClassOrInterfaceType ClassOrInterfaceType Dims
    | ArrayType_TypeVariable TypeVariable Dims
    


arrayType : Parser ArrayType
arrayType =
    oneOf
        [ keeper (
          ignorer (
          keeper (
          succeed ArrayType_PrimitiveType ) <|
          primitiveType ) <|
          spaces ) <|
          dims
        , keeper (
          ignorer (
          keeper (
          succeed ArrayType_ClassOrInterfaceType ) <|
          classOrInterfaceType ) <|
          spaces ) <|
          dims
        , keeper (
          ignorer (
          keeper (
          succeed ArrayType_TypeVariable ) <|
          typeVariable ) <|
          spaces ) <|
          dims
        ]


type Dims
    = Dims (List (List Annotation))
    


dims : Parser Dims
dims =
    kmap Dims
        ( list
            (
                ignorer (
                ignorer (
                ignorer (
                ignorer (
                list (annotation) ) <|
                spaces ) <|
                (symbol "[") ) <|
                spaces ) <|
                (symbol "]")
            )
          )


type TypeParameter
    = TypeParameter (List TypeParameterModifier) TypeIdentifier (Maybe TypeBound)
    


typeParameter : Parser TypeParameter
typeParameter =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        succeed TypeParameter ) <|
        list typeParameterModifier ) <|
        spaces ) <|
        typeIdentifier ) <|
        spaces ) <|
        optional typeBound


type TypeParameterModifier
    = TypeParameterModifier Annotation
    


typeParameterModifier : Parser TypeParameterModifier
typeParameterModifier =
    kmap TypeParameterModifier annotation


type TypeBound
    = TypeBound_TypeVariable TypeVariable
    | TypeBound_ClassOrInterfaceType ClassOrInterfaceType (List AdditionalBound)
    


typeBound : Parser TypeBound
typeBound =
    keeper (
    ignorer (
    ignorer (
    succeed identity ) <|
    (keyword "extends") ) <|
    spaces ) <|
    oneOf
            [ kmap TypeBound_TypeVariable typeVariable
            , kikmap TypeBound_ClassOrInterfaceType
                classOrInterfaceType
                spaces
                (list additionalBound)
            ]


type AdditionalBound
    = AdditionalBound InterfaceType
    


additionalBound : Parser AdditionalBound
additionalBound =
    iikmap AdditionalBound
        (symbol "&")
        spaces
        interfaceType


type TypeArguments
    = TypeArguments_Brackets TypeArgumentList
    


typeArguments : Parser TypeArguments
typeArguments =
    ikimap TypeArguments_Brackets
        (symbol "<")
        typeArgumentList
        (symbol ">")


type TypeArgumentList
    = TypeArgumentList (List TypeArgument)
    


typeArgumentList : Parser TypeArgumentList
typeArgumentList =
    kmap TypeArgumentList
        (nonEmptySep "," typeArgument)


type TypeArgument
    = TypeArgument_ReferenceType ReferenceType
    | TypeArgument_Wildcard Wildcard
    


typeArgument : Parser TypeArgument
typeArgument =
    oneOf
        [ kmap TypeArgument_ReferenceType referenceType
        , kmap TypeArgument_Wildcard wildcard
        ]


type Wildcard
    = Wildcard (List Annotation) (Maybe WildcardBounds)
    


wildcard : Parser Wildcard
wildcard =
    lazy (\_ ->
        kiikmap Wildcard
            (list annotation)
            spaces
            (symbol "?")
            (optional wildcardBounds)
        )


type WildcardBounds
    = WildcardBounds_Extends ReferenceType
    | WildcardBounds_Super ReferenceType
    


wildcardBounds : Parser WildcardBounds
wildcardBounds =
    oneOf
        [ ikmap WildcardBounds_Extends
            (keyword "extends")
            referenceType
        , ikmap WildcardBounds_Super
            (keyword "super")
            referenceType
        ]



-- }}}
-- {{{ Productions from §6 (Names)


type ModuleName
    = ModuleName (List Identifier)
    


moduleName : Parser ModuleName
moduleName =
    kmap ModuleName (dotted identifier)


type PackageName
    = PackageName (List Identifier)
    


packageName : Parser PackageName
packageName =
    kmap PackageName (dotted identifier)


type TypeName
    = TypeName (List TypeIdentifier)
    


typeName : Parser TypeName
typeName =
    kmap TypeName (dotted typeIdentifier)


type ExpressionName
    = ExpressionName (List Identifier)
    


expressionName : Parser ExpressionName
expressionName =
    kmap ExpressionName (dotted identifier)


type MethodName
    = MethodName UnqualifiedMethodIdentifier
    


methodName : Parser MethodName
methodName =
    kmap MethodName unqualifiedMethodIdentifier


type PackageOrTypeName
    = PackageOrTypeName (List Identifier)
    


packageOrTypeName : Parser PackageOrTypeName
packageOrTypeName =
    kmap PackageOrTypeName (dotted identifier)


-- }}}
-- {{{ Productions from §7 (Packages and Modules)


type CompilationUnit
    = CompilationUnit_Ordinary OrdinaryCompilationUnit
    | CompilationUnit_Modular ModularCompilationUnit
    


compilationUnit : Parser CompilationUnit
compilationUnit =
    oneOf
        [ kmap CompilationUnit_Ordinary ordinaryCompilationUnit
        , kmap CompilationUnit_Modular modularCompilationUnit
        ]


type OrdinaryCompilationUnit
    = OrdinaryCompilationUnit (Maybe PackageDeclaration) (List ImportDeclaration) (List TypeDeclaration)
    


ordinaryCompilationUnit : Parser OrdinaryCompilationUnit
ordinaryCompilationUnit =
    kikikmap OrdinaryCompilationUnit
        (optional packageDeclaration)
        spaces
        (list importDeclaration)
        spaces
        (list typeDeclaration)


type ModularCompilationUnit
    = ModularCompilationUnit (List ImportDeclaration) ModuleDeclaration
    


modularCompilationUnit : Parser ModularCompilationUnit
modularCompilationUnit =
    kikmap ModularCompilationUnit
        (list importDeclaration)
        spaces
        (moduleDeclaration)


type PackageDeclaration
    = PackageDeclaration (List PackageModifier) (List Identifier)
    


packageDeclaration : Parser PackageDeclaration
packageDeclaration =
    kiiikiimap PackageDeclaration
        (list packageModifier)
        spaces
        (keyword "package")
        spaces
        (nonEmptySep "." identifier)
        spaces
        (symbol ";")


type PackageModifier
    = PackageModifier Annotation
    


packageModifier : Parser PackageModifier
packageModifier =
    kmap PackageModifier annotation


type ImportDeclaration
    = ImportDeclaration_SingleTypeImport SingleTypeImportDeclaration
    | ImportDeclaration_TypeImportOnDemand TypeImportOnDemandDeclaration
    | ImportDeclaration_SingleStaticImport SingleStaticImportDeclaration
    | ImportDeclaration_StaticImportOnDemand StaticImportOnDemandDeclaration
    


importDeclaration : Parser ImportDeclaration
importDeclaration =
    oneOf
        [ kmap ImportDeclaration_SingleTypeImport singleTypeImportDeclaration
        , kmap ImportDeclaration_TypeImportOnDemand typeImportOnDemandDeclaration
        , kmap ImportDeclaration_SingleStaticImport singleStaticImportDeclaration
        , kmap ImportDeclaration_StaticImportOnDemand staticImportOnDemandDeclaration
        ]


type SingleTypeImportDeclaration
    = SingleTypeImportDeclaration TypeName
    


singleTypeImportDeclaration : Parser SingleTypeImportDeclaration
singleTypeImportDeclaration =
    iikiimap SingleTypeImportDeclaration
        (keyword "import")
        spaces
        typeName
        spaces
        (symbol ";")


type TypeImportOnDemandDeclaration
    = TypeImportOnDemandDeclaration PackageOrTypeName
    


typeImportOnDemandDeclaration : Parser TypeImportOnDemandDeclaration
typeImportOnDemandDeclaration =
    iikiiiiimap TypeImportOnDemandDeclaration
        (keyword "import")
        spaces
        packageOrTypeName
        (symbol ".")
        spaces
        (symbol "*")
        spaces
        (symbol ";")


type SingleStaticImportDeclaration
    --= SingleStaticImportDeclaration TypeName Identifier
    = SingleStaticImportDeclaration PackageOrTypeName
    


singleStaticImportDeclaration : Parser SingleStaticImportDeclaration
singleStaticImportDeclaration =
    iiiikiimap SingleStaticImportDeclaration
        (keyword "import")
        spaces
        (keyword "static")
        spaces
        packageOrTypeName
        spaces
        (symbol ";")


type StaticImportOnDemandDeclaration
    = StaticImportOnDemandDeclaration TypeName
    


staticImportOnDemandDeclaration : Parser StaticImportOnDemandDeclaration
staticImportOnDemandDeclaration =
    iiiikiiiiiimap StaticImportOnDemandDeclaration
        (keyword "import")
        spaces
        (keyword "static")
        spaces
        typeName
        spaces
        (symbol ".")
        spaces
        (symbol "*")
        spaces
        (symbol ";")


type TypeDeclaration
    = TypeDeclaration_ClassDeclaration ClassDeclaration
    | TypeDeclaration_InterfaceDeclaration InterfaceDeclaration
    | TypeDeclaration_Semi
    


typeDeclaration : Parser TypeDeclaration
typeDeclaration =
    oneOf
        [ kmap TypeDeclaration_ClassDeclaration classDeclaration
        , kmap TypeDeclaration_InterfaceDeclaration interfaceDeclaration
        , imap TypeDeclaration_Semi (symbol ";")
        ]


type ModuleDeclaration
    = ModuleDeclaration (List Annotation) (Maybe ()) (List Identifier) (List ModuleDirective)
    


moduleDeclaration : Parser ModuleDeclaration
moduleDeclaration =
    kikiiikiiikiimap ModuleDeclaration
        (list annotation)
        spaces
        (optional (keyword "open"))
        spaces
        (keyword "module ")
        spaces
        (nonEmptySep "." identifier)
        spaces
        (symbol "{")
        spaces
        (list moduleDirective)
        spaces
        (symbol "}") 


type ModuleDirective
    = ModuleDirective_Requires (List RequiresModifier) ModuleName
    | ModuleDirective_Exports PackageName (Maybe (List ModuleName))
    | ModuleDirective_Opens PackageName (Maybe (List ModuleName))
    | ModuleDirective_Uses TypeName
    | ModuleDirective_Provides TypeName (List TypeName)
    


moduleDirective : Parser ModuleDirective
moduleDirective =
    oneOf
        [ iikikiimap ModuleDirective_Requires
            (keyword "requires")
            spaces
            (list requiresModifier)
            spaces
            moduleName
            spaces
            (symbol ";")
        , iikikiimap ModuleDirective_Exports
            (keyword "exports")
            spaces
            packageName
            spaces
            (optional
                ( iikmap identity
                        (keyword "to")
                        spaces
                        (nonEmptySep "," moduleName)
                 )
            )
            spaces
            (symbol ";")
        , iikikiimap ModuleDirective_Opens
            (keyword "opens")
            spaces
            packageName
            spaces
            (optional
              ( iikmap identity
                        (keyword "to")
                        spaces
                        (nonEmptySep "," moduleName)
              )
            )
            spaces
            (symbol ";")
        , iikiimap ModuleDirective_Uses
            (keyword "uses")
            spaces
            typeName
            spaces
            (symbol ";")
        , iikiiikiimap ModuleDirective_Provides
            (keyword "provides")
            spaces
            typeName
            spaces
            (keyword "with")
            spaces
            (nonEmptySep "," typeName)
            spaces
            (symbol ";")
        ]


type RequiresModifier
    = RequiresModifier_Transitive
    | RequiresModifier_Static
    


requiresModifier : Parser RequiresModifier
requiresModifier =
    oneOf
        [ imap RequiresModifier_Transitive (keyword "transitive")
        , imap RequiresModifier_Static (keyword "static")
        ]


-- }}}
-- {{{ Productions from §8 (Classes)


type ClassDeclaration
    = ClassDeclaration_Normal NormalClassDeclaration
    | ClassDeclaration_Enum EnumDeclaration
    


classDeclaration : Parser ClassDeclaration
classDeclaration =
    oneOf
        [ kmap ClassDeclaration_Normal normalClassDeclaration
        , kmap ClassDeclaration_Enum enumDeclaration
        ]


type NormalClassDeclaration
    = NormalClassDeclaration (List ClassModifier) TypeIdentifier (Maybe TypeParameters) (Maybe Superclass) (Maybe Superinterfaces) ClassBody
    


normalClassDeclaration : Parser NormalClassDeclaration
normalClassDeclaration =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        succeed NormalClassDeclaration ) <|
        list classModifier ) <|
        spaces ) <|
        (keyword "class") ) <|
        spaces ) <|
        typeIdentifier ) <|
        spaces ) <|
        optional typeParameters ) <|
        spaces ) <|
        optional superclass ) <|
        spaces ) <|
        optional superinterfaces ) <|
        spaces ) <|
        classBody

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
    oneOf
        [ kmap ClassModifier_Annotation annotation
        , imap ClassModifier_Public (keyword "public")
        , imap ClassModifier_Protected (keyword "protected")
        , imap ClassModifier_Private (keyword "private")
        , imap ClassModifier_Abstract (keyword "abstract")
        , imap ClassModifier_Static (keyword "static")
        , imap ClassModifier_Final (keyword "final")
        , imap ClassModifier_StrictFp (keyword "strictfp")
        ]


type TypeParameters
    = TypeParameters TypeParameterList
    


typeParameters : Parser TypeParameters
typeParameters =
    iikiimap TypeParameters
        (symbol "<")
        spaces
        typeParameterList
        spaces
        (symbol ">")


type TypeParameterList
    = TypeParameterList (List TypeParameter)
    


typeParameterList : Parser TypeParameterList
typeParameterList =
    kmap TypeParameterList (nonEmptySep "," typeParameter)


type Superclass
    = Superclass ClassType
    


superclass : Parser Superclass
superclass =
    iikmap Superclass
        (keyword "extends")
        spaces
        classType


type Superinterfaces
    = Superinterfaces InterfaceTypeList
    


superinterfaces : Parser Superinterfaces
superinterfaces =
    iikmap Superinterfaces
        (keyword "implements")
        spaces
        interfaceTypeList


type InterfaceTypeList
    = InterfaceTypeList (List InterfaceType)
    


interfaceTypeList : Parser InterfaceTypeList
interfaceTypeList =
    kmap InterfaceTypeList
        (nonEmptySep "," interfaceType)


type ClassBody
    = ClassBody (List ClassBodyDeclaration)
    


classBody : Parser ClassBody
classBody =
    lazy (\_ ->
        iikiimap ClassBody
            (symbol "{")
            spaces
            (list classBodyDeclaration)
            spaces
            (symbol "}")
    )


type ClassBodyDeclaration
    = ClassBodyDeclaration_ClassMemberDeclaration ClassMemberDeclaration
    | ClassBodyDeclaration_InstanceInitializer InstanceInitializer
    | ClassBodyDeclaration_StaticInitializer StaticInitializer
    | ClassBodyDeclaration_ConstructorDeclaration ConstructorDeclaration
    


classBodyDeclaration : Parser ClassBodyDeclaration
classBodyDeclaration =
    oneOf
        [ kmap ClassBodyDeclaration_ClassMemberDeclaration
            classMemberDeclaration
        , kmap ClassBodyDeclaration_InstanceInitializer
            instanceInitializer
        , kmap ClassBodyDeclaration_StaticInitializer
            staticInitializer
        , kmap ClassBodyDeclaration_ConstructorDeclaration
            constructorDeclaration
        ]


type ClassMemberDeclaration
    = ClassMemberDeclaration_Field FieldDeclaration
    | ClassMemberDeclaration_Method MethodDeclaration
    | ClassMemberDeclaration_Class ClassDeclaration
    | ClassMemberDeclaration_Interface InterfaceDeclaration
    | ClassMemberDeclaration_Semi
    


classMemberDeclaration : Parser ClassMemberDeclaration
classMemberDeclaration =
    oneOf
        [ kmap ClassMemberDeclaration_Field fieldDeclaration
        , kmap ClassMemberDeclaration_Method methodDeclaration
        , kmap ClassMemberDeclaration_Class classDeclaration
        , kmap ClassMemberDeclaration_Interface interfaceDeclaration
        , imap ClassMemberDeclaration_Semi (symbol ";")
        ]


type FieldDeclaration
    = FieldDeclaration (List FieldModifier) UnannType VariableDeclaratorList
    


fieldDeclaration : Parser FieldDeclaration
fieldDeclaration =
    kikikiimap FieldDeclaration
        (list fieldModifier)
        spaces
        unannType
        spaces
        variableDeclaratorList
        spaces
        (symbol ";")


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
    oneOf
        [ kmap FieldModifier_Annotation annotation
        , imap FieldModifier_Public (keyword "public")
        , imap FieldModifier_Protected (keyword "protected")
        , imap FieldModifier_Private (keyword "private")
        , imap FieldModifier_Static (keyword "static")
        , imap FieldModifier_Final (keyword "final")
        , imap FieldModifier_Transient (keyword "transient")
        , imap FieldModifier_Volatile (keyword "volatile")
        ]


type VariableDeclaratorList
    = VariableDeclaratorList (List VariableDeclarator)
    


variableDeclaratorList : Parser VariableDeclaratorList
variableDeclaratorList =
    kmap VariableDeclaratorList
        (nonEmptySep "," variableDeclarator)


type VariableDeclarator
    = VariableDeclarator VariableDeclaratorId (Maybe VariableInitializer)
    


variableDeclarator : Parser VariableDeclarator
variableDeclarator =
    kikmap VariableDeclarator
        variableDeclaratorId
        spaces
        ( optional
            (iikmap identity
                (symbol "=")
                spaces
                variableInitializer
            )
        )


type VariableDeclaratorId
    = VariableDeclaratorId Identifier (Maybe Dims)
    


variableDeclaratorId : Parser VariableDeclaratorId
variableDeclaratorId =
    kikmap VariableDeclaratorId
        identifier
        spaces
        (optional dims)


type VariableInitializer
    = VariableInitializer_Expression Expression
    | VariableInitializer_ArrayInitializer ArrayInitializer
    


variableInitializer : Parser VariableInitializer
variableInitializer =
    lazy (\_ ->
        oneOf
            [ kmap VariableInitializer_Expression expression
            , kmap VariableInitializer_ArrayInitializer arrayInitializer
            ]
    )


type UnannType
    = UnannType_Primitive UnannPrimitiveType
    | UnannType_Reference UnannReferenceType
    


unannType : Parser UnannType
unannType =
    oneOf
        [ kmap UnannType_Primitive unannPrimitiveType
        , kmap UnannType_Reference unannReferenceType
        ]


type UnannPrimitiveType
    = UnannPrimitiveType_Numeric NumericType
    | UnannPrimitiveType_Boolean
    


unannPrimitiveType : Parser UnannPrimitiveType
unannPrimitiveType =
    oneOf
        [ kmap UnannPrimitiveType_Numeric numericType
        , imap UnannPrimitiveType_Boolean (keyword "boolean")
        ]


type UnannReferenceType
    = UnannReferenceType_Class UnannClassOrInterfaceType
    | UnannReferenceType_TypeVariable UnannTypeVariable
    | UnannReferenceType_Array UnannArrayType
    


unannReferenceType : Parser UnannReferenceType
unannReferenceType =
    oneOf
        [ kmap UnannReferenceType_Class unannClassOrInterfaceType
        , kmap UnannReferenceType_TypeVariable unannTypeVariable
        , kmap UnannReferenceType_Array unannArrayType
        ]


type UnannClassOrInterfaceType
    = UnannClassOrInterfaceType_Class UnannClassType
    | UnannClassOrInterfaceType_Interface UnannInterfaceType
    


unannClassOrInterfaceType : Parser UnannClassOrInterfaceType
unannClassOrInterfaceType =
    oneOf
        [ kmap UnannClassOrInterfaceType_Class unannClassType
        , kmap UnannClassOrInterfaceType_Interface unannInterfaceType
        ]


type UnannClassType
    = UnannClassType_TypeIdentifer TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Package PackageName (List Annotation) TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Class UnannClassOrInterfaceType (List Annotation) TypeIdentifier (Maybe TypeArguments)
    


unannClassType : Parser UnannClassType
unannClassType =
    lazy (\_ ->
        oneOf
            [ kikmap UnannClassType_TypeIdentifer
                typeIdentifier
                spaces
                (optional typeArguments)
            , kiiikikikmap UnannClassType_Package
                packageName
                spaces
                (symbol ".")
                spaces
                (list annotation)
                spaces
                typeIdentifier
                spaces
                (optional typeArguments)
            --, succeed UnannClassType_Class TODO
            --    |= unannClassOrInterfaceType
            --    |. spaces
            --    |. (symbol ".")
            --    |. spaces
            --    |= list annotation
            --    |. spaces
            --    |= typeIdentifier
            --    |. spaces
            --    |= optional typeArguments
            ]
    )


type UnannInterfaceType
    = UnannInterfaceType UnannClassType
    


unannInterfaceType : Parser UnannInterfaceType
unannInterfaceType =
    kmap UnannInterfaceType unannClassType


type UnannTypeVariable
    = UnannTypeVariable TypeIdentifier
    


unannTypeVariable : Parser UnannTypeVariable
unannTypeVariable =
    kmap UnannTypeVariable typeIdentifier


type UnannArrayType
    = UnannArrayType_Primitive UnannPrimitiveType Dims
    | UnannArrayType_Class UnannClassOrInterfaceType Dims
    | UnannArrayType_TypeVariable UnannTypeVariable Dims
    


unannArrayType : Parser UnannArrayType
unannArrayType =
    oneOf
        [ kikmap UnannArrayType_Primitive
            unannPrimitiveType
            spaces
            dims
        , kikmap UnannArrayType_Class
            unannClassOrInterfaceType
            spaces
            dims
        , kikmap UnannArrayType_TypeVariable
            unannTypeVariable
            spaces
            dims
        ]


type MethodDeclaration
    = MethodDeclaration (List MethodModifier) MethodHeader MethodBody
    


methodDeclaration : Parser MethodDeclaration
methodDeclaration =
    kikikmap MethodDeclaration
        (list methodModifier)
        spaces
        methodHeader
        spaces
        methodBody


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
    oneOf
        [ kmap MethodModifier_Annotation annotation
        , imap MethodModifier_Public (keyword "public")
        , imap MethodModifier_Protected (keyword "protected")
        , imap MethodModifier_Private (keyword "private")
        , imap MethodModifier_Abstract (keyword "abstract")
        , imap MethodModifier_Static (keyword "static")
        , imap MethodModifier_Final (keyword "final")
        , imap MethodModifier_Synchronized (keyword "synchronized")
        , imap MethodModifier_Native (keyword "native")
        , imap MethodModifier_Static (keyword "strictfp")
        ]


type MethodHeader
    = MethodHeader_Result Result MethodDeclarator (Maybe Throws)
    | MethodHeader_TypeParameters TypeParameters (List Annotation) Result MethodDeclarator (Maybe Throws)
    


methodHeader : Parser MethodHeader
methodHeader =
    oneOf
        [ kikikmap MethodHeader_Result
            result
            spaces
            methodDeclarator
            spaces
            (optional throws)
        , kikikikikmap MethodHeader_TypeParameters
            typeParameters
            spaces
            (list annotation)
            spaces
            result
            spaces
            methodDeclarator
            spaces
            (optional throws)
        ]


type Result
    = Result_UnannType UnannType
    | Result_Void
    


result : Parser Result
result =
    oneOf
        [ kmap Result_UnannType unannType
        , imap Result_Void (keyword "void")
        ]


type MethodDeclarator
    = MethodDeclarator Identifier (Maybe ReceiverParameter) (Maybe FormalParameterList) (Maybe Dims)
    


methodDeclarator : Parser MethodDeclarator
methodDeclarator =
    kiikkiikmap MethodDeclarator
        identifier
        spaces
        (symbol "(")
        ( optional
            (kimap identity
                receiverParameter
                (symbol ",")
            )
        )
        (optional formalParameterList)
        (symbol ")")
        spaces
        (optional dims)


type ReceiverParameter
    = ReceiverParameter (List Annotation) UnannType (Maybe Identifier)
    


receiverParameter : Parser ReceiverParameter
receiverParameter =
    kikikiimap ReceiverParameter
        (list annotation)
        spaces
        unannType
        spaces
        ( optional
         ( kimap identity
             identifier
             (symbol ".")
         )
        )
        spaces
        (keyword "this")


type FormalParameterList
    = FormalParameterList (List FormalParameter)
    


formalParameterList : Parser FormalParameterList
formalParameterList =
    kmap FormalParameterList
        (nonEmptySep "," formalParameter)


type FormalParameter
    = FormalParameter_Normal (List VariableModifier) UnannType VariableDeclaratorId
    | FormalParameter_Arity VariableArityParameter
    


formalParameter : Parser FormalParameter
formalParameter =
    oneOf
        [ kikikmap FormalParameter_Normal
            (list variableModifier)
            spaces
            unannType
            spaces
            variableDeclaratorId
        , kmap FormalParameter_Arity
            variableArityParameter
        ]


type VariableArityParameter
    = VariableArityParameter (List VariableModifier) UnannType (List Annotation) Identifier
    


variableArityParameter : Parser VariableArityParameter
variableArityParameter =
    kikikiiikmap VariableArityParameter
        (list variableModifier)
        spaces
        unannType
        spaces
        (list annotation)
        spaces
        (keyword "...")
        spaces
        identifier


type VariableModifier
    = VariableModifier_Annotation Annotation
    | VariableModifier_Final
    


variableModifier : Parser VariableModifier
variableModifier =
    oneOf
        [ kmap VariableModifier_Annotation annotation
        , imap VariableModifier_Final (keyword "final")
        ]


type Throws
    = Throws ExceptionTypeList
    


throws : Parser Throws
throws =
    iikmap Throws
        (keyword "throws")
        spaces
        exceptionTypeList


type ExceptionTypeList
    = ExceptionTypeList (List ExceptionType)
    


exceptionTypeList : Parser ExceptionTypeList
exceptionTypeList =
    kmap ExceptionTypeList
        (nonEmptySep "," exceptionType)


type ExceptionType
    = ExceptionType_Class ClassType
    | ExceptionType_TypeVariable TypeVariable
    


exceptionType : Parser ExceptionType
exceptionType =
    oneOf
        [ kmap ExceptionType_Class classType
        , kmap ExceptionType_TypeVariable typeVariable
        ]


type MethodBody
    = MethodBody_Block Block
    | MethodBody_Semi
    


methodBody : Parser MethodBody
methodBody =
    oneOf
        [ kmap MethodBody_Block block
        , imap MethodBody_Semi (symbol ";")
        ]


type InstanceInitializer
    = InstanceInitializer Block
    


instanceInitializer : Parser InstanceInitializer
instanceInitializer =
    kmap InstanceInitializer block


type StaticInitializer
    = StaticInitializer Block
    


staticInitializer : Parser StaticInitializer
staticInitializer =
    iikmap StaticInitializer
        (keyword "static")
        spaces
        block


type ConstructorDeclaration
    = ConstructorDeclaration (List ConstructorModifier) ConstructorDeclarator (Maybe Throws) ConstructorBody
    


constructorDeclaration : Parser ConstructorDeclaration
constructorDeclaration =
    kikikikmap ConstructorDeclaration
        (list constructorModifier)
        spaces
        constructorDeclarator
        spaces
        (optional throws)
        spaces
        constructorBody


type ConstructorModifier
    = ConstructorModifier_Annotation Annotation
    | ConstructorModifier_Public
    | ConstructorModifier_Protected
    | ConstructorModifier_Private
    


constructorModifier : Parser ConstructorModifier
constructorModifier =
    oneOf
        [ kmap ConstructorModifier_Annotation annotation
        , imap ConstructorModifier_Public (keyword "public")
        , imap ConstructorModifier_Protected (keyword "protected")
        , imap ConstructorModifier_Private (keyword "private")
        ]


type ConstructorDeclarator
    = ConstructorDeclarator (Maybe TypeParameters) SimpleTypeName (Maybe ReceiverParameter) (Maybe FormalParameterList)
    


constructorDeclarator : Parser ConstructorDeclarator
constructorDeclarator =
    kikiiikikiimap ConstructorDeclarator
        (optional typeParameters)
        spaces
        simpleTypeName
        spaces
        (symbol "(")
        spaces
        (optional
         (kiimap identity
             receiverParameter
             spaces
             (symbol ",")
         )
        )
        spaces
        (optional formalParameterList)
        spaces
        (symbol ")")


type SimpleTypeName
    = SimpleTypeName TypeIdentifier
    


simpleTypeName : Parser SimpleTypeName
simpleTypeName =
    kmap SimpleTypeName typeIdentifier


type ConstructorBody
    = ConstructorBody (Maybe ExplicitConstructorInvocation) (Maybe BlockStatements)
    


constructorBody : Parser ConstructorBody
constructorBody =
    iikikiimap ConstructorBody
        (symbol "{")
        spaces
        (optional explicitConstructorInvocation)
        spaces
        (optional blockStatements)
        spaces
        (symbol "}")


type ExplicitConstructorInvocation
    = ExplicitConstructorInvocation_This (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_Super (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_ExpressionSuper ExpressionName (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_PrimarySuper Primary (Maybe TypeArguments) (Maybe ArgumentList)
    


explicitConstructorInvocation : Parser ExplicitConstructorInvocation
explicitConstructorInvocation =
    oneOf
        [ ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed ExplicitConstructorInvocation_This ) <|
          (optional typeArguments) ) <|
          spaces ) <|
          (keyword "this") ) <|
          spaces ) <|
          (symbol "(") ) <|
          spaces ) <|
          (optional argumentList) ) <|
          spaces ) <|
          (symbol ")") ) <|
          spaces ) <|
          (symbol ";")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper ( 
          succeed ExplicitConstructorInvocation_Super ) <|
          (optional typeArguments) ) <|
          spaces ) <|
          (keyword "super") ) <|
          spaces ) <|
          (symbol "(") ) <|
          spaces ) <|
          (optional argumentList) ) <|
          spaces ) <|
          (symbol ")") ) <|
          spaces ) <|
          (symbol ";")
        , ignorer ( 
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed ExplicitConstructorInvocation_ExpressionSuper ) <|
          expressionName ) <|
          spaces ) <|
          keyword "." ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          (keyword "super") ) <|
          spaces ) <|
          (symbol "(") ) <|
          spaces ) <|
          optional argumentList ) <|
          spaces ) <|
          (symbol ")") ) <|
          spaces ) <|
          (symbol ";")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed ExplicitConstructorInvocation_PrimarySuper ) <|
          primary ) <|
          spaces ) <|
          keyword "." ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          (keyword "super") ) <|
          spaces ) <|
          (symbol "(") ) <|
          spaces ) <|
          optional argumentList ) <|
          spaces ) <|
          (symbol ")") ) <|
          spaces ) <|
          (symbol ";")
        ]


type EnumDeclaration
    = EnumDeclaration (List ClassModifier) TypeIdentifier (Maybe Superinterfaces) EnumBody
    


enumDeclaration : Parser EnumDeclaration
enumDeclaration =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        succeed EnumDeclaration ) <|
        list classModifier ) <|
        spaces ) <|
        (keyword "enum") ) <|
        spaces ) <|
        typeIdentifier ) <|
        spaces ) <|
        optional superinterfaces ) <|
        spaces ) <|
        enumBody


type EnumBody
    = EnumBody (Maybe EnumConstantList) (Maybe EnumBodyDeclarations)
    


enumBody : Parser EnumBody
enumBody =
    lazy (\_ ->
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed EnumBody ) <|
            (symbol "{") ) <|
            spaces ) <|
            optional enumConstantList ) <|
            spaces ) <|
            (symbol ",") ) <|
            spaces ) <|
            optional enumBodyDeclarations ) <|
            spaces ) <|
            (symbol "}")
    )


type EnumConstantList
    = EnumConstantList EnumConstant (List EnumConstant)
    


enumConstantList : Parser EnumConstantList
enumConstantList =
    kikmap EnumConstantList
        enumConstant
        spaces
        ( list
            ( iikmap identity
                (symbol ",")
                spaces
                enumConstant
            )
        )


type EnumConstant
    = EnumConstant (List EnumConstantModifier) Identifier (Maybe (Maybe ArgumentList)) (Maybe ClassBody)
    


enumConstant =
    kikikikmap EnumConstant
        (list enumConstantModifier)
        spaces
        identifier
        spaces
        (optional
         (iikiimap identity
             (symbol "(")
             spaces
             (optional argumentList)
             spaces
             (symbol ")")
         )
        )
        spaces
        (optional (classBody))


type EnumConstantModifier
    = EnumConstantModifier Annotation
    


enumConstantModifier : Parser EnumConstantModifier
enumConstantModifier =
    kmap EnumConstantModifier
         annotation


type EnumBodyDeclarations
    = EnumBodyDeclarations (List ClassBodyDeclaration)
    


enumBodyDeclarations : Parser EnumBodyDeclarations
enumBodyDeclarations =
    iikmap EnumBodyDeclarations
        (symbol ";")
        spaces
        (list classBodyDeclaration)



-- }}}
-- {{{ Productions from §9 (Interfaces)


type InterfaceDeclaration
    = InterfaceDeclaration_Normal NormalInterfaceDeclaration
    | InterfaceDeclaration_Annotation AnnotationTypeDeclaration
    


interfaceDeclaration : Parser InterfaceDeclaration
interfaceDeclaration =
    oneOf
        [ kmap InterfaceDeclaration_Normal normalInterfaceDeclaration
        , kmap InterfaceDeclaration_Annotation annotationTypeDeclaration
        ]


type NormalInterfaceDeclaration
    = NormalInterfaceDeclaration (List InterfaceModifier) TypeIdentifier (Maybe TypeParameters) (Maybe ExtendsInterfaces) InterfaceBody
    


normalInterfaceDeclaration : Parser NormalInterfaceDeclaration
normalInterfaceDeclaration =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        succeed NormalInterfaceDeclaration ) <|
        list interfaceModifier ) <|
        spaces ) <|
        (keyword "interface") ) <|
        spaces ) <|
        typeIdentifier ) <|
        spaces ) <|
        optional typeParameters ) <|
        spaces ) <|
        optional extendsInterfaces ) <|
        spaces ) <|
        interfaceBody


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
    oneOf
        [ kmap InterfaceModifier_Annotation annotation
        , imap InterfaceModifier_Public (keyword "public")
        , imap InterfaceModifier_Protected (keyword "protected")
        , imap InterfaceModifier_Private (keyword "private")
        , imap InterfaceModifier_Abstract (keyword "abstract")
        , imap InterfaceModifier_Static (keyword "static")
        , imap InterfaceModifier_Strictfp (keyword "strictfp")
        ]


type ExtendsInterfaces
    = ExtendsInterfaces InterfaceTypeList
    


extendsInterfaces : Parser ExtendsInterfaces
extendsInterfaces =
    iikmap ExtendsInterfaces
        (keyword "extends")
        spaces
        interfaceTypeList


type InterfaceBody
    = InterfaceBody (List InterfaceMemberDeclaration)
    


interfaceBody : Parser InterfaceBody
interfaceBody =
    lazy (\_ -> 
            iikiimap InterfaceBody
                (keyword "{")
                spaces
                (list interfaceMemberDeclaration)
                spaces
                (keyword "}")
    )


type InterfaceMemberDeclaration
    = InterfaceMemberDeclaration_Constant ConstantDeclaration
    | InterfaceMemberDeclaration_Method InterfaceMethodDeclaration
    | InterfaceMemberDeclaration_Class ClassDeclaration
    | InterfaceMemberDeclaration_Interface InterfaceDeclaration
    | InterfaceMemberDeclaration_Semi
    


interfaceMemberDeclaration : Parser InterfaceMemberDeclaration
interfaceMemberDeclaration =
    oneOf
        [ kmap InterfaceMemberDeclaration_Constant constantDeclaration
        , kmap InterfaceMemberDeclaration_Method interfaceMethodDeclaration
        , kmap InterfaceMemberDeclaration_Class classDeclaration
        , kmap InterfaceMemberDeclaration_Interface interfaceDeclaration
        , imap InterfaceMemberDeclaration_Semi (symbol ",")
        ]


type ConstantDeclaration
    = ConstantDeclaration (List ConstantModifier) UnannType VariableDeclaratorList
    


constantDeclaration : Parser ConstantDeclaration
constantDeclaration =
    kikikiimap ConstantDeclaration
        (list constantModifier)
        spaces
        unannType
        spaces
        variableDeclaratorList
        spaces
        (symbol ";")


type ConstantModifier
    = ConstantModifier_Annotation Annotation
    | ConstantModifier_Public
    | ConstantModifier_Static
    | ConstantModifier_Final
    


constantModifier : Parser ConstantModifier
constantModifier =
    oneOf
        [ kmap ConstantModifier_Annotation annotation
        , imap ConstantModifier_Public (keyword "public")
        , imap ConstantModifier_Static (keyword "static")
        , imap ConstantModifier_Final (keyword "final")
        ]


type InterfaceMethodDeclaration
    = InterfaceMethodDeclaration (List InterfaceMethodModifier) MethodHeader MethodBody
    


interfaceMethodDeclaration : Parser InterfaceMethodDeclaration
interfaceMethodDeclaration =
    kikikmap InterfaceMethodDeclaration
        (list interfaceMethodModifier)
        spaces
        methodHeader
        spaces
        methodBody


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
    oneOf
        [ kmap InterfaceMethodModifier_Annotation annotation
        , imap InterfaceMethodModifier_Public (keyword "public")
        , imap InterfaceMethodModifier_Private (keyword "private")
        , imap InterfaceMethodModifier_Abstract (keyword "abstract")
        , imap InterfaceMethodModifier_Default (keyword "default")
        , imap InterfaceMethodModifier_Static (keyword "static")
        , imap InterfaceMethodModifier_Strictfp (keyword "strictfp")
        ]


type AnnotationTypeDeclaration
    = AnnotationTypeDeclaration (List InterfaceModifier) TypeIdentifier AnnotationTypeBody
    


annotationTypeDeclaration : Parser AnnotationTypeDeclaration
annotationTypeDeclaration =
    kiiiiikikmap AnnotationTypeDeclaration
        (list interfaceModifier)
        spaces
        (symbol "@")
        spaces
        (keyword "interface")
        spaces
        typeIdentifier
        spaces
        annotationTypeBody


type AnnotationTypeBody
    = AnnotationTypeBody (List AnnotationTypeMemberDeclaration)
    


annotationTypeBody : Parser AnnotationTypeBody
annotationTypeBody =
    lazy (\_ ->
        iikiimap AnnotationTypeBody
            (symbol "{")
            spaces
            (list annotationTypeMemberDeclaration)
            spaces
            (symbol "}")
    )


type AnnotationTypeMemberDeclaration
    = AnnotationTypeMemberDeclaration_Element AnnotationTypeElementDeclaration
    | AnnotationTypeMemberDeclaration_Constant ConstantDeclaration
    | AnnotationTypeMemberDeclaration_Class ClassDeclaration
    | AnnotationTypeMemberDeclaration_Interface InterfaceDeclaration
    | AnnotationTypeMemberDeclaration_Semi
    


annotationTypeMemberDeclaration : Parser AnnotationTypeMemberDeclaration
annotationTypeMemberDeclaration =
    oneOf
        [ kmap AnnotationTypeMemberDeclaration_Element
             annotationTypeElementDeclaration
        , kmap AnnotationTypeMemberDeclaration_Constant
             constantDeclaration
        , kmap AnnotationTypeMemberDeclaration_Class
             classDeclaration
        , kmap AnnotationTypeMemberDeclaration_Interface
             interfaceDeclaration
        , imap AnnotationTypeMemberDeclaration_Semi
             (symbol ";")
        ]


type AnnotationTypeElementDeclaration
    = AnnotationTypeElementDeclaration (List AnnotationTypeElementModifier) UnannType Identifier (Maybe Dims) (Maybe DefaultValue)
    


annotationTypeElementDeclaration : Parser AnnotationTypeElementDeclaration
annotationTypeElementDeclaration =
        ignorer (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        succeed AnnotationTypeElementDeclaration ) <|
        list annotationTypeElementModifier ) <|
        spaces ) <|
        unannType ) <|
        spaces ) <|
        identifier ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        optional dims ) <|
        spaces ) <|
        optional defaultValue ) <|
        spaces ) <|
        (symbol ";")


type AnnotationTypeElementModifier
    = AnnotationTypeElementModifier_Annotation Annotation
    | AnnotationTypeElementModifier_Public
    | AnnotationTypeElementModifier_Abstract
    


annotationTypeElementModifier : Parser AnnotationTypeElementModifier
annotationTypeElementModifier =
    oneOf
        [ kmap AnnotationTypeElementModifier_Annotation annotation
        , imap AnnotationTypeElementModifier_Public (keyword "public")
        , imap AnnotationTypeElementModifier_Abstract (keyword "abstract")
        ]


type DefaultValue
    = DefaultValue ElementValue
    


defaultValue : Parser DefaultValue
defaultValue =
    iikmap DefaultValue
        (keyword "default")
        spaces
        elementValue


type Annotation
    = Annotation_Normal NormalAnnotation
    | Annotation_Marker MarkerAnnotation
    | Annotation_SingleElement SingleElementAnnotation
    


annotation : Parser Annotation
annotation =
    lazy (\_ ->
        oneOf
            [ kmap Annotation_Normal normalAnnotation
            , kmap Annotation_Marker markerAnnotation
            , kmap Annotation_SingleElement singleElementAnnotation
            ]
    )


type NormalAnnotation
    = NormalAnnotation TypeName (Maybe ElementValuePairList)
    


normalAnnotation : Parser NormalAnnotation
normalAnnotation =
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed NormalAnnotation ) <|
        (symbol "@") ) <|
        spaces ) <|
        typeName ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        (optional elementValuePairList) ) <|
        spaces ) <|
        (symbol ")")


type ElementValuePairList
    = ElementValuePairList ElementValuePair (List ElementValuePair)
    


elementValuePairList : Parser ElementValuePairList
elementValuePairList =
    kikmap ElementValuePairList
        elementValuePair
        spaces
        (list
            (iikmap identity
                (symbol ",")
                spaces
                elementValuePair
            )
        )


type ElementValuePair
    = ElementValuePair Identifier ElementValue
    


elementValuePair : Parser ElementValuePair
elementValuePair =
    kiiikmap ElementValuePair
        identifier
        spaces
        (symbol "=")
        spaces
        elementValue


type ElementValue
    = ElementValue_Conditional ConditionalExpression
    | ElementValue_ArrayInitializer ElementValueArrayInitializer
    | ElementValue_Annotation Annotation
    


elementValue : Parser ElementValue
elementValue =
    lazy (\_ ->
        oneOf
            --[ succeed ElementValue_Conditional
            --    |= conditionalExpression
            [ kmap ElementValue_ArrayInitializer elementValueArrayInitializer
            , kmap ElementValue_Annotation annotation
            ]
    )


type ElementValueArrayInitializer
    = ElementValueArrayInitializer (Maybe ElementValueList)
    


elementValueArrayInitializer : Parser ElementValueArrayInitializer
elementValueArrayInitializer =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed ElementValueArrayInitializer ) <|
        (symbol "{") ) <|
        spaces ) <|
        (optional (elementValueList)) ) <|
        spaces ) <|
        (optional (symbol ",")) ) <|
        spaces ) <|
        (symbol "}")


type ElementValueList
    = ElementValueList ElementValue (List ElementValue)
    


elementValueList : Parser ElementValueList
elementValueList =
    kikmap ElementValueList
        elementValue
        spaces
        (list
            (iikmap identity
                (symbol ",")
                spaces
                elementValue
            )
        )


type MarkerAnnotation
    = MarkerAnnotation TypeName
    


markerAnnotation : Parser MarkerAnnotation
markerAnnotation =
    ikmap MarkerAnnotation
        (symbol "@")
        typeName


type SingleElementAnnotation
    = SingleElementAnnotation TypeName ElementValue
    


singleElementAnnotation : Parser SingleElementAnnotation
singleElementAnnotation =
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed SingleElementAnnotation ) <|
        (symbol "@") ) <|
        spaces ) <|
        typeName ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        elementValue ) <|
        spaces ) <|
        (symbol ")")



-- }}}
-- {{{ Productions from §10 (Arrays)


type ArrayInitializer
    = ArrayInitializer (Maybe VariableInitializerList)
    


arrayInitializer : Parser ArrayInitializer
arrayInitializer =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed ArrayInitializer ) <|
        (symbol "{") ) <| 
        spaces ) <|
            -- TODO spaces?
        optional (variableInitializerList) ) <|
        spaces ) <|
        optional (symbol ",") ) <|
        spaces ) <|
        (symbol "}")


type VariableInitializerList
    = VariableInitializerList VariableInitializer (List VariableInitializer)
    


variableInitializerList : Parser VariableInitializerList
variableInitializerList =
    kikmap VariableInitializerList
        variableInitializer
        spaces
        (list
            (iikmap identity
                (symbol ",")
                spaces
                variableInitializer
            )
        )



-- }}}
-- {{{ Productions from §14 (Blocks and Statements)


type Block
    = Block (Maybe BlockStatements)
    


block : Parser Block
block =
    iikiimap Block
        (symbol "{")
        (spaces)
        (optional blockStatements)
        (spaces)
        (symbol "}")


type BlockStatements
    = BlockStatements BlockStatement (List BlockStatement)
    


blockStatements : Parser BlockStatements
blockStatements =
    kikmap BlockStatements
        blockStatement
        spaces
        (list blockStatement)


type BlockStatement
    = BlockStatement_LocalVariable LocalVariableDeclarationStatement
    | BlockStatement_Class ClassDeclaration
    | BlockStatement_Statement Statement
    


blockStatement : Parser BlockStatement
blockStatement =
    oneOf
        [ kmap BlockStatement_LocalVariable localVariableDeclarationStatement
        , kmap BlockStatement_Class (lazy (\_ -> classDeclaration))
        , kmap BlockStatement_Statement statement
        ]


type LocalVariableDeclarationStatement
    = LocalVariableDeclarationStatement LocalVariableDeclaration
    


localVariableDeclarationStatement : Parser LocalVariableDeclarationStatement
localVariableDeclarationStatement =
    kiimap LocalVariableDeclarationStatement
        localVariableDeclaration
        spaces
        (symbol ";")


type LocalVariableDeclaration
    = LocalVariableDeclaration (List VariableModifier) LocalVariableType VariableDeclaratorList
    


localVariableDeclaration : Parser LocalVariableDeclaration
localVariableDeclaration =
    kikikmap LocalVariableDeclaration
        (list variableModifier)
        spaces
        localVariableType
        spaces
        variableDeclaratorList


type LocalVariableType
    = LocalVariableType_UnannType UnannType
    | LocalVariableType_Var
    


localVariableType : Parser LocalVariableType
localVariableType =
    oneOf
        [ imap LocalVariableType_Var (keyword "var")
        , kmap LocalVariableType_UnannType unannType
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
    oneOf
        [ kmap Statement_Statement statementWithoutTrailingSubstatement
        , kmap Statement_Labeled labeledStatement
        , kmap Statement_IfThenElse ifThenElseStatement
        , kmap Statement_If ifThenStatement
        , kmap Statement_While whileStatement
        , kmap Statement_For forStatement
        ]


type StatementNoShortIf
    = StatementNoShortIf_NoTrailing StatementWithoutTrailingSubstatement
    | StatementNoShortIf_Labeled LabeledStatementNoShortIf
    | StatementNoShortIf_IfThenElse IfThenElseStatementNoShortIf
    | StatementNoShortIf_While WhileStatementNoShortIf
    | StatementNoShortIf_For ForStatementNoShortIf
    


statementNoShortIf : Parser StatementNoShortIf
statementNoShortIf =
    oneOf
        [ kmap StatementNoShortIf_NoTrailing statementWithoutTrailingSubstatement
        , kmap StatementNoShortIf_Labeled labeledStatementNoShortIf
        , kmap StatementNoShortIf_IfThenElse ifThenElseStatementNoShortIf
        , kmap StatementNoShortIf_While whileStatementNoShortIf
        , kmap StatementNoShortIf_For forStatementNoShortIf
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
    oneOf
        [ kmap StatementWithoutTrailingSubstatement_Empty
             emptyStatement
        , kmap StatementWithoutTrailingSubstatement_Block
             (lazy (\_ -> block))
        , kmap StatementWithoutTrailingSubstatement_Expression
             expressionStatement -- HERE
        , kmap StatementWithoutTrailingSubstatement_Assert
             assertStatement
        , kmap StatementWithoutTrailingSubstatement_Switch
             switchStatement
        , kmap StatementWithoutTrailingSubstatement_Do
             doStatement
        , kmap StatementWithoutTrailingSubstatement_Break
             breakStatement
        , kmap StatementWithoutTrailingSubstatement_Continue
             continueStatement
        , kmap StatementWithoutTrailingSubstatement_Return
             returnStatement
        , kmap StatementWithoutTrailingSubstatement_Synchronized
             synchronizedStatement
        , kmap StatementWithoutTrailingSubstatement_Throw
             throwStatement
        , kmap StatementWithoutTrailingSubstatement_Try
             tryStatement
        , kmap StatementWithoutTrailingSubstatement_Yield
             yieldStatement
        ]


type EmptyStatement
    = EmptyStatement
    


emptyStatement : Parser EmptyStatement
emptyStatement =
    imap EmptyStatement (symbol ";")


type LabeledStatement
    = LabeledStatement Identifier Statement
    


labeledStatement : Parser LabeledStatement
labeledStatement =
    kiiikmap LabeledStatement
        identifier
        spaces
        (symbol ":")
        spaces
        (lazy (\_ -> statement))


type LabeledStatementNoShortIf
    = LabeledStatementNoShortIf Identifier StatementNoShortIf
    


labeledStatementNoShortIf : Parser LabeledStatementNoShortIf
labeledStatementNoShortIf =
    kiiikmap LabeledStatementNoShortIf
        identifier
        spaces
        (symbol ":")
        spaces
        (lazy (\_ -> statementNoShortIf))


type ExpressionStatement
    = ExpressionStatement StatementExpression
    


expressionStatement : Parser ExpressionStatement
expressionStatement =
    kiimap ExpressionStatement
        statementExpression
        spaces
        (symbol ";")


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
    oneOf
        [ kmap StatementExpression_Assignment
             assignment
        , kmap StatementExpression_PreIncrement
             preIncrementExpression
        , kmap StatementExpression_PreDecrement
             preDecrementExpression
        --, kmap StatementExpression_PostIncrement
        --     postIncrementExpression
        --, kmap StatementExpression_PostDecrement
        --     postDecrementExpression
        , kmap StatementExpression_MethodInvocation
             methodInvocation
        --, kmap StatementExpression_ClassCreation
        --     classInstanceCreationExpression
        ]


type IfThenStatement
    = IfThenStatement Expression Statement
    


ifThenStatement : Parser IfThenStatement
ifThenStatement =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed IfThenStatement ) <|
        (keyword "if") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        (lazy (\_ -> statement))


type IfThenElseStatement
    = IfThenElseStatement Expression StatementNoShortIf Statement


ifThenElseStatement : Parser IfThenElseStatement
ifThenElseStatement =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed IfThenElseStatement ) <|
        (keyword "if") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        statementNoShortIf ) <|
        spaces ) <|
        (keyword "else") ) <|
        spaces ) <|
        lazy (\_ -> statement)


type IfThenElseStatementNoShortIf
    = IfThenElseStatementNoShortIf Expression StatementNoShortIf StatementNoShortIf
    


ifThenElseStatementNoShortIf : Parser IfThenElseStatementNoShortIf
ifThenElseStatementNoShortIf =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed IfThenElseStatementNoShortIf ) <|
        (keyword "if") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        lazy (\_ -> statementNoShortIf) ) <|
        spaces ) <|
        (keyword "else") ) <|
        spaces ) <|
        lazy (\_ -> statementNoShortIf)


type AssertStatement
    = AssertStatement_Expression Expression
    | AssertStatement_WithError Expression Expression
    


assertStatement : Parser AssertStatement
assertStatement =
    oneOf
        [ iikiimap AssertStatement_Expression
            (keyword "assert")
            spaces
            expression
            spaces
            (symbol ";")
        , ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          succeed AssertStatement_WithError) <|
          (keyword "assert")) <|
          spaces) <|
          expression) <|
          spaces) <|
          (symbol ":")) <|
          spaces) <|
          expression) <|
          spaces) <|
          (symbol ";")
        ]


type SwitchStatement
    = SwitchStatement Expression SwitchBlock
    


switchStatement : Parser SwitchStatement
switchStatement =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed SwitchStatement ) <|
        (keyword "switch") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        switchBlock


type SwitchBlock
    = SwitchBlock_Rule SwitchRule (List SwitchRule)
    | SwitchBlock_Group (List SwitchBlockStatementGroup) (List SwitchLabel)
    


switchBlock : Parser SwitchBlock
switchBlock =
    iikiimap identity
        (symbol "{")
        spaces
        ( oneOf
            [ kikmap SwitchBlock_Rule
                switchRule
                spaces
                (list switchRule)
            , kikmap SwitchBlock_Group
                (list switchBlockStatementGroup)
                spaces
                (list
                    (kiimap identity
                        switchLabel
                        spaces
                        (symbol ":")
                    )
                )
            ]
        )
        spaces
        (symbol "}")


type SwitchRule
    = SwitchRule_Expression SwitchLabel Expression
    | SwitchRule_Block SwitchLabel Block
    | SwitchRule_Throw SwitchLabel ThrowStatement
    


switchRule : Parser SwitchRule
switchRule =
    oneOf
        [ kiiikiimap SwitchRule_Expression
            switchLabel
            spaces
            (symbol "->")
            spaces
            expression
            spaces
            (symbol ";")
        , kiiikiimap SwitchRule_Block
            switchLabel
            spaces
            (symbol "->")
            spaces
            (lazy (\_ -> block))
            spaces
            (symbol ";")
        , kiiikiimap SwitchRule_Throw
            switchLabel
            spaces
            (symbol "->")
            spaces
            throwStatement
            spaces
            (symbol ";")
        ]


type SwitchBlockStatementGroup
    = SwitchBlockStatementGroup SwitchLabel (List SwitchLabel) BlockStatements
    


switchBlockStatementGroup : Parser SwitchBlockStatementGroup
switchBlockStatementGroup =
    kiiikikmap SwitchBlockStatementGroup
        switchLabel
        spaces
        (symbol ":")
        spaces
        (list
            (kiimap identity
                switchLabel
                spaces
                (keyword ":")
            )
        )
        spaces
        (lazy (\_ -> blockStatements))


type SwitchLabel
    = SwitchLabel_Case CaseConstant (List CaseConstant)
    | SwitchLabel_Default
    


switchLabel : Parser SwitchLabel
switchLabel =
    oneOf
        [ iikkmap SwitchLabel_Case
            (keyword "case")
            spaces
            caseConstant
            (list
                (iikmap identity
                    (symbol ",")
                    spaces
                    caseConstant
                )
            )
        , imap SwitchLabel_Default
            (keyword "default")
        ]


type CaseConstant
    = CaseConstant ConditionalExpression
    


caseConstant : Parser CaseConstant
caseConstant =
    kmap CaseConstant (lazy (\_ -> conditionalExpression))


type WhileStatement
    = WhileStatement Expression Statement
    


whileStatement : Parser WhileStatement
whileStatement =
    iiiikiiikmap WhileStatement
        (keyword "while")
        spaces
        (symbol "(")
        spaces
        expression
        spaces
        (symbol ")")
        spaces
        (lazy (\_ -> statement))


type WhileStatementNoShortIf
    = WhileStatementNoShortIf Expression StatementNoShortIf
    


whileStatementNoShortIf : Parser WhileStatementNoShortIf
whileStatementNoShortIf =
    iiiikiiikmap WhileStatementNoShortIf
        (keyword "while")
        spaces
        (symbol "(")
        spaces
        expression
        spaces
        (symbol ")")
        spaces
        (lazy (\_ -> statementNoShortIf))


type DoStatement
    = DoStatement Statement Expression
    


doStatement : Parser DoStatement
doStatement =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed DoStatement ) <|
        (keyword "do") ) <|
        spaces ) <|
        lazy (\_ -> statement) ) <|
        spaces ) <|
        (keyword "while") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        (symbol ";")


type ForStatement
    = ForStatement_Basic BasicForStatement
    | ForStatement_Enhanced EnhancedForStatement
    


forStatement : Parser ForStatement
forStatement =
    oneOf
        [ kmap ForStatement_Basic basicForStatement
        , kmap ForStatement_Enhanced enhancedForStatement
        ]


type ForStatementNoShortIf
    = ForStatementNoShortIf_Basic BasicForStatementNoShortIf
    | ForStatementNoShortIf_Enhanced EnhancedForStatementNoShortIf
    


forStatementNoShortIf : Parser ForStatementNoShortIf
forStatementNoShortIf =
    oneOf
        [ kmap ForStatementNoShortIf_Basic
            basicForStatementNoShortIf
        , kmap ForStatementNoShortIf_Enhanced
            enhancedForStatementNoShortIf
        ]


type BasicForStatement
    = BasicForStatement (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) Statement
    


basicForStatement : Parser BasicForStatement
basicForStatement =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed BasicForStatement ) <|
        (keyword "for") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        optional forInit ) <|
        spaces ) <|
        (symbol ";") ) <|
        spaces ) <|
        optional expression ) <|
        spaces ) <|
        (symbol ";") ) <|
        spaces ) <|
        optional forUpdate ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        lazy (\_ -> statement)


type BasicForStatementNoShortIf
    = BasicForStatementNoShortIf (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) StatementNoShortIf
    


basicForStatementNoShortIf : Parser BasicForStatementNoShortIf
basicForStatementNoShortIf =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed BasicForStatementNoShortIf ) <|
        (keyword "for") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        optional forInit ) <|
        spaces ) <|
        (symbol ";") ) <|
        spaces ) <|
        optional expression ) <|
        spaces ) <|
        (symbol ";") ) <|
        spaces ) <|
        optional forUpdate ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        lazy (\_ -> statementNoShortIf)


type ForInit
    = ForInit_StatementList StatementExpressionList
    | ForInit_Variable LocalVariableDeclaration
    


forInit : Parser ForInit
forInit =
    oneOf
        [ kmap ForInit_StatementList
            statementExpressionList
        , kmap ForInit_Variable
            localVariableDeclaration
        ]


type ForUpdate
    = ForUpdate StatementExpressionList
    


forUpdate : Parser ForUpdate
forUpdate = kmap ForUpdate statementExpressionList


type StatementExpressionList
    = StatementExpressionList StatementExpression (List StatementExpression)
    


statementExpressionList : Parser StatementExpressionList
statementExpressionList =
    kikmap StatementExpressionList
        statementExpression
        spaces
        (list
            (iikmap identity
                (symbol ",")
                spaces
                statementExpression
            )
        )


type EnhancedForStatement
    = EnhancedForStatement (List VariableModifier) LocalVariableType VariableDeclaratorId Expression Statement
    


enhancedForStatement : Parser EnhancedForStatement
enhancedForStatement =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed EnhancedForStatement ) <|
        (keyword "for") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        list variableModifier ) <|
        spaces ) <|
        localVariableType ) <|
        spaces ) <|
        variableDeclaratorId ) <|
        spaces ) <|
        (symbol ":") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        lazy (\_ -> statement)


type EnhancedForStatementNoShortIf
    = EnhancedForStatementNoShortIf (List VariableModifier) LocalVariableType VariableDeclaratorId Expression StatementNoShortIf
    


enhancedForStatementNoShortIf : Parser EnhancedForStatementNoShortIf
enhancedForStatementNoShortIf =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        succeed EnhancedForStatementNoShortIf ) <|
        (keyword "for") ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        list variableModifier ) <|
        spaces ) <|
        localVariableType ) <|
        spaces ) <|
        variableDeclaratorId ) <|
        spaces ) <|
        (symbol ":") ) <|
        spaces ) <|
        expression ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        lazy (\_ -> statementNoShortIf)


type BreakStatement
    = BreakStatement (Maybe Identifier)
    


breakStatement : Parser BreakStatement
breakStatement =
    iikiimap BreakStatement
        (keyword "break")
        spaces
        (optional identifier)
        spaces
        (symbol ";")


type YieldStatement
    = YieldStatement Expression
    


yieldStatement : Parser YieldStatement
yieldStatement =
    iikiimap YieldStatement
        (keyword "yield")
        spaces
        expression
        spaces
        (symbol ";")


type ContinueStatement
    = ContinueStatement (Maybe Identifier)
    


continueStatement : Parser ContinueStatement
continueStatement = 
    iikiimap ContinueStatement
        (keyword "continue")
        spaces
        (optional identifier)
        spaces
        (symbol ";")

type ReturnStatement
    = ReturnStatement (Maybe Expression)
    


returnStatement : Parser ReturnStatement
returnStatement =
    iikiimap ReturnStatement
        (keyword "return")
        spaces
        (optional expression)
        spaces
        (symbol ";")

type ThrowStatement
    = ThrowStatement Expression
    


throwStatement : Parser ThrowStatement
throwStatement =
    iikiimap ThrowStatement
        (keyword "throw")
        spaces
        expression
        spaces
        (symbol ";")


type SynchronizedStatement
    = SynchronizedStatement Expression Block
    


synchronizedStatement : Parser SynchronizedStatement
synchronizedStatement =
    iiiikiiikmap SynchronizedStatement
        (keyword "synchronized")
        spaces
        (symbol "(")
        spaces
        expression
        spaces
        (symbol ")")
        spaces
        (lazy (\_ -> block))


type TryStatement
    = TryStatement_Normal Block Catches
    | TryStatement_Finally Block (Maybe Catches) Finally
    | TryStatement_With TryWithResourcesStatement
    


tryStatement : Parser TryStatement
tryStatement =
    oneOf
        [ iikikmap TryStatement_Normal
            (keyword "try")
            spaces
            (lazy (\_ -> block))
            spaces
            catches
        , iikikikmap TryStatement_Finally
            (keyword "try")
            spaces
            (lazy (\_ -> block))
            spaces
            (optional catches)
            spaces
            finally
        , kmap TryStatement_With
            tryWithResourcesStatement
        ]


type Catches
    = Catches CatchClause (List CatchClause)
    


catches : Parser Catches
catches =
    kikmap Catches
        catchClause
        spaces
        (list catchClause)


type CatchClause
    = CatchClause CatchFormalParameter Block
    


catchClause : Parser CatchClause
catchClause =
    iiiikiiikmap CatchClause
        (keyword "catch")
        spaces
        (symbol "(")
        spaces
        catchFormalParameter
        spaces
        (symbol ")")
        spaces
        (lazy (\_ -> block))


type CatchFormalParameter
    = CatchFormalParameter (List VariableModifier) CatchType VariableDeclaratorId
    


catchFormalParameter : Parser CatchFormalParameter
catchFormalParameter =
    kikikmap CatchFormalParameter
        (list variableModifier)
        spaces
        catchType
        spaces
        variableDeclaratorId


type CatchType
    = CatchType UnannClassType (List ClassType)
    


catchType : Parser CatchType
catchType =
    kikmap CatchType
           unannClassType
           spaces
           (list
            (iikmap identity
                (symbol "|")
                spaces
                classType
            )
           )


type Finally
    = Finally Block
    


finally : Parser Finally
finally =
    iikmap Finally
        (succeed "finally")
        spaces
        (lazy (\_ -> block))


type TryWithResourcesStatement
    = TryWithResourcesStatement ResourceSpecification Block (Maybe Catches) (Maybe Finally)
    


tryWithResourcesStatement : Parser TryWithResourcesStatement
tryWithResourcesStatement =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed TryWithResourcesStatement ) <|
        (keyword "try") ) <|
        spaces ) <|
        resourceSpecification ) <|
        spaces ) <|
        (lazy (\_ -> block)) ) <|
        spaces ) <|
        (optional catches) ) <|
        spaces ) <|
        (optional finally)


type ResourceSpecification
    = ResourceSpecification ResourceList
    


resourceSpecification : Parser ResourceSpecification
resourceSpecification =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed ResourceSpecification ) <|
        (symbol "(") ) <|
        spaces ) <|
        resourceList ) <|
        spaces ) <|
        (optional (symbol ";")) ) <|
        spaces ) <|
        (symbol ")")


type ResourceList
    = ResourceList Resource (List Resource)
    


resourceList : Parser ResourceList
resourceList =
    kikmap ResourceList
        resource
        spaces
        (list
            (iikmap identity
                (symbol ";")
                spaces
                resource
            )
        )


type Resource
    = Resource_Declaration (List VariableModifier) LocalVariableType Identifier Expression
    | Resource_VariableAccess VariableAccess
    


resource : Parser Resource
resource =
    oneOf
        [ keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          keeper (
          ignorer (
          keeper (
          succeed Resource_Declaration ) <|
          list variableModifier ) <|
          spaces ) <|
          localVariableType ) <|
          spaces ) <|
          identifier ) <|
          spaces ) <|
          (symbol "=") ) <|
          spaces ) <|
          expression
        , kmap Resource_VariableAccess
            variableAccess
        ]


type VariableAccess
    = VariableAccess_Expression ExpressionName
    | VariableAccess_Field FieldAccess
    


variableAccess : Parser VariableAccess
variableAccess =
    oneOf
        [ kmap VariableAccess_Expression
            expressionName
        , kmap VariableAccess_Field
            fieldAccess
        ]



-- }}}
-- {{{ Productions from §15 (Expressions)


type Primary
    = Primary_NoNewArray PrimaryNoNewArray
    | Primary_Creation ArrayCreationExpression
    


primary : Parser Primary
primary =
    oneOf
        [ kmap Primary_NoNewArray
            primaryNoNewArray
        , kmap Primary_Creation
            arrayCreationExpression
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
    oneOf
        [ kmap PrimaryNoNewArray_Literal
            literal
        , kmap PrimaryNoNewArray_ClassLiteral
            classLiteral
        , imap PrimaryNoNewArray_This
            (keyword "this")
        , kiiiimap PrimaryNoNewArray_TypeThis
            typeName
            spaces
            (symbol ".")
            spaces
            (keyword "this")
        , iikiimap PrimaryNoNewArray_BracketsExpression
            (symbol "(")
            spaces
            expression
            spaces
            (symbol ")")
        , kmap PrimaryNoNewArray_ClassCreation
           classInstanceCreationExpression
        --, kmap PrimaryNoNewArray_FieldAccess
        --     fieldAccess
        --, kmap PrimaryNoNewArray_ArrayAccess
        --     (lazy (\_ -> arrayAccess)) -- ?
        --, kmap PrimaryNoNewArray_MethodInvocation
        --     methodInvocation
        --, kmap PrimaryNoNewArray_MethodReference
        --     methodReference
        ]


type ClassLiteral
    = ClassLiteral_TypeName TypeName Int
    | ClassLiteral_Numeric NumericType Int
    | ClassLiteral_Boolean Int
    | ClassLiteral_Void
    


classLiteral : Parser ClassLiteral
classLiteral =
    oneOf
        [ ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          keeper (
          succeed ClassLiteral_TypeName ) <|
          typeName ) <|
          spaces ) <|
          brackets ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          keeper (
          succeed ClassLiteral_Numeric ) <|
          numericType ) <|
          spaces ) <|
          brackets ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          succeed ClassLiteral_Boolean ) <|
          (keyword "boolean") ) <|
          spaces ) <|
          brackets ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          succeed ClassLiteral_Void ) <|
          (keyword "void") ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "class")
        ]


type ClassInstanceCreationExpression
    = ClassInstanceCreationExpression_Normal UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Expression ExpressionName UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Primary Primary UnqualifiedClassInstanceCreationExpression
    


classInstanceCreationExpression : Parser ClassInstanceCreationExpression
classInstanceCreationExpression =
    oneOf
        [ kmap ClassInstanceCreationExpression_Normal
            unqualifiedClassInstanceCreationExpression
        , kiiikmap ClassInstanceCreationExpression_Expression
            expressionName
            spaces
            (symbol ".")
            spaces
            unqualifiedClassInstanceCreationExpression
        --, kiiikmap ClassInstanceCreationExpression_Primary
        --    (lazy (\_ -> primary))
        --    spaces
        --    (symbol ".")
        --    spaces
        --    unqualifiedClassInstanceCreationExpression
        ]


type UnqualifiedClassInstanceCreationExpression
    = UnqualifiedClassInstanceCreationExpression (Maybe TypeArguments) ClassOrInterfaceTypeToInstantiate (Maybe ArgumentList) (Maybe ClassBody)
    


unqualifiedClassInstanceCreationExpression : Parser UnqualifiedClassInstanceCreationExpression
unqualifiedClassInstanceCreationExpression =
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed UnqualifiedClassInstanceCreationExpression ) <|
        (keyword "new") ) <|
        spaces ) <|
        optional typeArguments ) <|
        spaces ) <|
        classOrInterfaceTypeToInstantiate ) <|
        spaces ) <|
        (symbol "(") ) <|
        spaces ) <|
        optional argumentList ) <|
        spaces ) <|
        (symbol ")") ) <|
        spaces ) <|
        optional (lazy (\_ -> classBody))


type ClassOrInterfaceTypeToInstantiate
    = ClassOrInterfaceTypeToInstantiate (List Annotation) Identifier (List(
        (List Annotation), Identifier )) (Maybe TypeArgumentsOrDiamond)
    


classOrInterfaceTypeToInstantiate : Parser ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate =
    kikikikmap ClassOrInterfaceTypeToInstantiate
        (list annotation)
        spaces
        identifier
        spaces
        (list
            (iikikmap Tuple.pair
                (symbol ".")
                spaces
                (list annotation)
                spaces
                identifier
            )
        )
        spaces
        (optional typeArgumentsOrDiamond)


type TypeArgumentsOrDiamond
    = TypeArguments_TypeArguments TypeArguments
    | TypeArguments_Diamond
    


typeArgumentsOrDiamond : Parser TypeArgumentsOrDiamond
typeArgumentsOrDiamond =
    oneOf
        [ imap TypeArguments_Diamond
            (symbol "<>")
        , kmap TypeArguments_TypeArguments
            typeArguments
        ]


type FieldAccess
    = FieldAccess_Primary Primary Identifier
    | FieldAccess_Super Identifier
    | FieldAccess_TypeNameSuper TypeName Identifier
    


fieldAccess : Parser FieldAccess
fieldAccess =
    oneOf
        [ kiiikmap FieldAccess_Primary
            (lazy (\_ -> primary))
            spaces
            (symbol ".")
            spaces
            identifier
        , iiiikmap FieldAccess_Super
            (keyword "super")
            spaces
            (symbol ".")
            spaces
            identifier
        , keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed FieldAccess_TypeNameSuper ) <|
          typeName ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "super") ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          identifier
        ]


type ArrayAccess
    = ArrayAccess_Expression ExpressionName Expression
    | ArrayAccess_Primary PrimaryNoNewArray Expression
    


arrayAccess : Parser ArrayAccess
arrayAccess =
    oneOf
        [ kiiikiimap ArrayAccess_Expression
            expressionName
            spaces
            (symbol "[")
            spaces
            expression
            spaces
            (symbol "]")
        , kiiikiimap ArrayAccess_Primary
            primaryNoNewArray
            spaces
            (symbol "[")
            spaces
            expression
            spaces
            (symbol "]")
        ]


type MethodInvocation
    = MethodInvocation_Name MethodName (Maybe ArgumentList)
    | MethodInvocation_Type TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Expression ExpressionName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    --| MethodInvocation_Primary Primary (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    {--}| MethodInvocation_TODO (List Identifier) (Maybe ArgumentList)
    | MethodInvocation_Super (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_TypeSuper TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    


methodInvocation : Parser MethodInvocation
methodInvocation =
    oneOf
        [
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            succeed MethodInvocation_Name ) <|
            methodName ) <|
            spaces ) <|
            (symbol "(") ) <|
            spaces ) <|
            optional argumentList ) <|
            spaces ) <|
            (symbol ")")
        ,
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            succeed MethodInvocation_Type ) <|
            typeName ) <|
            spaces ) <|
            (symbol ".") ) <|
            spaces ) <|
            optional typeArguments ) <|
            spaces ) <|
            identifier ) <|
            spaces ) <|
            (symbol "(") ) <|
            spaces ) <|
            optional argumentList ) <|
            spaces ) <|
            (symbol ")")
        ,   
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            succeed MethodInvocation_Expression ) <|
            expressionName ) <|
            spaces ) <|
            (symbol ".") ) <|
            spaces ) <|
            optional typeArguments ) <|
            spaces ) <|
            identifier ) <|
            spaces ) <|
            (symbol "(") ) <|
            spaces ) <|
            optional argumentList ) <|
            spaces ) <|
            (symbol ")")
            ----, ignorer (
            ----  ignorer (
            ----  keeper (
            ----  ignorer (
            ----  ignorer (
            ----  ignorer (
            ----  keeper (
            ----  ignorer (
            ----  keeper (
            ----  ignorer (
            ----  ignorer (
            ----  ignorer (
            ----  keeper (
            ----  succeed MethodInvocation_Primary ) <|
            ----  lazy (\_ -> primary) ) <|
            ----  spaces ) <|
            ----  (symbol ".") ) <|
            ----  spaces ) <|
            ----  optional typeArguments ) <|
            ----  spaces ) <|
            ----  identifier ) <|
            ----  spaces ) <|
            ----  (symbol "(") ) <|
            ----  spaces ) <|
            ----  optional argumentList ) <|
            ----  spaces ) <|
            ----  (symbol ")")
            , ignorer (
              ignorer (
              keeper (
              ignorer (
              ignorer (
              ignorer (
              keeper (
              succeed MethodInvocation_TODO ) <|
              dotted identifier ) <|
              spaces ) <|
              (symbol "(") ) <|
              spaces ) <|
              optional argumentList ) <|
              spaces ) <|
              (symbol ")")
        ,   
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            succeed MethodInvocation_Super ) <|
            symbol "super" ) <|
            spaces ) <|
            (symbol ".") ) <|
            spaces ) <|
            optional typeArguments ) <|
            spaces ) <|
            identifier ) <|
            spaces ) <|
            (symbol "(") ) <|
            spaces ) <|
            optional argumentList ) <|
            spaces ) <|
            (symbol ")")
        , 
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            succeed MethodInvocation_TypeSuper ) <|
            typeName ) <|
            spaces ) <|
            (symbol ".") ) <|
            spaces ) <|
            (keyword "super") ) <|
            spaces ) <|
            (symbol ".") ) <|
            spaces ) <|
            optional typeArguments ) <|
            spaces ) <|
            identifier ) <|
            spaces ) <|
            (symbol "(") ) <|
            spaces ) <|
            optional argumentList ) <|
            spaces ) <|
            (symbol ")")
        ]


type ArgumentList
    = ArgumentList Expression (List Expression)
    


argumentList : Parser ArgumentList
argumentList =
    kikmap ArgumentList
           expression
           spaces
           (list
            (iikmap identity
                (symbol ",")
                spaces
                expression
            )
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
    oneOf
        [ keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_Expression ) <|
          expressionName ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          identifier
        , keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_Primary ) <|
          lazy (\_ -> primary) ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          identifier
        , keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_Reference ) <|
          referenceType ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          identifier
        , keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          succeed MethodReference_Super ) <|
          (keyword "super") ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          identifier
        , keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          succeed MethodReference_TypeSuper ) <|
          spaces ) <|
          typeName ) <|
          spaces ) <|
          (symbol ".") ) <|
          spaces ) <|
          (keyword "super") ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          identifier
        , ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_ClassNew ) <|
          classType ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          optional typeArguments ) <|
          spaces ) <|
          (keyword "new")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_ArrayNew ) <|
          arrayType ) <|
          spaces ) <|
          symbol ":" ) <|
          spaces ) <|
          (keyword "new")
        ]


type ArrayCreationExpression
    = ArrayCreationExpression_Primitive PrimitiveType DimExprs (Maybe Dims)
    | ArrayCreationExpression_Class ClassOrInterfaceType DimExprs (Maybe Dims)
    | ArrayCreationExpression_PrimitiveArrayInit PrimitiveType Dims ArrayInitializer
    | ArrayCreationExpression_ClassArrayInit ClassOrInterfaceType Dims ArrayInitializer
    


arrayCreationExpression : Parser ArrayCreationExpression
arrayCreationExpression =
    oneOf
        [
            keeper (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed ArrayCreationExpression_Primitive ) <|
            (keyword "new") ) <|
            spaces ) <|
            primitiveType ) <|
            spaces ) <|
            dimExprs ) <|
            spaces ) <|
            optional dims
        ,
            keeper (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed ArrayCreationExpression_Class ) <|
            (keyword "new") ) <|
            spaces ) <|
            classOrInterfaceType ) <|
            spaces ) <|
            dimExprs ) <|
            spaces ) <|
            optional dims
        ,
            keeper (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed ArrayCreationExpression_PrimitiveArrayInit ) <|
            (keyword "new") ) <|
            spaces ) <|
            primitiveType ) <|
            spaces ) <|
            dims ) <|
            spaces ) <|
            arrayInitializer
        ,
            keeper (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed ArrayCreationExpression_ClassArrayInit ) <|
            (keyword "new") ) <|
            spaces ) <|
            classOrInterfaceType ) <|
            spaces ) <|
            dims ) <|
            spaces ) <|
            arrayInitializer
        ]


type DimExprs
    = DimExprs DimExpr (List DimExpr)
    


dimExprs : Parser DimExprs
dimExprs =
    kikmap DimExprs
        dimExpr
        spaces
        (list dimExpr)


type DimExpr
    = DimExpr (List Annotation) Expression
    


dimExpr : Parser DimExpr
dimExpr =
    kiiikiimap DimExpr
        (list annotation)
        spaces
        (symbol "[")
        spaces
        expression
        spaces
        (symbol "]")


type Expression
    = Expression_Lambda LambdaExpression
    | Expression_Assignment AssignmentExpression
    


expression : Parser Expression
expression =
    oneOf
        [ kmap Expression_Lambda
            lambdaExpression
        , kmap Expression_Assignment
            assignmentExpression
        ]


type LambdaExpression
    = LambdaExpression LambdaParameters LambdaBody
    


lambdaExpression : Parser LambdaExpression
lambdaExpression =
    kiiikmap LambdaExpression
        lambdaParameters
        spaces
        (symbol "->")
        spaces
        (lazy (\_ -> lambdaBody))


type LambdaParameters
    = LambdaParameters_List (Maybe LambdaParameterList)
    | LambdaParameters_Identifier Identifier
    


lambdaParameters : Parser LambdaParameters
lambdaParameters =
    oneOf
        [ iikiimap LambdaParameters_List
            (symbol "(")
            spaces
            (optional lambdaParameterList)
            spaces
            (symbol ")")
        , kmap LambdaParameters_Identifier
            identifier
        ]


type LambdaParameterList
    = LambdaParameterList_Parameters LambdaParameter (List LambdaParameter)
    | LambdaParameterList_Identifiers Identifier (List Identifier)
    


lambdaParameterList : Parser LambdaParameterList
lambdaParameterList =
    oneOf
        [ kikmap LambdaParameterList_Parameters
            lambdaParameter
            spaces
            (list
                (iikmap identity
                    (symbol ",")
                    spaces
                    lambdaParameter
                )
               )
        , kikmap LambdaParameterList_Identifiers
            identifier
            spaces
            (list
                (iikmap identity
                    (symbol ",")
                    spaces
                    identifier
                )
               )
        ]


type LambdaParameter
    = LambdaParameter_Normal (List VariableModifier) LambdaParameterType VariableDeclaratorId
    | LambdaParameter_Arity VariableArityParameter
    


lambdaParameter : Parser LambdaParameter
lambdaParameter =
    oneOf
        [ kikikmap LambdaParameter_Normal
            (list variableModifier)
            spaces
            lambdaParameterType
            spaces
            variableDeclaratorId
        , kmap LambdaParameter_Arity
            variableArityParameter
        ]


type LambdaParameterType
    = LambdaParameterType_Unann UnannType
    | LambdaParameterType_Var
    


lambdaParameterType : Parser LambdaParameterType
lambdaParameterType =
    oneOf
        [ kmap LambdaParameterType_Unann
            unannType
        , imap LambdaParameterType_Var
            (keyword "var")
        ]


type LambdaBody
    = LambdaBody_Expression Expression
    | LambdaBody_Block Block
    


lambdaBody : Parser LambdaBody
lambdaBody =
    oneOf
        [ kmap LambdaBody_Expression
            expression
        , kmap LambdaBody_Block
            block
        ]


type AssignmentExpression
    = AssignmentExpression_Conditional ConditionalExpression
    | AssignmentExpression_Assignment Assignment
    


assignmentExpression : Parser AssignmentExpression
assignmentExpression =
    oneOf
        [ kmap AssignmentExpression_Conditional
            conditionalExpression
        , kmap AssignmentExpression_Assignment
            assignment
        ]


type Assignment
    = Assignment LeftHandSide AssignmentOperator Expression
    


assignment : Parser Assignment
assignment =
    kikikmap Assignment
        leftHandSide
        spaces
        assignmentOperator
        spaces
        (lazy (\_ -> expression))


type LeftHandSide
    = LeftHandSide_Expression ExpressionName
    | LeftHandSide_Field FieldAccess
    | LeftHandSide_Array ArrayAccess
    


leftHandSide : Parser LeftHandSide
leftHandSide =
    oneOf
        [ --kmap LeftHandSide_Array (lazy (\_ -> arrayAccess))
          kmap LeftHandSide_Field fieldAccess
        , kmap LeftHandSide_Expression expressionName
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
    oneOf
        [ imap AssignmentOperator_Normal
            (symbol "=")
        , imap AssignmentOperator_Multiply
            (symbol "*=")
        , imap AssignmentOperator_Divide
            (symbol "/=")
        , imap AssignmentOperator_Modulus
            (symbol "%=")
        , imap AssignmentOperator_Add
            (symbol "+=")
        , imap AssignmentOperator_Subtract
            (symbol "-=")
        , imap AssignmentOperator_LeftShift
            (symbol "<<=")
        , imap AssignmentOperator_RightShift3
            (symbol ">>>=")
        , imap AssignmentOperator_RightShift
            (symbol ">>=")
        , imap AssignmentOperator_And
            (symbol "&=")
        , imap AssignmentOperator_Xor
            (symbol "^=")
        , imap AssignmentOperator_Or
            (symbol "|=")
        ]


type ConditionalExpression
    = ConditionalExpression_Or ConditionalOrExpression
    | ConditionalExpression_TernaryConditional ConditionalOrExpression Expression ConditionalExpression
    | ConditionalExpression_TernaryLambda ConditionalOrExpression Expression LambdaExpression
    


conditionalExpression : Parser ConditionalExpression
conditionalExpression =
    oneOf
        [ kmap ConditionalExpression_Or
            conditionalOrExpression
        , kiiikiiikmap ConditionalExpression_TernaryConditional
            conditionalOrExpression
            spaces
            (symbol "?")
            spaces
            (lazy (\_ -> expression))
            spaces
            (symbol ":")
            spaces
            (lazy (\_ -> conditionalExpression))
        , kiiikiiikmap ConditionalExpression_TernaryLambda
            conditionalOrExpression
            spaces
            (symbol "?")
            spaces
            (lazy (\_ -> expression))
            spaces
            (symbol ":")
            spaces
            lambdaExpression
        ]


--------------------------- FROM HERE

zeta : String -> (a -> c) -> (b -> a -> c)
              -> Parser a -> (() -> Parser b) -> Parser c
zeta op tca tcb aparser bparser =
  kikmap (\a mb -> case mb of
                        Just b -> tcb b a
                        Nothing -> tca a)
  aparser
  spaces
  (optional
      ( iikmap identity
           (symbol op)
           spaces
           (lazy bparser)
      )
  )


type ConditionalOrExpression
  = ConditionalOrExpression_And ConditionalAndExpression
  | ConditionalOrExpression_Or ConditionalOrExpression ConditionalAndExpression
    

conditionalOrExpression : Parser ConditionalOrExpression
conditionalOrExpression =
  zeta
  "||"
  ConditionalOrExpression_And
  ConditionalOrExpression_Or
  conditionalAndExpression
  (\_ -> conditionalOrExpression)


type ConditionalAndExpression
  = ConditionalAndExpression_Or InclusiveOrExpression
  | ConditionalAndExpression_And ConditionalAndExpression InclusiveOrExpression
    

conditionalAndExpression : Parser ConditionalAndExpression
conditionalAndExpression =
  zeta
  "&&"
  ConditionalAndExpression_Or
  ConditionalAndExpression_And
  inclusiveOrExpression
  (\_ -> conditionalAndExpression)

type InclusiveOrExpression
  = InclusiveOrExpression_Xor ExclusiveOrExpression
  | InclusiveOrExpression_Or InclusiveOrExpression ExclusiveOrExpression
    

inclusiveOrExpression : Parser InclusiveOrExpression
inclusiveOrExpression =
  zeta
  "|"
  InclusiveOrExpression_Xor
  InclusiveOrExpression_Or
  exclusiveOrExpression
  (\_ -> inclusiveOrExpression)


type ExclusiveOrExpression
  = ExclusiveOrExpression_And AndExpression
  | ExclusiveOrExpression_Xor ExclusiveOrExpression AndExpression
    

exclusiveOrExpression : Parser ExclusiveOrExpression
exclusiveOrExpression =
  zeta
  "^"
  ExclusiveOrExpression_And
  ExclusiveOrExpression_Xor
  andExpression
  (\_ -> exclusiveOrExpression)


type AndExpression
  = AndExpression_Equality EqualityExpression
  | AndExpression_And AndExpression EqualityExpression
    

andExpression : Parser AndExpression
andExpression =
  zeta
  "&"
  AndExpression_Equality
  AndExpression_And
  equalityExpression
  (\_ -> andExpression)


type EqualityExpression
  = EqualityExpression_Relational RelationalExpression
  | EqualityExpression_Equals EqualityExpression RelationalExpression
  | EqualityExpression_NotEquals EqualityExpression RelationalExpression
    

equalityExpression : Parser EqualityExpression
equalityExpression =
  kikmap (\re mf ->
                 case mf of
                   Just f -> f re
                   Nothing -> EqualityExpression_Relational re
            )
     (lazy (\_ -> relationalExpression))
     spaces
     (optional 
       ( kikmap (\f ee -> f ee)
            (oneOf
            [ imap EqualityExpression_Equals (symbol "==")
            , imap EqualityExpression_NotEquals (symbol "!=")
            ]
            )
            spaces
            (lazy (\_ -> equalityExpression))
       )
     )


type RelationalExpression
  = RelationalExpression_Shift ShiftExpression
  | RelationalExpression_Less RelationalExpression ShiftExpression
  | RelationalExpression_Greater RelationalExpression ShiftExpression
  | RelationalExpression_LessEqual RelationalExpression ShiftExpression
  | RelationalExpression_GreaterEqual RelationalExpression ShiftExpression
  | RelationalExpression_Instanceof RelationalExpression ReferenceType
    

relationalExpression : Parser RelationalExpression
relationalExpression =
  kikmap (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> RelationalExpression_Shift a)
     (lazy (\_ -> shiftExpression))
     spaces
     (optional
      ( kikmap (\f a -> f a)
         (oneOf 
             [ imap RelationalExpression_LessEqual
                  (symbol "<=")
             , imap RelationalExpression_GreaterEqual
                  (symbol ">=")
             , imap RelationalExpression_Less
                  (symbol "<")
             , imap RelationalExpression_Greater
                  (symbol ">")
             ]
          )
          spaces
          (lazy (\_ -> relationalExpression))
       )
      )
       {- TODO
    , succeed RelationalExpression_Instanceof
      |= this
      |. spaces
      |. (keyword "instanceof")
      |. spaces
      |= lazy (\_ -> referenceType)
    ]
       -}


type ShiftExpression
  = ShiftExpression_Additive AdditiveExpression
  | ShiftExpression_Left ShiftExpression AdditiveExpression
  | ShiftExpression_Right ShiftExpression AdditiveExpression
  | ShiftExpression_Right2 ShiftExpression AdditiveExpression
    

shiftExpression : Parser ShiftExpression
shiftExpression =
  kikmap (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> ShiftExpression_Additive a)
     (lazy (\_ -> additiveExpression))
     spaces
     (optional
      ( kikmap (\f a -> f a)
          (oneOf 
             [ imap ShiftExpression_Left (symbol "<<")
             , imap ShiftExpression_Right2 (symbol ">>>")
             , imap ShiftExpression_Right (symbol ">>")
             ]
          )
          spaces
          (lazy (\_ -> shiftExpression))
      )
     )


type AdditiveExpression
  = AdditiveExpression_Multiplicative MultiplicativeExpression
  | AdditiveExpression_Plus AdditiveExpression MultiplicativeExpression
  | AdditiveExpression_Minus AdditiveExpression MultiplicativeExpression
    

additiveExpression : Parser AdditiveExpression
additiveExpression =
  kikmap (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing ->  AdditiveExpression_Multiplicative a)
     (lazy (\_ -> multiplicativeExpression))
     spaces
     (optional
      ( kikmap (\f a -> f a)
         (oneOf 
            [ imap AdditiveExpression_Plus
                 (symbol "+")
            , imap AdditiveExpression_Minus
                 (symbol "-")
            ]
          )
          spaces
          (lazy (\_ -> additiveExpression))
      )
     )

type MultiplicativeExpression
  = MultiplicativeExpression_Unary UnaryExpression
  | MultiplicativeExpression_Multiply MultiplicativeExpression UnaryExpression
  | MultiplicativeExpression_Divide MultiplicativeExpression UnaryExpression
  | MultiplicativeExpression_Mod MultiplicativeExpression UnaryExpression
    

multiplicativeExpression : Parser MultiplicativeExpression
multiplicativeExpression =
  kikmap (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> MultiplicativeExpression_Unary a)
     (lazy (\_ -> unaryExpression))
     spaces
     (optional
      (kikmap (\f a -> f a)
          (oneOf
           [ imap MultiplicativeExpression_Multiply
                (symbol "*")
           , imap MultiplicativeExpression_Divide
                (symbol "/")
           , imap MultiplicativeExpression_Mod
                (symbol "%")
           ]
          )
          spaces
          (lazy (\_ -> multiplicativeExpression))
      )
     )
 
--------------------------- TO HERE

type UnaryExpression
    = UnaryExpression_PreIncrement PreIncrementExpression
    | UnaryExpression_PreDecrement PreDecrementExpression
    | UnaryExpression_Plus UnaryExpression
    | UnaryExpression_Minus UnaryExpression
    | UnaryExpression_NotPlusMinus UnaryExpressionNotPlusMinus
    


unaryExpression : Parser UnaryExpression
unaryExpression =
    oneOf
        [ kmap UnaryExpression_PreIncrement
            preIncrementExpression
        , kmap UnaryExpression_PreDecrement
            preDecrementExpression
        , iikmap UnaryExpression_Plus
            (symbol "+")
            spaces
            (lazy (\_ -> unaryExpression))
        , iikmap UnaryExpression_Minus
            (symbol "-")
            spaces
            (lazy (\_ -> unaryExpression))
        , kmap UnaryExpression_NotPlusMinus
            unaryExpressionNotPlusMinus
        ]


type PreIncrementExpression
    = PreIncrementExpression UnaryExpression
    


preIncrementExpression : Parser PreIncrementExpression
preIncrementExpression =
    iikmap PreIncrementExpression
        (symbol "++")
        spaces
        (lazy (\_ -> unaryExpression))


type PreDecrementExpression
    = PreDecrementExpression UnaryExpression
    


preDecrementExpression : Parser PreDecrementExpression
preDecrementExpression =
    iikmap PreDecrementExpression
        (symbol "--")
        spaces
        (lazy (\_ -> unaryExpression))


type UnaryExpressionNotPlusMinus
    = UnaryExpressionNotPlusMinus_Postfix PostfixExpression
    | UnaryExpressionNotPlusMinus_BitwiseNot UnaryExpression
    | UnaryExpressionNotPlusMinus_LogicalNot UnaryExpression
    | UnaryExpressionNotPlusMinus_Cast CastExpression
    | UnaryExpressionNotPlusMinus_Switch SwitchExpression
    


unaryExpressionNotPlusMinus : Parser UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinus =
    oneOf
        [ kmap UnaryExpressionNotPlusMinus_Postfix
            postfixExpression
        , iikmap UnaryExpressionNotPlusMinus_BitwiseNot
            (symbol "~")
            spaces
            (lazy (\_ -> unaryExpression))
        , iikmap UnaryExpressionNotPlusMinus_LogicalNot
            (symbol "!")
            spaces
            (lazy (\_ -> unaryExpression))
        , kmap UnaryExpressionNotPlusMinus_Cast
            (lazy (\_ -> castExpression))
        , kmap UnaryExpressionNotPlusMinus_Switch
            switchExpression
        ]


type PostfixExpression
    = PostfixExpression_Primary Primary
    | PostfixExpression_Name ExpressionName
    | PostfixExpression_Increment PostIncrementExpression
    | PostfixExpression_Decrement PostDecrementExpression
    


postfixExpression : Parser PostfixExpression
postfixExpression =
    oneOf
        [ kmap PostfixExpression_Primary
            primary
        , kmap PostfixExpression_Name
            expressionName
        --, kmap PostfixExpression_Increment
        --    postIncrementExpression
        --, kmap PostfixExpression_Decrement
        --    postDecrementExpression
        ]


type PostIncrementExpression
    = PostIncrementExpression PostfixExpression
    


postIncrementExpression : Parser PostIncrementExpression
postIncrementExpression =
    kiimap PostIncrementExpression
        (lazy (\_ -> postfixExpression))
        spaces
        (symbol "++")


type PostDecrementExpression
    = PostDecrementExpression PostfixExpression
    


postDecrementExpression : Parser PostDecrementExpression
postDecrementExpression =
    kiimap PostDecrementExpression
        (lazy (\_ -> postfixExpression))
        spaces
        (symbol "--")


type CastExpression
    = CastExpression_Unary PrimitiveType UnaryExpression
    | CastExpression_UnaryAdditional ReferenceType (List AdditionalBound) UnaryExpressionNotPlusMinus
    | CastExpression_Lambda ReferenceType (List AdditionalBound) LambdaExpression
    


castExpression : Parser CastExpression
castExpression =
    oneOf
        [
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed CastExpression_Unary ) <|
            (symbol "(") ) <|
            spaces ) <|
            primitiveType ) <|
            spaces ) <|
            (symbol ")") ) <|
            spaces ) <|
            unaryExpression
        ,
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed CastExpression_UnaryAdditional ) <|
            (symbol "(") ) <|
            spaces ) <|
            referenceType ) <|
            spaces ) <|
            list additionalBound ) <|
            spaces ) <|
            (symbol ")") ) <|
            spaces ) <|
            unaryExpressionNotPlusMinus
        ,
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            succeed CastExpression_Lambda ) <|
            (symbol "(") ) <|
            spaces ) <|
            referenceType ) <|
            spaces ) <|
            list additionalBound ) <|
            spaces ) <|
            (symbol ")") ) <|
            spaces ) <|
            lambdaExpression
        ]


type SwitchExpression
    = SwitchExpression Expression SwitchBlock
    


switchExpression : Parser SwitchExpression
switchExpression =
    keeper (
    ignorer (
    ignorer (
    ignorer (
    keeper (
    ignorer (
    ignorer (
    ignorer (
    ignorer (
    succeed SwitchExpression ) <|
    (keyword "switch") ) <|
    spaces ) <|
    (symbol "(") ) <|
    spaces ) <|
    expression ) <|
    spaces ) <|
    (symbol ")") ) <|
    spaces ) <|
    switchBlock


type ConstantExpression
    = ConstantExpression Expression
    


constantExpression : Parser ConstantExpression
constantExpression =
    kmap ConstantExpression
        expression



-- }}}
