module Fast15 where

import qualified Data.Char as Char
import qualified Data.List as List

--{{{ custom parser
type Parser a = String -> Either String (a, String)

a |. b = ignorer a b
a |= b = keeper a b

data BrackNum = BN Identifier deriving (Show)

num0 :: Parser BrackNum
num0 =
    succeed BN
    |. (symbol "<")
    |. spaces
    |= identifier
    |. spaces
    |. (symbol ">")

num1 :: Parser BrackNum
num1 =
    ignorer (
    ignorer (
    keeper (
    ignorer (
    ignorer (
    succeed BN ) $
    (symbol "<") ) $
    spaces ) $
    identifier ) $
    spaces ) $
    (symbol ">")
   
num2 :: Parser BrackNum
num2 =
  ( ignorer
    ( ignorer
      ( keeper
        ( ignorer
          ( ignorer
              (succeed BN)
              (symbol "<")
          )
          (spaces)
        )
        (identifier)
      )
      (spaces)
    )
    (symbol ">")
  )

succeed :: a -> Parser a
succeed x =
    \str -> Right (x, str)

ignorer :: Parser keep -> Parser ignore -> Parser keep
ignorer keep ignore =
    \str ->
        case keep str of
            Right (x, more) ->
                case ignore more of
                    Right (_, end) -> Right (x, end)
                    Left e -> Left e
            Left e ->
                Left e

keeper :: Parser (a -> b) -> Parser a -> Parser b
keeper parserFunc parserArg =
    \str ->
        case parserFunc str of
            Right (x, more) ->
                case parserArg more of
                    Right (y, end) ->
                        Right (x y, end)
                    Left e ->
                        Left e
            Left e ->
                Left e

kmap f x = keeper (succeed f) x

imap f x = ignorer (succeed f) x

kimap f a b = ignorer (keeper (succeed f) a) b

ikmap f a b = keeper (ignorer (succeed f) a) b

iikmap f a b c = keeper (ignorer (ignorer (succeed f) a) b) c

kikmap f a b c = keeper (ignorer (keeper (succeed f) a) b) c

ikimap f a b c = ignorer (keeper (ignorer (succeed f) a) b) c

kiikmap f a b c d = keeper (ignorer (ignorer (keeper (succeed f) a) b) c) d

kikikmap f a b c d e =
    keeper (ignorer (keeper (ignorer (keeper (succeed f) a) b) c) d) e

kikikiimap = (((((((((((i.).i).).k).).i).).k).).i).).k.succeed
    where i = ignorer
          k = keeper

iikiimap f a b c d e = i ( i ( k (i ( i (succeed f) a) b) c) d) e
    where i = ignorer
          k = keeper

kiiikiimap func a b c d e f g = 
    i (i (k (i (i (i (k (succeed func) a) b) c) d) e) f) g
    where i = ignorer
          k = keeper

iikiiiiimap func a b c d e f g h =
    i (i (i (i (i (k (i (i (succeed func) a) b) c) d) e) f) g) h
    where i = ignorer
          k = keeper

iiiikiiiiiimap func a b c d e f g h j l m =
    i (i (i (i (i (i (k (i (i (i (i (succeed func) a) b) c) d) e) f) g) h) j) l) m
    where i = ignorer
          k = keeper
          
iiiikiiikiimap =
  (((((((((((((((((((i.).i).).k).).i).).i).).i).).k).).i).).i).).i).).i.succeed
  where i = ignorer
        k = keeper

kikiiikiiikiimap = 
  (((((((((((((((((((((((i.).i).).k).).i).).i).).i).).k).).i).).i).).i).).k).).i).).k.succeed
  where i = ignorer
        k = keeper

iikiiikiimap =
  (((((((((((((((i.).i).).k).).i).).i).).i).).k).).i).).i.succeed
  where i = ignorer
        k = keeper

iikikiimap =
  (((((((((((i.).i).).k).).i).).k).).i).).i.succeed
  where i = ignorer
        k = keeper

kiiikikikmap = (((((((((((((((k.).i).).k).).i).).k).).i).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

kikikikikmap = (((((((((((((((k.).i).).k).).i).).k).).i).).k).).i).).k.succeed
  where i = ignorer
        k = keeper

kiikkiikmap = (((((((((((((k.).i).).i).).k).).k).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

kikikiiikmap = (((((((((((((((k.).i).).i).).i).).k).).i).).k).).i).).k.succeed
  where i = ignorer
        k = keeper

kikikikmap = (((((((((((k.).i).).k).).i).).k).).i).).k.succeed
  where i = ignorer
        k = keeper

kiimap = (((i.).i).).k.succeed
  where i = ignorer
        k = keeper

kikiiikikiimap =
  (((((((((((((((((((i.).i).).k).).i).).k).).i).).i).).i).).k).).i).).k.succeed
  where i = ignorer
        k = keeper

kiiiiikikmap = ((((((((((((((((k).).i).).k).).i).).i).).i).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

kiiikmap = (((((((k.).i).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

iiikiiikmap = (((((((((((k.).i).).i).).i).).k).).i).).i.succeed
  where i = ignorer
        k = keeper

iiiikiiikmap = (((((((((((((((k.).i).).i).).i).).k).).i).).i).).i).).i.succeed
  where i = ignorer
        k = keeper

kiikikmap = (((((((((k.).i).).k).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

ikkmap = (((k.).k).).i.succeed
  where i = ignorer
        k = keeper

iikkmap = (((((k.).k).).i).).i.succeed
  where i = ignorer
        k = keeper

kiiikikmap = (((((((((((k.).i).).k).).i).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

iikikikmap = (((((((((((k.).i).).k).).i).).k).).i).).i.succeed
  where i = ignorer
        k = keeper

iikikmap = (((((((k.).i).).k).).i).).i.succeed
  where i = ignorer
        k = keeper

kiiiimap = (((((((i.).i).).i).).i).).k.succeed
  where i = ignorer
        k = keeper

iiiikmap = (((((((k.).i).).i).).i).).i.succeed
  where i = ignorer
        k = keeper

-- HELP

oneOf :: [Parser a] -> Parser a
oneOf parsers =
    case parsers of
        p:ps ->
            \str ->
                case p str of
                    Right (x, more) ->
                        Right (x, more)
                    Left _ ->
                        (oneOf ps) str
        [] ->
            \str -> Left str

identity = id

startsWith = List.isPrefixOf

keyword :: String -> Parser ()
keyword kwd =
    \str ->
        if startsWith kwd str then
            Right ((), drop (length kwd) str)
        else
            Left str

lazy :: (() -> Parser a) -> Parser a
lazy x = x ()


symbol :: String -> Parser ()
symbol kwd = keyword kwd

spaces :: Parser ()
spaces =
    \str ->
        Right ((), dropWhile (== ' ') str)


fail :: Parser a
fail = \str -> Left str

--}}}
-- {{{ helpers

optional :: Parser a -> Parser (Maybe a)
optional p =
    oneOf
        [ succeed Just
          |= p
        , succeed Nothing
        ]


nonEmptySep :: String -> Parser a -> Parser [a]
nonEmptySep sep p =
    succeed (:)
        |= p
        |. spaces
        |= list
            (succeed identity
                |. symbol sep
                |. spaces
                |= p
            )


list :: Parser a -> Parser [a]
list p =
    \str ->
        case (p |. spaces) str of
            Right (x, more) ->
                case (list p) more of
                    Right (xs, end) -> Right (x : xs, end)
            Left _ -> 
                Right ([], str)


dotted :: Parser a -> Parser [a]
dotted =
    nonEmptySep "."


brackets :: Parser Int
brackets =
    let
      bs = list b
      b = 
        ignorer (
        ignorer (
        (symbol "[") ) $
        spaces ) $
        (symbol "]")
    in
      \str ->
        case bs str of
          Right (x, rest) -> Right (length x, rest)
          Left x -> Left x



--}}}
-- {{{ Productions from §3 (Lexical Structure)


keywords :: [ String ]
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


data Identifier
    = Identifier String
    deriving (Show)

identifier :: Parser Identifier
identifier =
  \str ->
    let
      ident = takeWhile javaLetter str
    in
      if length ident > 0 then
         Right (Identifier ident, drop (length ident) str)
      else
         Left str


javaLetter :: Char -> Bool
javaLetter c =
    Char.isAlpha c || c == '_' || c == '$'


javaLetterOrDigit :: Char -> Bool
javaLetterOrDigit c =
    javaLetter c || Char.isDigit c


data TypeIdentifier
    = TypeIdentifier Identifier
    deriving (Show)


typeIdentifier :: Parser TypeIdentifier
typeIdentifier =
    kmap TypeIdentifier identifier


data UnqualifiedMethodIdentifier
    = UnqualifiedMethodIdentifier Identifier
    deriving (Show)


unqualifiedMethodIdentifier :: Parser UnqualifiedMethodIdentifier
unqualifiedMethodIdentifier =
    kmap UnqualifiedMethodIdentifier identifier


data Literal
    = Literal_IntegerLiteral Int
    | Literal_FloatingPointLiteral Float
    | Literal_BooleanLiteral Bool
    | Literal_CharacterLiteral Char
    | Literal_StringLiteral String
    | Literal_TextBlock String
    | Literal_NullLiteral
    deriving (Show)


literal :: Parser Literal
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


nullLiteral :: Parser ()
nullLiteral =
    (keyword "null")


booleanLiteral :: Parser Bool
booleanLiteral =
    oneOf
        [ imap True (keyword "true")
        , imap False (keyword "false")
        ]


characterLiteral :: Parser Char
characterLiteral =
    succeed 'c'
        -- TODO
        |. (symbol "'")


textBlock :: Parser String
textBlock =
    succeed "TODO"
        -- TODO
        |. (symbol "\"")


stringLiteral :: Parser String
stringLiteral =
    succeed "TODO"
        -- TODO
        |. (symbol "\"")


integerLiteral :: Parser Int
integerLiteral =
    succeed 1
        -- TODO
        |. (symbol "1")


floatingPointLiteral :: Parser Float
floatingPointLiteral =
    succeed 1
        -- TODO
        |. (symbol "1")



-- }}}
-- {{{ Productions from §4 (Types, Values, and Variables)


data Type
    = Type_PrimitiveType PrimitiveType
    | Type_ReferenceType ReferenceType
    deriving (Show)


type_ :: Parser Type
type_ =
    oneOf
        [ kmap Type_PrimitiveType primitiveType
        , kmap Type_ReferenceType referenceType
        ]


data PrimitiveType
    = PrimitiveType_Numeric ([ Annotation ]) NumericType
    | PrimitiveType_Boolean ([ Annotation ])
    deriving (Show)


primitiveType :: Parser PrimitiveType
primitiveType =
    keeper (
    ignorer (
    keeper (
    succeed (\annotations f -> f annotations) ) $
    list annotation ) $
    spaces ) $
    oneOf
      [ kmap (\num -> \ann -> PrimitiveType_Numeric ann num) numericType
      , imap PrimitiveType_Boolean (keyword "boolean")
      ]


data NumericType
    = NumericType_IntegralType IntegralType
    | NumericType_FloatingPointType FloatingPointType
    deriving (Show)


numericType :: Parser NumericType
numericType =
    oneOf
        [ kmap NumericType_IntegralType integralType
        , kmap NumericType_FloatingPointType floatingPointType
        ]


data IntegralType
    = IntegralType_Byte
    | IntegralType_Short
    | IntegralType_Int
    | IntegralType_Long
    | IntegralType_Char
    deriving (Show)


integralType :: Parser IntegralType
integralType =
    oneOf
        [ imap IntegralType_Byte (keyword "byte")
        , imap IntegralType_Short (keyword "short")
        , imap IntegralType_Int (keyword "int")
        , imap IntegralType_Long (keyword "long")
        , imap IntegralType_Char (keyword "char")
        ]


data FloatingPointType
    = FloatingPointType_Float
    | FloatingPointType_Double
    deriving (Show)


floatingPointType :: Parser FloatingPointType
floatingPointType =
    oneOf
        [ imap FloatingPointType_Float (keyword "float")
        , imap FloatingPointType_Double (keyword "double")
        ]


data ReferenceType
    = ReferenceType_ClassOrInterfaceType ClassOrInterfaceType
    | ReferenceType_TypeVariable TypeVariable
    | ReferenceType_ArrayType ArrayType
    deriving (Show)


referenceType :: Parser ReferenceType
referenceType =
    lazy (\_ ->
        oneOf
            [ kmap ReferenceType_ClassOrInterfaceType classOrInterfaceType
            , kmap ReferenceType_TypeVariable typeVariable
            , kmap ReferenceType_ArrayType arrayType
            ]
    )


data ClassOrInterfaceType
    = ClassOrInterfaceType_ClassType ClassType
    | ClassOrInterfaceType_InterfaceType InterfaceType
    deriving (Show)


classOrInterfaceType :: Parser ClassOrInterfaceType
classOrInterfaceType =
    oneOf
        [ kmap ClassOrInterfaceType_ClassType classType
        , kmap ClassOrInterfaceType_InterfaceType interfaceType
        ]


data ClassType
    = ClassType_NoPackage ([ Annotation ]) TypeIdentifier (Maybe TypeArguments)
    | ClassType_Package PackageName ([ Annotation ]) TypeIdentifier (Maybe TypeArguments)
    | ClassType_ClassOrInterfaceType ClassOrInterfaceType ([ Annotation ]) TypeIdentifier (Maybe TypeArguments)
    deriving (Show)


classType :: Parser ClassType
classType =
    lazy (\_ ->
        oneOf
            [ keeper (
              ignorer (
              keeper (
              ignorer (
              keeper (
              succeed ClassType_NoPackage ) $
              list annotation ) $
              spaces ) $
              typeIdentifier ) $
              spaces ) $
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
              succeed ClassType_Package ) $
              packageName ) $
              spaces ) $
              (symbol ".") ) $
              spaces ) $
              list annotation ) $
              spaces ) $
              typeIdentifier ) $
              spaces ) $
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
              succeed ClassType_ClassOrInterfaceType ) $
              classOrInterfaceType ) $
              spaces ) $
              (symbol ".") ) $
              spaces ) $
              list annotation ) $
              spaces ) $
              typeIdentifier ) $
              spaces ) $
              optional typeArguments
            ]
    )


data InterfaceType
    = InterfaceType_ClassType ClassType
    deriving (Show)


interfaceType :: Parser InterfaceType
interfaceType =
    succeed InterfaceType_ClassType
        |= classType


data TypeVariable
    = TypeVariable ([ Annotation ]) TypeIdentifier
    deriving (Show)


typeVariable :: Parser TypeVariable
typeVariable =
    keeper (
    ignorer (
    keeper (
    succeed TypeVariable ) $
    list annotation ) $
    spaces ) $
    typeIdentifier


data ArrayType
    = ArrayType_PrimitiveType PrimitiveType Dims
    | ArrayType_ClassOrInterfaceType ClassOrInterfaceType Dims
    | ArrayType_TypeVariable TypeVariable Dims
    deriving (Show)


arrayType :: Parser ArrayType
arrayType =
    oneOf
        [ keeper (
          ignorer (
          keeper (
          succeed ArrayType_PrimitiveType ) $
          primitiveType ) $
          spaces ) $
          dims
        , keeper (
          ignorer (
          keeper (
          succeed ArrayType_ClassOrInterfaceType ) $
          classOrInterfaceType ) $
          spaces ) $
          dims
        , keeper (
          ignorer (
          keeper (
          succeed ArrayType_TypeVariable ) $
          typeVariable ) $
          spaces ) $
          dims
        ]


data Dims
    = Dims [[ Annotation ]]
    deriving (Show)


dims :: Parser Dims
dims =
    succeed Dims
        |= list
            (
                ignorer (
                ignorer (
                ignorer (
                ignorer (
                list (annotation) ) $ -- ?
                spaces ) $
                (symbol "[") ) $
                spaces ) $
                (symbol "]")
            )


data TypeParameter
    = TypeParameter ([ TypeParameterModifier ]) TypeIdentifier (Maybe TypeBound)
    deriving (Show)


typeParameter :: Parser TypeParameter
typeParameter =
        keeper (
        ignorer (
        keeper (
        ignorer (
        keeper (
        succeed TypeParameter ) $
        list typeParameterModifier ) $
        spaces ) $
        typeIdentifier ) $
        spaces ) $
        optional typeBound


data TypeParameterModifier
    = TypeParameterModifier Annotation
    deriving (Show)


typeParameterModifier :: Parser TypeParameterModifier
typeParameterModifier =
    kmap TypeParameterModifier annotation


data TypeBound
    = TypeBound_TypeVariable TypeVariable
    | TypeBound_ClassOrInterfaceType ClassOrInterfaceType ([ AdditionalBound ])
    deriving (Show)


typeBound :: Parser TypeBound
typeBound =
    keeper (
    ignorer (
    ignorer (
    succeed identity ) $
    (keyword "extends") ) $
    spaces ) $
    oneOf
            [ kmap TypeBound_TypeVariable typeVariable
            , succeed TypeBound_ClassOrInterfaceType
                |= classOrInterfaceType
                |. spaces
                |= list additionalBound
            ]


data AdditionalBound
    = AdditionalBound InterfaceType
    deriving (Show)


additionalBound :: Parser AdditionalBound
additionalBound =
    iikmap AdditionalBound
        (symbol "&")
        spaces
        interfaceType


data TypeArguments
    = TypeArguments_Brackets TypeArgumentList
    deriving (Show)


typeArguments :: Parser TypeArguments
typeArguments =
    ikimap TypeArguments_Brackets
        (symbol "<")
        typeArgumentList
        (symbol ">")


data TypeArgumentList
    = TypeArgumentList ([ TypeArgument ])
    deriving (Show)


typeArgumentList :: Parser TypeArgumentList
typeArgumentList =
    kmap TypeArgumentList
        (nonEmptySep "," typeArgument)


data TypeArgument
    = TypeArgument_ReferenceType ReferenceType
    | TypeArgument_Wildcard Wildcard
    deriving (Show)


typeArgument :: Parser TypeArgument
typeArgument =
    oneOf
        [ kmap TypeArgument_ReferenceType referenceType
        , kmap TypeArgument_Wildcard wildcard
        ]


data Wildcard
    = Wildcard ([ Annotation ]) (Maybe WildcardBounds)
    deriving (Show)


wildcard :: Parser Wildcard
wildcard =
    lazy (\_ ->
        kiikmap Wildcard
            (list annotation)
            spaces
            (symbol "?")
            (optional wildcardBounds)
            )


data WildcardBounds
    = WildcardBounds_Extends ReferenceType
    | WildcardBounds_Super ReferenceType
    deriving (Show)


wildcardBounds :: Parser WildcardBounds
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


data ModuleName
    = ModuleName ([ Identifier ])
    deriving (Show)


moduleName :: Parser ModuleName
moduleName =
    kmap ModuleName (dotted identifier)


data PackageName
    = PackageName ([ Identifier ])
    deriving (Show)


packageName :: Parser PackageName
packageName =
    kmap PackageName (dotted identifier)


data TypeName
    = TypeName ([ TypeIdentifier ])
    deriving (Show)


typeName :: Parser TypeName
typeName =
    kmap TypeName (dotted typeIdentifier)


data ExpressionName
    = ExpressionName_Identifier Identifier
    | ExpressionName_AmbiguousDotIdentifier AmbiguousName Identifier
    deriving (Show)


expressionName :: Parser ExpressionName
expressionName =
    oneOf
        [ kmap ExpressionName_Identifier identifier
        , kikmap ExpressionName_AmbiguousDotIdentifier
            ambiguousName
            (ignorer spaces (ignorer (symbol ".") spaces))
            (identifier)
        ]


data MethodName
    = MethodName UnqualifiedMethodIdentifier
    deriving (Show)


methodName :: Parser MethodName
methodName =
    kmap MethodName unqualifiedMethodIdentifier


data PackageOrTypeName
    = PackageOrTypeName ([ Identifier ])
    deriving (Show)


packageOrTypeName :: Parser PackageOrTypeName
packageOrTypeName =
    kmap PackageOrTypeName (dotted identifier)


data AmbiguousName
    = AmbiguousName ([ Identifier ])
    deriving (Show)


ambiguousName :: Parser AmbiguousName
ambiguousName =
    kmap AmbiguousName (dotted identifier)



-- }}}
-- {{{ Productions from §7 (Packages and Modules)


data CompilationUnit
    = CompilationUnit_Ordinary OrdinaryCompilationUnit
    | CompilationUnit_Modular ModularCompilationUnit
    deriving (Show)


compilationUnit :: Parser CompilationUnit
compilationUnit =
    oneOf
        [ kmap CompilationUnit_Ordinary ordinaryCompilationUnit
        , kmap CompilationUnit_Modular modularCompilationUnit
        ]


data OrdinaryCompilationUnit
    = OrdinaryCompilationUnit (Maybe PackageDeclaration) ([ ImportDeclaration ]) ([ TypeDeclaration ])
    deriving (Show)


ordinaryCompilationUnit :: Parser OrdinaryCompilationUnit
ordinaryCompilationUnit =
    kikikmap OrdinaryCompilationUnit
        (optional packageDeclaration)
        spaces
        (list importDeclaration)
        spaces
        (list typeDeclaration)


data ModularCompilationUnit
    = ModularCompilationUnit ([ ImportDeclaration ]) ModuleDeclaration
    deriving (Show)


modularCompilationUnit :: Parser ModularCompilationUnit
modularCompilationUnit =
    kikmap ModularCompilationUnit
        (list importDeclaration)
        spaces
        (moduleDeclaration)


data PackageDeclaration
    = PackageDeclaration ([ PackageModifier ]) ([ Identifier ])
    deriving (Show)


packageDeclaration :: Parser PackageDeclaration
packageDeclaration =
    kiiikiimap PackageDeclaration
        (list packageModifier)
        spaces
        (keyword "package")
        spaces
        (nonEmptySep "." identifier)
        spaces
        (symbol ";")


data PackageModifier
    = PackageModifier Annotation
    deriving (Show)


packageModifier :: Parser PackageModifier
packageModifier =
    kmap PackageModifier annotation


data ImportDeclaration
    = ImportDeclaration_SingleTypeImport SingleTypeImportDeclaration
    | ImportDeclaration_TypeImportOnDemand TypeImportOnDemandDeclaration
    | ImportDeclaration_SingleStaticImport SingleStaticImportDeclaration
    | ImportDeclaration_StaticImportOnDemand StaticImportOnDemandDeclaration
    deriving (Show)


importDeclaration :: Parser ImportDeclaration
importDeclaration =
    oneOf
        [ kmap ImportDeclaration_SingleTypeImport singleTypeImportDeclaration
        , kmap ImportDeclaration_TypeImportOnDemand typeImportOnDemandDeclaration
        , kmap ImportDeclaration_SingleStaticImport singleStaticImportDeclaration
        , kmap ImportDeclaration_StaticImportOnDemand staticImportOnDemandDeclaration
        ]


data SingleTypeImportDeclaration
    = SingleTypeImportDeclaration TypeName
    deriving (Show)


singleTypeImportDeclaration :: Parser SingleTypeImportDeclaration
singleTypeImportDeclaration =
    iikiimap SingleTypeImportDeclaration
        (keyword "import")
        spaces
        typeName
        spaces
        (symbol ";")


data TypeImportOnDemandDeclaration
    = TypeImportOnDemandDeclaration PackageOrTypeName
    deriving (Show)


typeImportOnDemandDeclaration :: Parser TypeImportOnDemandDeclaration
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


data SingleStaticImportDeclaration
    = SingleStaticImportDeclaration TypeName Identifier
    deriving (Show)


singleStaticImportDeclaration :: Parser SingleStaticImportDeclaration
singleStaticImportDeclaration =
    iiiikiiikiimap SingleStaticImportDeclaration
        (keyword "import")
        spaces
        (keyword "static")
        spaces
        typeName
        spaces
        (symbol ".")
        spaces
        identifier
        spaces
        (symbol ";")


data StaticImportOnDemandDeclaration
    = StaticImportOnDemandDeclaration TypeName
    deriving (Show)


staticImportOnDemandDeclaration :: Parser StaticImportOnDemandDeclaration
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


data TypeDeclaration
    = TypeDeclaration_ClassDeclaration ClassDeclaration
    | TypeDeclaration_InterfaceDeclaration InterfaceDeclaration
    | TypeDeclaration_Semi
    deriving (Show)


typeDeclaration :: Parser TypeDeclaration
typeDeclaration =
    oneOf
        [ kmap TypeDeclaration_ClassDeclaration classDeclaration
        , kmap TypeDeclaration_InterfaceDeclaration interfaceDeclaration
        , imap TypeDeclaration_Semi (symbol ";")
        ]


data ModuleDeclaration
    = ModuleDeclaration ([ Annotation ]) (Maybe ()) ([ Identifier ]) ([ ModuleDirective ])
    deriving (Show)


moduleDeclaration :: Parser ModuleDeclaration
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


data ModuleDirective
    = ModuleDirective_Requires ([ RequiresModifier ]) ModuleName
    | ModuleDirective_Exports PackageName (Maybe ([ ModuleName ]))
    | ModuleDirective_Opens PackageName (Maybe ([ ModuleName ]))
    | ModuleDirective_Uses TypeName
    | ModuleDirective_Provides TypeName ([ TypeName ])
    deriving (Show)


moduleDirective :: Parser ModuleDirective
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


data RequiresModifier
    = RequiresModifier_Transitive
    | RequiresModifier_Static
    deriving (Show)


requiresModifier :: Parser RequiresModifier
requiresModifier =
    oneOf
        [ imap RequiresModifier_Transitive (keyword "transitive")
        , imap RequiresModifier_Static (keyword "static")
        ]


-- }}}
-- {{{ Productions from §8 (Classes)


data ClassDeclaration
    = ClassDeclaration_Normal NormalClassDeclaration
    | ClassDeclaration_Enum EnumDeclaration
    deriving (Show)


classDeclaration :: Parser ClassDeclaration
classDeclaration =
    oneOf
        [ kmap ClassDeclaration_Normal normalClassDeclaration
        , kmap ClassDeclaration_Enum enumDeclaration
        ]


data NormalClassDeclaration
    = NormalClassDeclaration ([ ClassModifier ]) TypeIdentifier (Maybe TypeParameters) (Maybe Superclass) (Maybe Superinterfaces) ClassBody
    deriving (Show)


normalClassDeclaration :: Parser NormalClassDeclaration
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
        succeed NormalClassDeclaration ) $
        list classModifier ) $
        spaces ) $
        (keyword "class") ) $
        spaces ) $
        typeIdentifier ) $
        spaces ) $
        optional typeParameters ) $
        spaces ) $
        optional superclass ) $
        spaces ) $
        optional superinterfaces ) $
        spaces ) $
        classBody

data ClassModifier
    = ClassModifier_Annotation Annotation
    | ClassModifier_Public
    | ClassModifier_Protected
    | ClassModifier_Private
    | ClassModifier_Abstract
    | ClassModifier_Static
    | ClassModifier_Final
    | ClassModifier_StrictFp
    deriving (Show)


classModifier :: Parser ClassModifier
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


data TypeParameters
    = TypeParameters TypeParameterList
    deriving (Show)


typeParameters :: Parser TypeParameters
typeParameters =
    iikiimap TypeParameters
        (symbol "<")
        spaces
        typeParameterList
        spaces
        (symbol ">")


data TypeParameterList
    = TypeParameterList ([ TypeParameter ])
    deriving (Show)


typeParameterList :: Parser TypeParameterList
typeParameterList =
    kmap TypeParameterList (nonEmptySep "," typeParameter)


data Superclass
    = Superclass ClassType
    deriving (Show)


superclass :: Parser Superclass
superclass =
    iikmap Superclass
        (keyword "extends")
        spaces
        classType


data Superinterfaces
    = Superinterfaces InterfaceTypeList
    deriving (Show)


superinterfaces :: Parser Superinterfaces
superinterfaces =
    iikmap Superinterfaces
        (keyword "implements")
        spaces
        interfaceTypeList


data InterfaceTypeList
    = InterfaceTypeList ([ InterfaceType ])
    deriving (Show)


interfaceTypeList :: Parser InterfaceTypeList
interfaceTypeList =
    kmap InterfaceTypeList
        (nonEmptySep "," interfaceType)


data ClassBody
    = ClassBody ([ ClassBodyDeclaration ])
    deriving (Show)


classBody :: Parser ClassBody
classBody =
    lazy (\_ ->
        iikiimap ClassBody
            (symbol "{")
            spaces
            (list classBodyDeclaration)
            spaces
            (symbol "}")
    )


data ClassBodyDeclaration
    = ClassBodyDeclaration_ClassMemberDeclaration ClassMemberDeclaration
    | ClassBodyDeclaration_InstanceInitializer InstanceInitializer
    | ClassBodyDeclaration_StaticInitializer StaticInitializer
    | ClassBodyDeclaration_ConstructorDeclaration ConstructorDeclaration
    deriving (Show)


classBodyDeclaration :: Parser ClassBodyDeclaration
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


data ClassMemberDeclaration
    = ClassMemberDeclaration_Field FieldDeclaration
    | ClassMemberDeclaration_Method MethodDeclaration
    | ClassMemberDeclaration_Class ClassDeclaration
    | ClassMemberDeclaration_Interface InterfaceDeclaration
    | ClassMemberDeclaration_Semi
    deriving (Show)


classMemberDeclaration :: Parser ClassMemberDeclaration
classMemberDeclaration =
    oneOf
        [ kmap ClassMemberDeclaration_Field fieldDeclaration
        , kmap ClassMemberDeclaration_Method methodDeclaration
        , kmap ClassMemberDeclaration_Class classDeclaration
        , kmap ClassMemberDeclaration_Interface interfaceDeclaration
        , imap ClassMemberDeclaration_Semi (symbol ";")
        ]


data FieldDeclaration
    = FieldDeclaration ([ FieldModifier ]) UnannType VariableDeclaratorList
    deriving (Show)


fieldDeclaration :: Parser FieldDeclaration
fieldDeclaration =
    kikikiimap FieldDeclaration
        (list fieldModifier)
        spaces
        unannType
        spaces
        variableDeclaratorList
        spaces
        (symbol ";")


data FieldModifier
    = FieldModifier_Annotation Annotation
    | FieldModifier_Public
    | FieldModifier_Protected
    | FieldModifier_Private
    | FieldModifier_Static
    | FieldModifier_Final
    | FieldModifier_Transient
    | FieldModifier_Volatile
    deriving (Show)


fieldModifier :: Parser FieldModifier
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


data VariableDeclaratorList
    = VariableDeclaratorList ([ VariableDeclarator ])
    deriving (Show)


variableDeclaratorList :: Parser VariableDeclaratorList
variableDeclaratorList =
    kmap VariableDeclaratorList
        (nonEmptySep "," variableDeclarator)


data VariableDeclarator
    = VariableDeclarator VariableDeclaratorId (Maybe VariableInitializer)
    deriving (Show)


variableDeclarator :: Parser VariableDeclarator
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


data VariableDeclaratorId
    = VariableDeclaratorId Identifier (Maybe Dims)
    deriving (Show)


variableDeclaratorId :: Parser VariableDeclaratorId
variableDeclaratorId =
    kikmap VariableDeclaratorId
        identifier
        spaces
        (optional dims)


data VariableInitializer
    = VariableInitializer_Expression Expression
    | VariableInitializer_ArrayInitializer ArrayInitializer
    deriving (Show)


variableInitializer :: Parser VariableInitializer
variableInitializer =
    lazy (\_ ->
        oneOf
            [ kmap VariableInitializer_Expression expression
            , kmap VariableInitializer_ArrayInitializer arrayInitializer
            ]
    )


data UnannType
    = UnannType_Primitive UnannPrimitiveType
    | UnannType_Reference UnannReferenceType
    deriving (Show)


unannType :: Parser UnannType
unannType =
    oneOf
        [ kmap UnannType_Primitive unannPrimitiveType
        , kmap UnannType_Reference unannReferenceType
        ]


data UnannPrimitiveType
    = UnannPrimitiveType_Numeric NumericType
    | UnannPrimitiveType_Boolean
    deriving (Show)


unannPrimitiveType :: Parser UnannPrimitiveType
unannPrimitiveType =
    oneOf
        [ kmap UnannPrimitiveType_Numeric numericType
        , imap UnannPrimitiveType_Boolean (keyword "boolean")
        ]


data UnannReferenceType
    = UnannReferenceType_Class UnannClassOrInterfaceType
    | UnannReferenceType_TypeVariable UnannTypeVariable
    | UnannReferenceType_Array UnannArrayType
    deriving (Show)


unannReferenceType :: Parser UnannReferenceType
unannReferenceType =
    oneOf
        [ kmap UnannReferenceType_Class unannClassOrInterfaceType
        , kmap UnannReferenceType_TypeVariable unannTypeVariable
        , kmap UnannReferenceType_Array unannArrayType
        ]


data UnannClassOrInterfaceType
    = UnannClassOrInterfaceType_Class UnannClassType
    | UnannClassOrInterfaceType_Interface UnannInterfaceType
    deriving (Show)


unannClassOrInterfaceType :: Parser UnannClassOrInterfaceType
unannClassOrInterfaceType =
    oneOf
        [ kmap UnannClassOrInterfaceType_Class unannClassType
        , kmap UnannClassOrInterfaceType_Interface unannInterfaceType
        ]


data UnannClassType
    = UnannClassType_TypeIdentifer TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Package PackageName ([ Annotation ]) TypeIdentifier (Maybe TypeArguments)
    | UnannClassType_Class UnannClassOrInterfaceType ([ Annotation ]) TypeIdentifier (Maybe TypeArguments)
    deriving (Show)


unannClassType :: Parser UnannClassType
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


data UnannInterfaceType
    = UnannInterfaceType UnannClassType
    deriving (Show)


unannInterfaceType :: Parser UnannInterfaceType
unannInterfaceType =
    kmap UnannInterfaceType unannClassType


data UnannTypeVariable
    = UnannTypeVariable TypeIdentifier
    deriving (Show)


unannTypeVariable :: Parser UnannTypeVariable
unannTypeVariable =
    kmap UnannTypeVariable typeIdentifier


data UnannArrayType
    = UnannArrayType_Primitive UnannPrimitiveType Dims
    | UnannArrayType_Class UnannClassOrInterfaceType Dims
    | UnannArrayType_TypeVariable UnannTypeVariable Dims
    deriving (Show)


unannArrayType :: Parser UnannArrayType
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


data MethodDeclaration
    = MethodDeclaration ([ MethodModifier ]) MethodHeader MethodBody
    deriving (Show)


methodDeclaration :: Parser MethodDeclaration
methodDeclaration =
    kikikmap MethodDeclaration
        (list methodModifier)
        spaces
        methodHeader
        spaces
        methodBody


data MethodModifier
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
    deriving (Show)


methodModifier :: Parser MethodModifier
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


data MethodHeader
    = MethodHeader_Result Result MethodDeclarator (Maybe Throws)
    | MethodHeader_TypeParameters TypeParameters ([ Annotation ]) Result MethodDeclarator (Maybe Throws)
    deriving (Show)


methodHeader :: Parser MethodHeader
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


data Result
    = Result_UnannType UnannType
    | Result_Void
    deriving (Show)


result :: Parser Result
result =
    oneOf
        [ kmap Result_UnannType unannType
        , imap Result_Void (keyword "void")
        ]


data MethodDeclarator
    = MethodDeclarator Identifier (Maybe ReceiverParameter) (Maybe FormalParameterList) (Maybe Dims)
    deriving (Show)


methodDeclarator :: Parser MethodDeclarator
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


data ReceiverParameter
    = ReceiverParameter ([ Annotation ]) UnannType (Maybe Identifier)
    deriving (Show)


receiverParameter :: Parser ReceiverParameter
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


data FormalParameterList
    = FormalParameterList ([ FormalParameter ])
    deriving (Show)


formalParameterList :: Parser FormalParameterList
formalParameterList =
    kmap FormalParameterList
        (nonEmptySep "," formalParameter)


data FormalParameter
    = FormalParameter_Normal ([ VariableModifier ]) UnannType VariableDeclaratorId
    | FormalParameter_Arity VariableArityParameter
    deriving (Show)


formalParameter :: Parser FormalParameter
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


data VariableArityParameter
    = VariableArityParameter ([ VariableModifier ]) UnannType ([ Annotation ]) Identifier
    deriving (Show)


variableArityParameter :: Parser VariableArityParameter
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


data VariableModifier
    = VariableModifier_Annotation Annotation
    | VariableModifier_Final
    deriving (Show)


variableModifier :: Parser VariableModifier
variableModifier =
    oneOf
        [ kmap VariableModifier_Annotation annotation
        , imap VariableModifier_Final (keyword "final")
        ]


data Throws
    = Throws ExceptionTypeList
    deriving (Show)


throws :: Parser Throws
throws =
    iikmap Throws
        (keyword "throws")
        spaces
        exceptionTypeList


data ExceptionTypeList
    = ExceptionTypeList ([ ExceptionType ])
    deriving (Show)


exceptionTypeList :: Parser ExceptionTypeList
exceptionTypeList =
    kmap ExceptionTypeList
        (nonEmptySep "," exceptionType)


data ExceptionType
    = ExceptionType_Class ClassType
    | ExceptionType_TypeVariable TypeVariable
    deriving (Show)


exceptionType :: Parser ExceptionType
exceptionType =
    oneOf
        [ kmap ExceptionType_Class classType
        , kmap ExceptionType_TypeVariable typeVariable
        ]


data MethodBody
    = MethodBody_Block Block
    | MethodBody_Semi
    deriving (Show)


methodBody :: Parser MethodBody
methodBody =
    oneOf
        [ kmap MethodBody_Block block
        , imap MethodBody_Semi (symbol ";")
        ]


data InstanceInitializer
    = InstanceInitializer Block
    deriving (Show)


instanceInitializer :: Parser InstanceInitializer
instanceInitializer =
    kmap InstanceInitializer block


data StaticInitializer
    = StaticInitializer Block
    deriving (Show)


staticInitializer :: Parser StaticInitializer
staticInitializer =
    iikmap StaticInitializer
        (keyword "static")
        spaces
        block


data ConstructorDeclaration
    = ConstructorDeclaration ([ ConstructorModifier ]) ConstructorDeclarator (Maybe Throws) ConstructorBody
    deriving (Show)


constructorDeclaration :: Parser ConstructorDeclaration
constructorDeclaration =
    kikikikmap ConstructorDeclaration
        (list constructorModifier)
        spaces
        constructorDeclarator
        spaces
        (optional throws)
        spaces
        constructorBody


data ConstructorModifier
    = ConstructorModifier_Annotation Annotation
    | ConstructorModifier_Public
    | ConstructorModifier_Protected
    | ConstructorModifier_Private
    deriving (Show)


constructorModifier :: Parser ConstructorModifier
constructorModifier =
    oneOf
        [ kmap ConstructorModifier_Annotation annotation
        , imap ConstructorModifier_Public (keyword "public")
        , imap ConstructorModifier_Protected (keyword "protected")
        , imap ConstructorModifier_Private (keyword "private")
        ]


data ConstructorDeclarator
    = ConstructorDeclarator (Maybe TypeParameters) SimpleTypeName (Maybe ReceiverParameter) (Maybe FormalParameterList)
    deriving (Show)


constructorDeclarator :: Parser ConstructorDeclarator
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


data SimpleTypeName
    = SimpleTypeName TypeIdentifier
    deriving (Show)


simpleTypeName :: Parser SimpleTypeName
simpleTypeName =
    kmap SimpleTypeName typeIdentifier


data ConstructorBody
    = ConstructorBody (Maybe ExplicitConstructorInvocation) (Maybe BlockStatements)
    deriving (Show)


constructorBody :: Parser ConstructorBody
constructorBody =
    iikikiimap ConstructorBody
        (symbol "{")
        spaces
        (optional explicitConstructorInvocation)
        spaces
        (optional blockStatements)
        spaces
        (symbol "}")


data ExplicitConstructorInvocation
    = ExplicitConstructorInvocation_This (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_Super (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_ExpressionSuper ExpressionName (Maybe TypeArguments) (Maybe ArgumentList)
    | ExplicitConstructorInvocation_PrimarySuper Primary (Maybe TypeArguments) (Maybe ArgumentList)
    deriving (Show)


explicitConstructorInvocation :: Parser ExplicitConstructorInvocation
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
          succeed ExplicitConstructorInvocation_This ) $
          (optional typeArguments) ) $
          spaces ) $
          (keyword "this") ) $
          spaces ) $
          (symbol "(") ) $
          spaces ) $
          (optional argumentList) ) $
          spaces ) $
          (symbol ")") ) $
          spaces ) $
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
          succeed ExplicitConstructorInvocation_Super ) $
          (optional typeArguments) ) $
          spaces ) $
          (keyword "super") ) $
          spaces ) $
          (symbol "(") ) $
          spaces ) $
          (optional argumentList) ) $
          spaces ) $
          (symbol ")") ) $
          spaces ) $
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
          succeed ExplicitConstructorInvocation_ExpressionSuper ) $
          expressionName ) $
          spaces ) $
          keyword "." ) $
          spaces ) $
          optional typeArguments ) $
          spaces ) $
          (keyword "super") ) $
          spaces ) $
          (symbol "(") ) $
          spaces ) $
          optional argumentList ) $
          spaces ) $
          (symbol ")") ) $
          spaces ) $
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
          succeed ExplicitConstructorInvocation_PrimarySuper ) $
          primary ) $
          spaces ) $
          keyword "." ) $
          spaces ) $
          optional typeArguments ) $
          spaces ) $
          (keyword "super") ) $
          spaces ) $
          (symbol "(") ) $
          spaces ) $
          optional argumentList ) $
          spaces ) $
          (symbol ")") ) $
          spaces ) $
          (symbol ";")
        ]


data EnumDeclaration
    = EnumDeclaration ([ ClassModifier ]) TypeIdentifier (Maybe Superinterfaces) EnumBody
    deriving (Show)


enumDeclaration :: Parser EnumDeclaration
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
        succeed EnumDeclaration ) $
        list classModifier ) $
        spaces ) $
        (keyword "enum") ) $
        spaces ) $
        typeIdentifier ) $
        spaces ) $
        optional superinterfaces ) $
        spaces ) $
        enumBody


data EnumBody
    = EnumBody (Maybe EnumConstantList) (Maybe EnumBodyDeclarations)
    deriving (Show)


enumBody :: Parser EnumBody
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
            succeed EnumBody ) $
            (symbol "{") ) $
            spaces ) $
            optional enumConstantList ) $
            spaces ) $
            (symbol ",") ) $
            spaces ) $
            optional enumBodyDeclarations ) $
            spaces ) $
            (symbol "{")
    )


data EnumConstantList
    = EnumConstantList EnumConstant ([ EnumConstant ])
    deriving (Show)


enumConstantList :: Parser EnumConstantList
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


data EnumConstant
    = EnumConstant ([ EnumConstantModifier ]) Identifier (Maybe (Maybe ArgumentList)) (Maybe ClassBody)
    deriving (Show)


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


data EnumConstantModifier
    = EnumConstantModifier Annotation
    deriving (Show)


enumConstantModifier :: Parser EnumConstantModifier
enumConstantModifier =
    succeed EnumConstantModifier
        |= annotation


data EnumBodyDeclarations
    = EnumBodyDeclarations ([ ClassBodyDeclaration ])
    deriving (Show)


enumBodyDeclarations :: Parser EnumBodyDeclarations
enumBodyDeclarations =
    iikmap EnumBodyDeclarations
        (symbol ";")
        spaces
        (list classBodyDeclaration)



-- }}}
-- {{{ Productions from §9 (Interfaces)


data InterfaceDeclaration
    = InterfaceDeclaration_Normal NormalInterfaceDeclaration
    | InterfaceDeclaration_Annotation AnnotationTypeDeclaration
    deriving (Show)


interfaceDeclaration :: Parser InterfaceDeclaration
interfaceDeclaration =
    oneOf
        [ kmap InterfaceDeclaration_Normal normalInterfaceDeclaration
        , kmap InterfaceDeclaration_Annotation annotationTypeDeclaration
        ]


data NormalInterfaceDeclaration
    = NormalInterfaceDeclaration ([ InterfaceModifier ]) TypeIdentifier (Maybe TypeParameters) (Maybe ExtendsInterfaces) InterfaceBody
    deriving (Show)


normalInterfaceDeclaration :: Parser NormalInterfaceDeclaration
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
        succeed NormalInterfaceDeclaration ) $
        list interfaceModifier ) $
        spaces ) $
        (keyword "interface") ) $
        spaces ) $
        typeIdentifier ) $
        spaces ) $
        optional typeParameters ) $
        spaces ) $
        optional extendsInterfaces ) $
        spaces ) $
        interfaceBody


data InterfaceModifier
    = InterfaceModifier_Annotation Annotation
    | InterfaceModifier_Public
    | InterfaceModifier_Protected
    | InterfaceModifier_Private
    | InterfaceModifier_Abstract
    | InterfaceModifier_Static
    | InterfaceModifier_Strictfp
    deriving (Show)


interfaceModifier :: Parser InterfaceModifier
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


data ExtendsInterfaces
    = ExtendsInterfaces InterfaceTypeList
    deriving (Show)


extendsInterfaces :: Parser ExtendsInterfaces
extendsInterfaces =
    iikmap ExtendsInterfaces
        (keyword "extends")
        spaces
        interfaceTypeList


data InterfaceBody
    = InterfaceBody ([ InterfaceMemberDeclaration ])
    deriving (Show)


interfaceBody :: Parser InterfaceBody
interfaceBody =
    lazy (\_ -> 
            iikiimap InterfaceBody
                (keyword "{")
                spaces
                (list interfaceMemberDeclaration)
                spaces
                (keyword "}")
    )


data InterfaceMemberDeclaration
    = InterfaceMemberDeclaration_Constant ConstantDeclaration
    | InterfaceMemberDeclaration_Method InterfaceMethodDeclaration
    | InterfaceMemberDeclaration_Class ClassDeclaration
    | InterfaceMemberDeclaration_Interface InterfaceDeclaration
    | InterfaceMemberDeclaration_Semi
    deriving (Show)


interfaceMemberDeclaration :: Parser InterfaceMemberDeclaration
interfaceMemberDeclaration =
    oneOf
        [ kmap InterfaceMemberDeclaration_Constant constantDeclaration
        , kmap InterfaceMemberDeclaration_Method interfaceMethodDeclaration
        , kmap InterfaceMemberDeclaration_Class classDeclaration
        , kmap InterfaceMemberDeclaration_Interface interfaceDeclaration
        , imap InterfaceMemberDeclaration_Semi (symbol ",")
        ]


data ConstantDeclaration
    = ConstantDeclaration ([ ConstantModifier ]) UnannType VariableDeclaratorList
    deriving (Show)


constantDeclaration :: Parser ConstantDeclaration
constantDeclaration =
    kikikiimap ConstantDeclaration
        (list constantModifier)
        spaces
        unannType
        spaces
        variableDeclaratorList
        spaces
        (symbol ";")


data ConstantModifier
    = ConstantModifier_Annotation Annotation
    | ConstantModifier_Public
    | ConstantModifier_Static
    | ConstantModifier_Final
    deriving (Show)


constantModifier :: Parser ConstantModifier
constantModifier =
    oneOf
        [ kmap ConstantModifier_Annotation annotation
        , imap ConstantModifier_Public (keyword "public")
        , imap ConstantModifier_Static (keyword "static")
        , imap ConstantModifier_Final (keyword "final")
        ]


data InterfaceMethodDeclaration
    = InterfaceMethodDeclaration ([ InterfaceMethodModifier ]) MethodHeader MethodBody
    deriving (Show)


interfaceMethodDeclaration :: Parser InterfaceMethodDeclaration
interfaceMethodDeclaration =
    kikikmap InterfaceMethodDeclaration
        (list interfaceMethodModifier)
        spaces
        methodHeader
        spaces
        methodBody


data InterfaceMethodModifier
    = InterfaceMethodModifier_Annotation Annotation
    | InterfaceMethodModifier_Public
    | InterfaceMethodModifier_Private
    | InterfaceMethodModifier_Abstract
    | InterfaceMethodModifier_Default
    | InterfaceMethodModifier_Static
    | InterfaceMethodModifier_Strictfp
    deriving (Show)


interfaceMethodModifier :: Parser InterfaceMethodModifier
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


data AnnotationTypeDeclaration
    = AnnotationTypeDeclaration ([ InterfaceModifier ]) TypeIdentifier AnnotationTypeBody
    deriving (Show)


annotationTypeDeclaration :: Parser AnnotationTypeDeclaration
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


data AnnotationTypeBody
    = AnnotationTypeBody ([ AnnotationTypeMemberDeclaration ])
    deriving (Show)


annotationTypeBody :: Parser AnnotationTypeBody
annotationTypeBody =
    lazy (\_ ->
        iikiimap AnnotationTypeBody
            (symbol "{")
            spaces
            (list annotationTypeMemberDeclaration)
            spaces
            (symbol "}")
    )


data AnnotationTypeMemberDeclaration
    = AnnotationTypeMemberDeclaration_Element AnnotationTypeElementDeclaration
    | AnnotationTypeMemberDeclaration_Constant ConstantDeclaration
    | AnnotationTypeMemberDeclaration_Class ClassDeclaration
    | AnnotationTypeMemberDeclaration_Interface InterfaceDeclaration
    | AnnotationTypeMemberDeclaration_Semi
    deriving (Show)


annotationTypeMemberDeclaration :: Parser AnnotationTypeMemberDeclaration
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


data AnnotationTypeElementDeclaration
    = AnnotationTypeElementDeclaration ([ AnnotationTypeElementModifier ]) UnannType Identifier (Maybe Dims) (Maybe DefaultValue)
    deriving (Show)


annotationTypeElementDeclaration :: Parser AnnotationTypeElementDeclaration
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
        succeed AnnotationTypeElementDeclaration ) $
        list annotationTypeElementModifier ) $
        spaces ) $
        unannType ) $
        spaces ) $
        identifier ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        optional dims ) $
        spaces ) $
        optional defaultValue ) $
        spaces ) $
        (symbol ";")


data AnnotationTypeElementModifier
    = AnnotationTypeElementModifier_Annotation Annotation
    | AnnotationTypeElementModifier_Public
    | AnnotationTypeElementModifier_Abstract
    deriving (Show)


annotationTypeElementModifier :: Parser AnnotationTypeElementModifier
annotationTypeElementModifier =
    oneOf
        [ kmap AnnotationTypeElementModifier_Annotation annotation
        , imap AnnotationTypeElementModifier_Public (keyword "public")
        , imap AnnotationTypeElementModifier_Abstract (keyword "abstract")
        ]


data DefaultValue
    = DefaultValue ElementValue
    deriving (Show)


defaultValue :: Parser DefaultValue
defaultValue =
    iikmap DefaultValue
        (keyword "default")
        spaces
        elementValue


data Annotation
    = Annotation_Normal NormalAnnotation
    | Annotation_Marker MarkerAnnotation
    | Annotation_SingleElement SingleElementAnnotation
    deriving (Show)


annotation :: Parser Annotation
annotation =
    lazy (\_ ->
        oneOf
            [ kmap Annotation_Normal normalAnnotation
            , kmap Annotation_Marker markerAnnotation
            , kmap Annotation_SingleElement singleElementAnnotation
            ]
    )


data NormalAnnotation
    = NormalAnnotation TypeName (Maybe ElementValuePairList)
    deriving (Show)


normalAnnotation :: Parser NormalAnnotation
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
        succeed NormalAnnotation ) $
        (symbol "@") ) $
        spaces ) $
        typeName ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        (optional elementValuePairList) ) $
        spaces ) $
        (symbol ")")


data ElementValuePairList
    = ElementValuePairList ElementValuePair ([ ElementValuePair ])
    deriving (Show)


elementValuePairList :: Parser ElementValuePairList
elementValuePairList =
    kikmap ElementValuePairList
        elementValuePair
        spaces
        (list
            (succeed identity
                |. (symbol ",")
                |. spaces
                |= elementValuePair
            )
        )


data ElementValuePair
    = ElementValuePair Identifier ElementValue
    deriving (Show)


elementValuePair :: Parser ElementValuePair
elementValuePair =
    kiiikmap ElementValuePair
        identifier
        spaces
        (symbol "=")
        spaces
        elementValue


data ElementValue
    = ElementValue_Conditional ConditionalExpression
    | ElementValue_ArrayInitializer ElementValueArrayInitializer
    | ElementValue_Annotation Annotation
    deriving (Show)


elementValue :: Parser ElementValue
elementValue =
    lazy (\_ ->
        oneOf
            --[ succeed ElementValue_Conditional
            --    |= conditionalExpression
            [ kmap ElementValue_ArrayInitializer elementValueArrayInitializer
            , kmap ElementValue_Annotation annotation
            ]
    )


data ElementValueArrayInitializer
    = ElementValueArrayInitializer (Maybe ElementValueList)
    deriving (Show)


elementValueArrayInitializer :: Parser ElementValueArrayInitializer
elementValueArrayInitializer =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed ElementValueArrayInitializer ) $
        (symbol "{") ) $
        spaces ) $
        (optional (elementValueList)) ) $
        spaces ) $
        (optional (symbol ",")) ) $
        spaces ) $
        (symbol "}")


data ElementValueList
    = ElementValueList ElementValue ([ ElementValue ])
    deriving (Show)


elementValueList :: Parser ElementValueList
elementValueList =
    kikmap ElementValueList
        elementValue
        spaces
        (list
            (succeed identity
                |. (symbol ",")
                |. spaces
                |= elementValue
            )
        )


data MarkerAnnotation
    = MarkerAnnotation TypeName
    deriving (Show)


markerAnnotation :: Parser MarkerAnnotation
markerAnnotation =
    ikmap MarkerAnnotation
        (symbol "@")
        typeName


data SingleElementAnnotation
    = SingleElementAnnotation TypeName ElementValue
    deriving (Show)


singleElementAnnotation :: Parser SingleElementAnnotation
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
        succeed SingleElementAnnotation ) $
        (symbol "@") ) $
        spaces ) $
        typeName ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        elementValue ) $
        spaces ) $
        (symbol ")")



-- }}}
-- {{{ Productions from §10 (Arrays)


data ArrayInitializer
    = ArrayInitializer (Maybe VariableInitializerList)
    deriving (Show)


arrayInitializer :: Parser ArrayInitializer
arrayInitializer =
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        succeed ArrayInitializer ) $
        (symbol "{") ) $
        optional (variableInitializerList) ) $
        spaces ) $
        optional (symbol ",") ) $
        (symbol "}")


data VariableInitializerList
    = VariableInitializerList VariableInitializer ([ VariableInitializer ])
    deriving (Show)


variableInitializerList :: Parser VariableInitializerList
variableInitializerList =
    kikmap VariableInitializerList
        variableInitializer
        spaces
        (list
            (succeed identity
                |. (symbol ",")
                |. spaces
                |= variableInitializer
            )
        )



-- }}}
-- {{{ Productions from §14 (Blocks and Statements)


data Block
    = Block (Maybe BlockStatements)
    deriving (Show)


block :: Parser Block
block =
    ikimap Block
        (symbol "{")
        (optional blockStatements)
        (symbol "}")


data BlockStatements
    = BlockStatements BlockStatement ([ BlockStatement ])
    deriving (Show)


blockStatements :: Parser BlockStatements
blockStatements =
    kikmap BlockStatements
        blockStatement
        spaces
        (list blockStatement)


data BlockStatement
    = BlockStatement_LocalVariable LocalVariableDeclarationStatement
    | BlockStatement_Class ClassDeclaration
    | BlockStatement_Statement Statement
    deriving (Show)


blockStatement :: Parser BlockStatement
blockStatement =
    oneOf
        [ kmap BlockStatement_LocalVariable localVariableDeclarationStatement
        , kmap BlockStatement_Class (lazy (\_ -> classDeclaration))
        , kmap BlockStatement_Statement statement
        ]


data LocalVariableDeclarationStatement
    = LocalVariableDeclarationStatement LocalVariableDeclaration
    deriving (Show)


localVariableDeclarationStatement :: Parser LocalVariableDeclarationStatement
localVariableDeclarationStatement =
    kiimap LocalVariableDeclarationStatement
        localVariableDeclaration
        spaces
        (symbol ";")


data LocalVariableDeclaration
    = LocalVariableDeclaration ([ VariableModifier ]) LocalVariableType VariableDeclaratorList
    deriving (Show)


localVariableDeclaration :: Parser LocalVariableDeclaration
localVariableDeclaration =
    kikikmap LocalVariableDeclaration
        (list variableModifier)
        spaces
        localVariableType
        spaces
        variableDeclaratorList


data LocalVariableType
    = LocalVariableType_UnannType UnannType
    | LocalVariableType_Var
    deriving (Show)


localVariableType :: Parser LocalVariableType
localVariableType =
    oneOf
        [ kmap LocalVariableType_UnannType unannType
        , imap LocalVariableType_Var (keyword "var")
        ]


data Statement
    = Statement_Statement StatementWithoutTrailingSubstatement
    | Statement_Labeled LabeledStatement
    | Statement_If IfThenStatement
    | Statement_IfThenElse IfThenElseStatement
    | Statement_While WhileStatement
    | Statement_For ForStatement
    deriving (Show)


statement :: Parser Statement
statement =
    oneOf
        [ kmap Statement_Statement statementWithoutTrailingSubstatement
        , kmap Statement_Labeled labeledStatement
        , kmap Statement_If ifThenStatement
        , kmap Statement_IfThenElse ifThenElseStatement
        , kmap Statement_While whileStatement
        , kmap Statement_For forStatement
        ]


data StatementNoShortIf
    = StatementNoShortIf_NoTrailing StatementWithoutTrailingSubstatement
    | StatementNoShortIf_Labeled LabeledStatementNoShortIf
    | StatementNoShortIf_IfThenElse IfThenElseStatementNoShortIf
    | StatementNoShortIf_While WhileStatementNoShortIf
    | StatementNoShortIf_For ForStatementNoShortIf
    deriving (Show)


statementNoShortIf :: Parser StatementNoShortIf
statementNoShortIf =
    oneOf
        [ kmap StatementNoShortIf_NoTrailing statementWithoutTrailingSubstatement
        , kmap StatementNoShortIf_Labeled labeledStatementNoShortIf
        , kmap StatementNoShortIf_IfThenElse ifThenElseStatementNoShortIf
        , kmap StatementNoShortIf_While whileStatementNoShortIf
        , kmap StatementNoShortIf_For forStatementNoShortIf
        ]


data StatementWithoutTrailingSubstatement
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
    deriving (Show)


statementWithoutTrailingSubstatement :: Parser StatementWithoutTrailingSubstatement
statementWithoutTrailingSubstatement =
    oneOf
        [ kmap StatementWithoutTrailingSubstatement_Block
             (lazy (\_ -> block))
        , kmap StatementWithoutTrailingSubstatement_Empty
             emptyStatement
        , kmap StatementWithoutTrailingSubstatement_Expression
             expressionStatement
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


data EmptyStatement
    = EmptyStatement
    deriving (Show)


emptyStatement :: Parser EmptyStatement
emptyStatement =
    imap EmptyStatement (symbol ";")


data LabeledStatement
    = LabeledStatement Identifier Statement
    deriving (Show)


labeledStatement :: Parser LabeledStatement
labeledStatement =
    kiiikmap LabeledStatement
        identifier
        spaces
        (symbol ":")
        spaces
        (lazy (\_ -> statement))


data LabeledStatementNoShortIf
    = LabeledStatementNoShortIf Identifier StatementNoShortIf
    deriving (Show)


labeledStatementNoShortIf :: Parser LabeledStatementNoShortIf
labeledStatementNoShortIf =
    kiiikmap LabeledStatementNoShortIf
        identifier
        spaces
        (symbol ":")
        spaces
        (lazy (\_ -> statementNoShortIf))


data ExpressionStatement
    = ExpressionStatement StatementExpression
    deriving (Show)


expressionStatement :: Parser ExpressionStatement
expressionStatement =
    kiimap ExpressionStatement
        statementExpression
        spaces
        (symbol ";")


data StatementExpression
    = StatementExpression_Assignment Assignment
    | StatementExpression_PreIncrement PreIncrementExpression
    | StatementExpression_PreDecrement PreDecrementExpression
    | StatementExpression_PostIncrement PostIncrementExpression
    | StatementExpression_PostDecrement PostDecrementExpression
    | StatementExpression_MethodInvocation MethodInvocation
    | StatementExpression_ClassCreation ClassInstanceCreationExpression
    deriving (Show)


statementExpression :: Parser StatementExpression
statementExpression =
    oneOf
        [ kmap StatementExpression_Assignment
             assignment
        , kmap StatementExpression_PreIncrement
             preIncrementExpression
        , kmap StatementExpression_PreDecrement
             preDecrementExpression
        , kmap StatementExpression_PostIncrement
             postIncrementExpression
        , kmap StatementExpression_PostDecrement
             postDecrementExpression
        , kmap StatementExpression_MethodInvocation
             methodInvocation
        , kmap StatementExpression_ClassCreation
             classInstanceCreationExpression
        ]


data IfThenStatement
    = IfThenStatement Expression Statement
    deriving (Show)


ifThenStatement :: Parser IfThenStatement
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
        succeed IfThenStatement ) $
        (keyword "if") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        (lazy (\_ -> statement))


data IfThenElseStatement
    = IfThenElseStatement Expression StatementNoShortIf Statement
    deriving (Show)


ifThenElseStatement :: Parser IfThenElseStatement
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
        succeed IfThenElseStatement ) $
        (keyword "if") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        statementNoShortIf ) $
        spaces ) $
        (keyword "else") ) $
        spaces ) $
        lazy (\_ -> statement)


data IfThenElseStatementNoShortIf
    = IfThenElseStatementNoShortIf Expression StatementNoShortIf StatementNoShortIf
    deriving (Show)


ifThenElseStatementNoShortIf :: Parser IfThenElseStatementNoShortIf
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
        succeed IfThenElseStatementNoShortIf ) $
        (keyword "if") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        lazy (\_ -> statementNoShortIf) ) $
        spaces ) $
        (keyword "else") ) $
        spaces ) $
        lazy (\_ -> statementNoShortIf)


data AssertStatement
    = AssertStatement_Expression Expression
    | AssertStatement_WithError Expression Expression
    deriving (Show)


assertStatement :: Parser AssertStatement
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
          succeed AssertStatement_WithError) $
          (keyword "assert")) $
          spaces) $
          expression) $
          spaces) $
          (symbol ":")) $
          spaces) $
          expression) $
          spaces) $
          (symbol ";")
        ]


data SwitchStatement
    = SwitchStatement Expression SwitchBlock
    deriving (Show)


switchStatement :: Parser SwitchStatement
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
        succeed SwitchStatement ) $
        (keyword "switch") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        switchBlock


data SwitchBlock
    = SwitchBlock_Rule SwitchRule ([ SwitchRule ])
    | SwitchBlock_Group ([ SwitchBlockStatementGroup ]) ([ SwitchLabel ])
    deriving (Show)


switchBlock :: Parser SwitchBlock
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


data SwitchRule
    = SwitchRule_Expression SwitchLabel Expression
    | SwitchRule_Block SwitchLabel Block
    | SwitchRule_Throw SwitchLabel ThrowStatement
    deriving (Show)


switchRule :: Parser SwitchRule
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


data SwitchBlockStatementGroup
    = SwitchBlockStatementGroup SwitchLabel ([ SwitchLabel ]) BlockStatements
    deriving (Show)


switchBlockStatementGroup :: Parser SwitchBlockStatementGroup
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


data SwitchLabel
    = SwitchLabel_Case CaseConstant ([ CaseConstant ])
    | SwitchLabel_Default
    deriving (Show)


switchLabel :: Parser SwitchLabel
switchLabel =
    oneOf
        [ iikkmap SwitchLabel_Case
            (keyword "case")
            spaces
            caseConstant
            (list
                (succeed identity
                    |. (symbol ",")
                    |. spaces
                    |= caseConstant
                )
            )
        , imap SwitchLabel_Default
            (keyword "default")
        ]


data CaseConstant
    = CaseConstant ConditionalExpression
    deriving (Show)


caseConstant :: Parser CaseConstant
caseConstant =
    kmap CaseConstant (lazy (\_ -> conditionalExpression))


data WhileStatement
    = WhileStatement Expression Statement
    deriving (Show)


whileStatement :: Parser WhileStatement
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


data WhileStatementNoShortIf
    = WhileStatementNoShortIf Expression StatementNoShortIf
    deriving (Show)


whileStatementNoShortIf :: Parser WhileStatementNoShortIf
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


data DoStatement
    = DoStatement Statement Expression
    deriving (Show)


doStatement :: Parser DoStatement
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
        succeed DoStatement ) $
        (keyword "do") ) $
        spaces ) $
        lazy (\_ -> statement) ) $
        spaces ) $
        (keyword "while") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        (symbol ";")


data ForStatement
    = ForStatement_Basic BasicForStatement
    | ForStatement_Enhanced EnhancedForStatement
    deriving (Show)


forStatement :: Parser ForStatement
forStatement =
    oneOf
        [ kmap ForStatement_Basic basicForStatement
        , kmap ForStatement_Enhanced enhancedForStatement
        ]


data ForStatementNoShortIf
    = ForStatementNoShortIf_Basic BasicForStatementNoShortIf
    | ForStatementNoShortIf_Enhanced EnhancedForStatementNoShortIf
    deriving (Show)


forStatementNoShortIf :: Parser ForStatementNoShortIf
forStatementNoShortIf =
    oneOf
        [ kmap ForStatementNoShortIf_Basic
            basicForStatementNoShortIf
        , kmap ForStatementNoShortIf_Enhanced
            enhancedForStatementNoShortIf
        ]


data BasicForStatement
    = BasicForStatement (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) Statement
    deriving (Show)


basicForStatement :: Parser BasicForStatement
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
        succeed BasicForStatement ) $
        (keyword "for") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        optional forInit ) $
        spaces ) $
        (symbol ";") ) $
        spaces ) $
        optional expression ) $
        spaces ) $
        (symbol ";") ) $
        spaces ) $
        optional forUpdate ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        lazy (\_ -> statement)


data BasicForStatementNoShortIf
    = BasicForStatementNoShortIf (Maybe ForInit) (Maybe Expression) (Maybe ForUpdate) StatementNoShortIf
    deriving (Show)


basicForStatementNoShortIf :: Parser BasicForStatementNoShortIf
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
        succeed BasicForStatementNoShortIf ) $
        (keyword "for") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        optional forInit ) $
        spaces ) $
        (symbol ";") ) $
        spaces ) $
        optional expression ) $
        spaces ) $
        (symbol ";") ) $
        spaces ) $
        optional forUpdate ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        lazy (\_ -> statementNoShortIf)


data ForInit
    = ForInit_StatementList StatementExpressionList
    | ForInit_Variable LocalVariableDeclaration
    deriving (Show)


forInit :: Parser ForInit
forInit =
    oneOf
        [ kmap ForInit_StatementList
            statementExpressionList
        , kmap ForInit_Variable
            localVariableDeclaration
        ]


data ForUpdate
    = ForUpdate StatementExpressionList
    deriving (Show)


forUpdate :: Parser ForUpdate
forUpdate = kmap ForUpdate statementExpressionList


data StatementExpressionList
    = StatementExpressionList StatementExpression ([ StatementExpression ])
    deriving (Show)


statementExpressionList :: Parser StatementExpressionList
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


data EnhancedForStatement
    = EnhancedForStatement ([ VariableModifier ]) LocalVariableType VariableDeclaratorId Expression Statement
    deriving (Show)


enhancedForStatement :: Parser EnhancedForStatement
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
        succeed EnhancedForStatement ) $
        (keyword "for") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        list variableModifier ) $
        spaces ) $
        localVariableType ) $
        spaces ) $
        variableDeclaratorId ) $
        spaces ) $
        (symbol ":") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        lazy (\_ -> statement)


data EnhancedForStatementNoShortIf
    = EnhancedForStatementNoShortIf ([ VariableModifier ]) LocalVariableType VariableDeclaratorId Expression StatementNoShortIf
    deriving (Show)


enhancedForStatementNoShortIf :: Parser EnhancedForStatementNoShortIf
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
        succeed EnhancedForStatementNoShortIf ) $
        (keyword "for") ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        list variableModifier ) $
        spaces ) $
        localVariableType ) $
        spaces ) $
        variableDeclaratorId ) $
        spaces ) $
        (symbol ":") ) $
        spaces ) $
        expression ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        lazy (\_ -> statementNoShortIf)


data BreakStatement
    = BreakStatement (Maybe Identifier)
    deriving (Show)


breakStatement :: Parser BreakStatement
breakStatement =
    iikiimap BreakStatement
        (keyword "break")
        spaces
        (optional identifier)
        spaces
        (symbol ";")


data YieldStatement
    = YieldStatement Expression
    deriving (Show)


yieldStatement :: Parser YieldStatement
yieldStatement =
    iikiimap YieldStatement
        (keyword "yield")
        spaces
        expression
        spaces
        (symbol ";")


data ContinueStatement
    = ContinueStatement (Maybe Identifier)
    deriving (Show)


continueStatement :: Parser ContinueStatement
continueStatement = 
    iikiimap ContinueStatement
        (keyword "continue")
        spaces
        (optional identifier)
        spaces
        (symbol ";")

data ReturnStatement
    = ReturnStatement (Maybe Expression)
    deriving (Show)


returnStatement :: Parser ReturnStatement
returnStatement =
    iikiimap ReturnStatement
        (keyword "return")
        spaces
        (optional expression)
        spaces
        (symbol ";")

data ThrowStatement
    = ThrowStatement Expression
    deriving (Show)


throwStatement :: Parser ThrowStatement
throwStatement =
    iikiimap ThrowStatement
        (keyword "throw")
        spaces
        expression
        spaces
        (symbol ";")


data SynchronizedStatement
    = SynchronizedStatement Expression Block
    deriving (Show)


synchronizedStatement :: Parser SynchronizedStatement
synchronizedStatement =
    iiiikiiikmap SynchronizedStatement
        (keyword "synchronized")
        spaces
        (symbol "(")
        spaces
        expression
        spaces
        (symbol "(")
        spaces
        (lazy (\_ -> block))


data TryStatement
    = TryStatement_Normal Block Catches
    | TryStatement_Finally Block (Maybe Catches) Finally
    | TryStatement_With TryWithResourcesStatement
    deriving (Show)


tryStatement :: Parser TryStatement
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


data Catches
    = Catches CatchClause ([ CatchClause ])
    deriving (Show)


catches :: Parser Catches
catches =
    kikmap Catches
        catchClause
        spaces
        (list catchClause)


data CatchClause
    = CatchClause CatchFormalParameter Block
    deriving (Show)


catchClause :: Parser CatchClause
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


data CatchFormalParameter
    = CatchFormalParameter ([ VariableModifier ]) CatchType VariableDeclaratorId
    deriving (Show)


catchFormalParameter :: Parser CatchFormalParameter
catchFormalParameter =
    kikikmap CatchFormalParameter
        (list variableModifier)
        spaces
        catchType
        spaces
        variableDeclaratorId


data CatchType
    = CatchType UnannClassType ([ ClassType ])
    deriving (Show)


catchType :: Parser CatchType
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


data Finally
    = Finally Block
    deriving (Show)


finally :: Parser Finally
finally =
    iikmap Finally
        (succeed "finally")
        spaces
        (lazy (\_ -> block))


data TryWithResourcesStatement
    = TryWithResourcesStatement ResourceSpecification Block (Maybe Catches) (Maybe Finally)
    deriving (Show)


tryWithResourcesStatement :: Parser TryWithResourcesStatement
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
        succeed TryWithResourcesStatement ) $
        (keyword "try") ) $
        spaces ) $
        resourceSpecification ) $
        spaces ) $
        (lazy (\_ -> block)) ) $
        spaces ) $
        (optional catches) ) $
        spaces ) $
        (optional finally)


data ResourceSpecification
    = ResourceSpecification ResourceList
    deriving (Show)


resourceSpecification :: Parser ResourceSpecification
resourceSpecification =
        ignorer (
        ignorer (
        ignorer (
        ignorer (
        keeper (
        ignorer (
        ignorer (
        succeed ResourceSpecification ) $
        (symbol "(") ) $
        spaces ) $
        resourceList ) $
        spaces ) $
        (optional (symbol ";")) ) $
        spaces ) $
        (symbol ")")


data ResourceList
    = ResourceList Resource ([ Resource ])
    deriving (Show)


resourceList :: Parser ResourceList
resourceList =
    kikmap ResourceList
        resource
        spaces
        (list
            (succeed identity
                |. (symbol ";")
                |. spaces
                |= resource
            )
        )


data Resource
    = Resource_Declaration ([ VariableModifier ]) LocalVariableType Identifier Expression
    | Resource_VariableAccess VariableAccess
    deriving (Show)


resource :: Parser Resource
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
          succeed Resource_Declaration ) $
          list variableModifier ) $
          spaces ) $
          localVariableType ) $
          spaces ) $
          identifier ) $
          spaces ) $
          (symbol "=") ) $
          spaces ) $
          expression
        , kmap Resource_VariableAccess
            variableAccess
        ]


data VariableAccess
    = VariableAccess_Expression ExpressionName
    | VariableAccess_Field FieldAccess
    deriving (Show)


variableAccess :: Parser VariableAccess
variableAccess =
    oneOf
        [ kmap VariableAccess_Expression
            expressionName
        , kmap VariableAccess_Field
            fieldAccess
        ]



-- }}}
-- {{{ Productions from §15 (Expressions)


data Primary
    = Primary_NoNewArray PrimaryNoNewArray
    | Primary_Creation ArrayCreationExpression
    deriving (Show)


primary :: Parser Primary
primary =
    oneOf
        [ kmap Primary_NoNewArray
            primaryNoNewArray
        , kmap Primary_Creation
            arrayCreationExpression
        ]


data PrimaryNoNewArray
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
    deriving (Show)


primaryNoNewArray :: Parser PrimaryNoNewArray
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
        , kmap PrimaryNoNewArray_FieldAccess
             fieldAccess
        , kmap PrimaryNoNewArray_ArrayAccess
             (lazy (\_ -> arrayAccess)) -- ?
        , kmap PrimaryNoNewArray_MethodInvocation
             methodInvocation
        , kmap PrimaryNoNewArray_MethodReference
             methodReference
        ]


data ClassLiteral
    = ClassLiteral_TypeName TypeName Int
    | ClassLiteral_Numeric NumericType Int
    | ClassLiteral_Boolean Int
    | ClassLiteral_Void
    deriving (Show)


classLiteral :: Parser ClassLiteral
classLiteral =
    oneOf
        [ ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          keeper (
          succeed ClassLiteral_TypeName ) $
          typeName ) $
          spaces ) $
          brackets ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          keeper (
          succeed ClassLiteral_Numeric ) $
          numericType ) $
          spaces ) $
          brackets ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          succeed ClassLiteral_Boolean ) $
          (keyword "boolean") ) $
          spaces ) $
          brackets ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          (keyword "class")
        , ignorer (
          ignorer (
          ignorer (
          ignorer (
          ignorer (
          succeed ClassLiteral_Void ) $
          (keyword "void") ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          (keyword "class")
        ]


data ClassInstanceCreationExpression
    = ClassInstanceCreationExpression_Normal UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Expression ExpressionName UnqualifiedClassInstanceCreationExpression
    | ClassInstanceCreationExpression_Primary Primary UnqualifiedClassInstanceCreationExpression
    deriving (Show)


classInstanceCreationExpression :: Parser ClassInstanceCreationExpression
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
        , kiiikmap ClassInstanceCreationExpression_Primary
            (lazy (\_ -> primary))
            spaces
            (symbol ".")
            spaces
            unqualifiedClassInstanceCreationExpression
        ]


data UnqualifiedClassInstanceCreationExpression
    = UnqualifiedClassInstanceCreationExpression (Maybe TypeArguments) ClassOrInterfaceTypeToInstantiate (Maybe ArgumentList) (Maybe ClassBody)
    deriving (Show)


unqualifiedClassInstanceCreationExpression :: Parser UnqualifiedClassInstanceCreationExpression
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
        succeed UnqualifiedClassInstanceCreationExpression ) $
        (keyword "new") ) $
        spaces ) $
        optional typeArguments ) $
        spaces ) $
        classOrInterfaceTypeToInstantiate ) $
        spaces ) $
        (symbol "(") ) $
        spaces ) $
        optional argumentList ) $
        spaces ) $
        (symbol ")") ) $
        spaces ) $
        optional (lazy (\_ -> classBody))


data ClassOrInterfaceTypeToInstantiate
    = ClassOrInterfaceTypeToInstantiate ([ Annotation ]) Identifier ([( [Annotation], Identifier )]) (Maybe TypeArgumentsOrDiamond)
    deriving (Show)


classOrInterfaceTypeToInstantiate :: Parser ClassOrInterfaceTypeToInstantiate
classOrInterfaceTypeToInstantiate =
    kikikikmap ClassOrInterfaceTypeToInstantiate
        (list annotation)
        spaces
        identifier
        spaces
        (list
            (iikikmap (,)
                (symbol ".")
                spaces
                (list annotation)
                spaces
                identifier
            )
        )
        spaces
        (optional typeArgumentsOrDiamond)


data TypeArgumentsOrDiamond
    = TypeArguments_TypeArguments TypeArguments
    | TypeArguments_Diamond
    deriving (Show)


typeArgumentsOrDiamond :: Parser TypeArgumentsOrDiamond
typeArgumentsOrDiamond =
    oneOf
        [ kmap TypeArguments_TypeArguments
            typeArguments
        , imap TypeArguments_Diamond
            (symbol "<>")
        ]


data FieldAccess
    = FieldAccess_Primary Primary Identifier
    | FieldAccess_Super Identifier
    | FieldAccess_TypeNameSuper TypeName Identifier
    deriving (Show)


fieldAccess :: Parser FieldAccess
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
          succeed FieldAccess_TypeNameSuper ) $
          typeName ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          (keyword "super") ) $
          spaces ) $
          (symbol ".") ) $
          spaces ) $
          identifier
        ]


data ArrayAccess
    = ArrayAccess_Expression ExpressionName Expression
    | ArrayAccess_Primary PrimaryNoNewArray Expression
    deriving (Show)


arrayAccess :: Parser ArrayAccess
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


data MethodInvocation
    = MethodInvocation_Name MethodName (Maybe ArgumentList)
    | MethodInvocation_Type TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Expression ExpressionName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Primary Primary (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_Super (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    | MethodInvocation_TypeSuper TypeName (Maybe TypeArguments) Identifier (Maybe ArgumentList)
    deriving (Show)


methodInvocation :: Parser MethodInvocation
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
            succeed MethodInvocation_Name ) $
            methodName ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
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
            succeed MethodInvocation_Type ) $
            typeName ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            optional typeArguments ) $
            spaces ) $
            identifier ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
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
            succeed MethodInvocation_Expression ) $
            expressionName ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            optional typeArguments ) $
            spaces ) $
            identifier ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
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
            succeed MethodInvocation_Primary ) $
            lazy (\_ -> primary) ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            optional typeArguments ) $
            spaces ) $
            identifier ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
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
            succeed MethodInvocation_Super ) $
            symbol "super" ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            optional typeArguments ) $
            spaces ) $
            identifier ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
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
            succeed MethodInvocation_TypeSuper ) $
            typeName ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            (keyword "super") ) $
            spaces ) $
            (symbol ".") ) $
            spaces ) $
            optional typeArguments ) $
            spaces ) $
            identifier ) $
            spaces ) $
            (symbol "(") ) $
            spaces ) $
            optional argumentList ) $
            spaces ) $
            (symbol ")")
        ]


data ArgumentList
    = ArgumentList Expression ([ Expression ])
    deriving (Show)


argumentList :: Parser ArgumentList
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


data MethodReference
    = MethodReference_Expression ExpressionName (Maybe TypeArguments) Identifier
    | MethodReference_Primary Primary (Maybe TypeArguments) Identifier
    | MethodReference_Reference ReferenceType (Maybe TypeArguments) Identifier
    | MethodReference_Super (Maybe TypeArguments) Identifier
    | MethodReference_TypeSuper TypeName (Maybe TypeArguments) Identifier
    | MethodReference_ClassNew ClassType (Maybe TypeArguments)
    | MethodReference_ArrayNew ArrayType
    deriving (Show)


methodReference :: Parser MethodReference
methodReference =
    oneOf
        [ keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_Expression ) $
          expressionName ) $
          spaces ) $
          symbol "::" ) $
          spaces ) $
          optional typeArguments ) $
          spaces ) $
          identifier
        , keeper (
          ignorer (
          keeper (
          ignorer (
          ignorer (
          ignorer (
          keeper (
          succeed MethodReference_Primary ) $
          lazy (\_ -> primary) ) $
          spaces ) $
          symbol "::" ) $
          spaces ) $
          optional typeArguments ) $
          spaces ) $
          identifier
        , 
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
          succeed MethodReference_Reference ) $
             referenceType ) $
             spaces ) $
             symbol "::" ) $
             spaces ) $
             optional typeArguments ) $
             spaces ) $
             identifier
        , 
            keeper (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            ignorer (
          succeed MethodReference_Super ) $
             (keyword "super") ) $
             spaces ) $
             symbol "::" ) $
             spaces ) $
             optional typeArguments ) $
             spaces ) $
             identifier
        , 
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
            ignorer (
          succeed MethodReference_TypeSuper ) $
             spaces ) $
             typeName ) $
             spaces ) $
             (symbol ".") ) $
             spaces ) $
             (keyword "super") ) $
             spaces ) $
             symbol "::" ) $
             spaces ) $
             optional typeArguments ) $
             spaces ) $
             identifier
        , 
            ignorer (
            ignorer (
            keeper (
            ignorer (
            ignorer (
            ignorer (
            keeper (
          succeed MethodReference_ClassNew ) $
             classType ) $
             spaces ) $
             symbol "::" ) $
             spaces ) $
             optional typeArguments ) $
             spaces ) $
             (keyword "new")
        , 
            ignorer (
            ignorer (
            ignorer (
            ignorer (
            keeper (
          succeed MethodReference_ArrayNew ) $
             arrayType ) $
             spaces ) $
             symbol "::" ) $
             spaces ) $
             (keyword "new")
        ]


data ArrayCreationExpression
    = ArrayCreationExpression_Primitive PrimitiveType DimExprs (Maybe Dims)
    | ArrayCreationExpression_Class ClassOrInterfaceType DimExprs (Maybe Dims)
    | ArrayCreationExpression_PrimitiveArrayInit PrimitiveType Dims ArrayInitializer
    | ArrayCreationExpression_ClassArrayInit ClassOrInterfaceType Dims ArrayInitializer
    deriving (Show)


arrayCreationExpression :: Parser ArrayCreationExpression
arrayCreationExpression =
    oneOf
        [
            succeed ArrayCreationExpression_Primitive
            |. (keyword "new")
            |. spaces
            |= primitiveType
            |. spaces
            |= dimExprs
            |. spaces
            |= optional dims
        ,
            succeed ArrayCreationExpression_Class
            |. (keyword "new")
            |. spaces
            |= classOrInterfaceType
            |. spaces
            |= dimExprs
            |. spaces
            |= optional dims
        ,
            succeed ArrayCreationExpression_PrimitiveArrayInit
            |. (keyword "new")
            |. spaces
            |= primitiveType
            |. spaces
            |= dims
            |. spaces
            |= arrayInitializer
        ,
            succeed ArrayCreationExpression_ClassArrayInit
            |. (keyword "new")
            |. spaces
            |= classOrInterfaceType
            |. spaces
            |= dims
            |. spaces
            |= arrayInitializer
        ]


data DimExprs
    = DimExprs DimExpr ([ DimExpr ])
    deriving (Show)


dimExprs :: Parser DimExprs
dimExprs =
    succeed DimExprs
        |= dimExpr
        |. spaces
        |= list dimExpr


data DimExpr
    = DimExpr ([ Annotation ]) Expression
    deriving (Show)


dimExpr :: Parser DimExpr
dimExpr =
    succeed DimExpr
        |= list annotation
        |. spaces
        |. (symbol "[")
        |. spaces
        |= expression
        |. spaces
        |. (symbol "]")


data Expression
    = Expression_Lambda LambdaExpression
    | Expression_Assignment AssignmentExpression
    deriving (Show)


expression :: Parser Expression
expression =
    oneOf
        [ succeed Expression_Lambda
            |= lambdaExpression
        , succeed Expression_Assignment
            |= assignmentExpression
        ]


data LambdaExpression
    = LambdaExpression LambdaParameters LambdaBody
    deriving (Show)


lambdaExpression :: Parser LambdaExpression
lambdaExpression =
    succeed LambdaExpression
        |= lambdaParameters
        |. spaces
        |. symbol "->"
        |. spaces
        |= lazy (\_ -> lambdaBody)


data LambdaParameters
    = LambdaParameters_List (Maybe LambdaParameterList)
    | LambdaParameters_Identifier Identifier
    deriving (Show)


lambdaParameters :: Parser LambdaParameters
lambdaParameters =
    oneOf
        [ succeed LambdaParameters_List
            |. (symbol "(")
            |. spaces
            |= optional lambdaParameterList
            |. spaces
            |. (symbol ")")
        , succeed LambdaParameters_Identifier
            |= identifier
        ]


data LambdaParameterList
    = LambdaParameterList_Parameters LambdaParameter ([ LambdaParameter ])
    | LambdaParameterList_Identifiers Identifier ([ Identifier ])
    deriving (Show)


lambdaParameterList :: Parser LambdaParameterList
lambdaParameterList =
    oneOf
        [ succeed LambdaParameterList_Parameters
            |= lambdaParameter
            |. spaces
            |= list
                (succeed identity
                    |. (symbol ",")
                    |. spaces
                    |= lambdaParameter
                )
        , succeed LambdaParameterList_Identifiers
            |= identifier
            |. spaces
            |= list
                (succeed identity
                    |. (symbol ",")
                    |. spaces
                    |= identifier
                )
        ]


data LambdaParameter
    = LambdaParameter_Normal ([ VariableModifier ]) LambdaParameterType VariableDeclaratorId
    | LambdaParameter_Arity VariableArityParameter
    deriving (Show)


lambdaParameter :: Parser LambdaParameter
lambdaParameter =
    oneOf
        [ succeed LambdaParameter_Normal
            |= list variableModifier
            |. spaces
            |= lambdaParameterType
            |. spaces
            |= variableDeclaratorId
        , succeed LambdaParameter_Arity
            |= variableArityParameter
        ]


data LambdaParameterType
    = LambdaParameterType_Unann UnannType
    | LambdaParameterType_Var
    deriving (Show)


lambdaParameterType :: Parser LambdaParameterType
lambdaParameterType =
    oneOf
        [ succeed LambdaParameterType_Unann
            |= unannType
        , succeed LambdaParameterType_Var
            |. (keyword "var")
        ]


data LambdaBody
    = LambdaBody_Expression Expression
    | LambdaBody_Block Block
    deriving (Show)


lambdaBody :: Parser LambdaBody
lambdaBody =
    oneOf
        [ succeed LambdaBody_Expression
            |= expression
        , succeed LambdaBody_Block
            |= block
        ]


data AssignmentExpression
    = AssignmentExpression_Conditional ConditionalExpression
    | AssignmentExpression_Assignment Assignment
    deriving (Show)


assignmentExpression :: Parser AssignmentExpression
assignmentExpression =
    oneOf
        [ succeed AssignmentExpression_Conditional
            |= conditionalExpression
        , succeed AssignmentExpression_Assignment
            |= assignment
        ]


data Assignment
    = Assignment LeftHandSide AssignmentOperator Expression
    deriving (Show)


assignment :: Parser Assignment
assignment =
    succeed Assignment
        |= leftHandSide
        |. spaces
        |= assignmentOperator
        |. spaces
        |= lazy (\_ -> expression)


data LeftHandSide
    = LeftHandSide_Expression ExpressionName
    | LeftHandSide_Field FieldAccess
    | LeftHandSide_Array ArrayAccess
    deriving (Show)


leftHandSide :: Parser LeftHandSide
leftHandSide =
    oneOf
        [ succeed LeftHandSide_Expression
            |= expressionName
        , succeed LeftHandSide_Field
            |= fieldAccess
        , succeed LeftHandSide_Array
            |= lazy (\_ -> arrayAccess)
        ]


data AssignmentOperator
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
    deriving (Show)


assignmentOperator :: Parser AssignmentOperator
assignmentOperator =
    oneOf
        [ succeed AssignmentOperator_Normal
            |. (symbol "=")
        , succeed AssignmentOperator_Multiply
            |. symbol "*="
        , succeed AssignmentOperator_Divide
            |. symbol "/="
        , succeed AssignmentOperator_Modulus
            |. symbol "%="
        , succeed AssignmentOperator_Add
            |. symbol "+="
        , succeed AssignmentOperator_Subtract
            |. symbol "-="
        , succeed AssignmentOperator_LeftShift
            |. symbol "<<="
        , succeed AssignmentOperator_RightShift3
            |. symbol ">>>="
        , succeed AssignmentOperator_RightShift
            |. symbol ">>="
        , succeed AssignmentOperator_And
            |. symbol "&="
        , succeed AssignmentOperator_Xor
            |. symbol "^="
        , succeed AssignmentOperator_Or
            |. symbol "|="
        ]


data ConditionalExpression
    = ConditionalExpression_Or ConditionalOrExpression
    | ConditionalExpression_TernaryConditional ConditionalOrExpression Expression ConditionalExpression
    | ConditionalExpression_TernaryLambda ConditionalOrExpression Expression LambdaExpression
    deriving (Show)


conditionalExpression :: Parser ConditionalExpression
conditionalExpression =
    oneOf
        [ succeed ConditionalExpression_Or
            |= conditionalOrExpression
        , succeed ConditionalExpression_TernaryConditional
            |= conditionalOrExpression
            |. spaces
            |. (symbol "?")
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. (symbol ":")
            |. spaces
            |= lazy (\_ -> conditionalExpression)
        , succeed ConditionalExpression_TernaryLambda
            |= conditionalOrExpression
            |. spaces
            |. (symbol "?")
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. (symbol ":")
            |. spaces
            |= lambdaExpression
        ]


--------------------------- FROM HERE

zeta :: String -> (a -> c) -> (b -> a -> c)
              -> Parser a -> (() -> Parser b) -> Parser c
zeta op tca tcb aparser bparser =
  succeed (\a mb -> case mb of
                        Just b -> tcb b a
                        Nothing -> tca a)
  |= aparser
  |. spaces
  |= optional
      ( succeed identity
        |. symbol op
        |. spaces
        |= lazy bparser )


data ConditionalOrExpression
  = ConditionalOrExpression_And ConditionalAndExpression
  | ConditionalOrExpression_Or ConditionalOrExpression ConditionalAndExpression
    deriving (Show)

conditionalOrExpression :: Parser ConditionalOrExpression
conditionalOrExpression =
  zeta
  "||"
  ConditionalOrExpression_And
  ConditionalOrExpression_Or
  conditionalAndExpression
  (\_ -> conditionalOrExpression)


data ConditionalAndExpression
  = ConditionalAndExpression_Or InclusiveOrExpression
  | ConditionalAndExpression_And ConditionalAndExpression InclusiveOrExpression
    deriving (Show)

conditionalAndExpression :: Parser ConditionalAndExpression
conditionalAndExpression =
  zeta
  "&&"
  ConditionalAndExpression_Or
  ConditionalAndExpression_And
  inclusiveOrExpression
  (\_ -> conditionalAndExpression)

data InclusiveOrExpression
  = InclusiveOrExpression_Xor ExclusiveOrExpression
  | InclusiveOrExpression_Or InclusiveOrExpression ExclusiveOrExpression
    deriving (Show)

inclusiveOrExpression :: Parser InclusiveOrExpression
inclusiveOrExpression =
  zeta
  "|"
  InclusiveOrExpression_Xor
  InclusiveOrExpression_Or
  exclusiveOrExpression
  (\_ -> inclusiveOrExpression)


data ExclusiveOrExpression
  = ExclusiveOrExpression_And AndExpression
  | ExclusiveOrExpression_Xor ExclusiveOrExpression AndExpression
    deriving (Show)

exclusiveOrExpression :: Parser ExclusiveOrExpression
exclusiveOrExpression =
  zeta
  "^"
  ExclusiveOrExpression_And
  ExclusiveOrExpression_Xor
  andExpression
  (\_ -> exclusiveOrExpression)


data AndExpression
  = AndExpression_Equality EqualityExpression
  | AndExpression_And AndExpression EqualityExpression
    deriving (Show)

andExpression :: Parser AndExpression
andExpression =
  zeta
  "&"
  AndExpression_Equality
  AndExpression_And
  equalityExpression
  (\_ -> andExpression)


data EqualityExpression
  = EqualityExpression_Relational RelationalExpression
  | EqualityExpression_Equals EqualityExpression RelationalExpression
  | EqualityExpression_NotEquals EqualityExpression RelationalExpression
    deriving (Show)

equalityExpression :: Parser EqualityExpression
equalityExpression =
  succeed (\re mf ->
                 case mf of
                   Just f -> f re
                   Nothing -> EqualityExpression_Relational re
            )
  |= lazy (\_ -> relationalExpression)
  |. spaces
  |= optional 
     ( succeed (\f ee -> f ee)
       |= oneOf
          [ succeed EqualityExpression_Equals
            |. symbol "=="
          , succeed EqualityExpression_NotEquals
            |. symbol "!="
          ]
       |. spaces
       |= lazy (\_ -> equalityExpression)
     )


data RelationalExpression
  = RelationalExpression_Shift ShiftExpression
  | RelationalExpression_Less RelationalExpression ShiftExpression
  | RelationalExpression_Greater RelationalExpression ShiftExpression
  | RelationalExpression_LessEqual RelationalExpression ShiftExpression
  | RelationalExpression_GreaterEqual RelationalExpression ShiftExpression
  | RelationalExpression_Instanceof RelationalExpression ReferenceType
    deriving (Show)

relationalExpression :: Parser RelationalExpression
relationalExpression =
  succeed (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> RelationalExpression_Shift a)
  |= lazy (\_ -> shiftExpression)
  |. spaces
  |= optional
     ( succeed (\f a -> f a)
       |= oneOf 
             [ succeed RelationalExpression_Less
               |. (symbol "<")
             , succeed RelationalExpression_Greater
               |. (symbol ">")
             , succeed RelationalExpression_LessEqual
               |. symbol "<="
             , succeed RelationalExpression_GreaterEqual
               |. symbol ">="
             ]
       |. spaces
       |= lazy (\_ -> relationalExpression) )

       {- TODO
    , succeed RelationalExpression_Instanceof
      |= this
      |. spaces
      |. (keyword "instanceof")
      |. spaces
      |= lazy (\_ -> referenceType)
    ]
       -}


data ShiftExpression
  = ShiftExpression_Additive AdditiveExpression
  | ShiftExpression_Left ShiftExpression AdditiveExpression
  | ShiftExpression_Right ShiftExpression AdditiveExpression
  | ShiftExpression_Right2 ShiftExpression AdditiveExpression
    deriving (Show)

shiftExpression :: Parser ShiftExpression
shiftExpression =
  succeed (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> ShiftExpression_Additive a)
  |= lazy (\_ -> additiveExpression)
  |. spaces
  |= optional
     ( succeed (\f a -> f a)
       |= oneOf 
            [ succeed ShiftExpression_Left
              |. symbol "<<"
            , succeed ShiftExpression_Right2
              |. symbol ">>>"
            , succeed ShiftExpression_Right
              |. symbol ">>"
            ]
       |. spaces
       |= lazy (\_ -> shiftExpression) )


data AdditiveExpression
  = AdditiveExpression_Multiplicative MultiplicativeExpression
  | AdditiveExpression_Plus AdditiveExpression MultiplicativeExpression
  | AdditiveExpression_Minus AdditiveExpression MultiplicativeExpression
    deriving (Show)

additiveExpression :: Parser AdditiveExpression
additiveExpression =
  succeed (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing ->  AdditiveExpression_Multiplicative a)
  |= lazy (\_ -> multiplicativeExpression)
  |. spaces
  |= optional
     ( succeed (\f a -> f a)
       |= oneOf 
            [ succeed AdditiveExpression_Plus
              |. (symbol "+")
            , succeed AdditiveExpression_Minus
              |. (symbol "-")
            ]
       |. spaces
       |= lazy (\_ -> additiveExpression) )

data MultiplicativeExpression
  = MultiplicativeExpression_Unary UnaryExpression
  | MultiplicativeExpression_Multiply MultiplicativeExpression UnaryExpression
  | MultiplicativeExpression_Divide MultiplicativeExpression UnaryExpression
  | MultiplicativeExpression_Mod MultiplicativeExpression UnaryExpression
    deriving (Show)

multiplicativeExpression :: Parser MultiplicativeExpression
multiplicativeExpression =
  succeed (\a mf ->
               case mf of 
                   Just f -> f a
                   Nothing -> MultiplicativeExpression_Unary a)
  |= lazy (\_ -> unaryExpression)
  |. spaces
  |= optional
     ( succeed (\f a -> f a)
       |= oneOf
           [ succeed MultiplicativeExpression_Multiply
             |. (symbol "*")
           , succeed MultiplicativeExpression_Divide
             |. (symbol "/")
           , succeed MultiplicativeExpression_Mod
             |. (symbol "%")
           ]
       |. spaces
       |= lazy (\_ -> multiplicativeExpression) )
 
--------------------------- TO HERE

data UnaryExpression
    = UnaryExpression_PreIncrement PreIncrementExpression
    | UnaryExpression_PreDecrement PreDecrementExpression
    | UnaryExpression_Plus UnaryExpression
    | UnaryExpression_Minus UnaryExpression
    | UnaryExpression_NotPlusMinus UnaryExpressionNotPlusMinus
    deriving (Show)


unaryExpression :: Parser UnaryExpression
unaryExpression =
    oneOf
        [ succeed UnaryExpression_PreIncrement
            |= preIncrementExpression
        , succeed UnaryExpression_PreDecrement
            |= preDecrementExpression
        , succeed UnaryExpression_Plus
            |. (symbol "+")
            |. spaces
            |= lazy (\_ -> unaryExpression)
        , succeed UnaryExpression_Minus
            |. (symbol "-")
            |. spaces
            |= lazy (\_ -> unaryExpression)
        , succeed UnaryExpression_NotPlusMinus
            |= unaryExpressionNotPlusMinus
        ]


data PreIncrementExpression
    = PreIncrementExpression UnaryExpression
    deriving (Show)


preIncrementExpression :: Parser PreIncrementExpression
preIncrementExpression =
    succeed PreIncrementExpression
        |. symbol "++"
        |. spaces
        |= lazy (\_ -> unaryExpression)


data PreDecrementExpression
    = PreDecrementExpression UnaryExpression
    deriving (Show)


preDecrementExpression :: Parser PreDecrementExpression
preDecrementExpression =
    succeed PreDecrementExpression
        |. symbol "--"
        |. spaces
        |= lazy (\_ -> unaryExpression)


data UnaryExpressionNotPlusMinus
    = UnaryExpressionNotPlusMinus_Postfix PostfixExpression
    | UnaryExpressionNotPlusMinus_BitwiseNot UnaryExpression
    | UnaryExpressionNotPlusMinus_LogicalNot UnaryExpression
    | UnaryExpressionNotPlusMinus_Cast CastExpression
    | UnaryExpressionNotPlusMinus_Switch SwitchExpression
    deriving (Show)


unaryExpressionNotPlusMinus :: Parser UnaryExpressionNotPlusMinus
unaryExpressionNotPlusMinus =
    oneOf
        [ succeed UnaryExpressionNotPlusMinus_Postfix
            |= postfixExpression
        , succeed UnaryExpressionNotPlusMinus_BitwiseNot
            |. (symbol "~")
            |. spaces
            |= lazy (\_ -> unaryExpression)
        , succeed UnaryExpressionNotPlusMinus_LogicalNot
            |. (symbol "!")
            |. spaces
            |= lazy (\_ -> unaryExpression)
        , succeed UnaryExpressionNotPlusMinus_Cast
            |= lazy (\_ -> castExpression)
        , succeed UnaryExpressionNotPlusMinus_Switch
            |= switchExpression
        ]


data PostfixExpression
    = PostfixExpression_Primary Primary
    | PostfixExpression_Name ExpressionName
    | PostfixExpression_Increment PostIncrementExpression
    | PostfixExpression_Decrement PostDecrementExpression
    deriving (Show)


postfixExpression :: Parser PostfixExpression
postfixExpression =
    oneOf
        [ succeed PostfixExpression_Primary
            |= primary
        , succeed PostfixExpression_Name
            |= expressionName
        , succeed PostfixExpression_Increment
            |= postIncrementExpression
        , succeed PostfixExpression_Decrement
            |= postDecrementExpression
        ]


data PostIncrementExpression
    = PostIncrementExpression PostfixExpression
    deriving (Show)


postIncrementExpression :: Parser PostIncrementExpression
postIncrementExpression =
    succeed PostIncrementExpression
        |= lazy (\_ -> postfixExpression)
        |. spaces
        |. symbol "++"


data PostDecrementExpression
    = PostDecrementExpression PostfixExpression
    deriving (Show)


postDecrementExpression :: Parser PostDecrementExpression
postDecrementExpression =
    succeed PostDecrementExpression
        |= lazy (\_ -> postfixExpression)
        |. spaces
        |. symbol "--"


data CastExpression
    = CastExpression_Unary PrimitiveType UnaryExpression
    | CastExpression_UnaryAdditional ReferenceType ([ AdditionalBound ]) UnaryExpressionNotPlusMinus
    | CastExpression_Lambda ReferenceType ([ AdditionalBound ]) LambdaExpression
    deriving (Show)


castExpression :: Parser CastExpression
castExpression =
    oneOf
        [ succeed CastExpression_Unary
            |. (symbol "(")
            |. spaces
            |= primitiveType
            |. spaces
            |. (symbol ")")
            |. spaces
            |= unaryExpression
        , succeed CastExpression_UnaryAdditional
            |. (symbol "(")
            |. spaces
            |= referenceType
            |. spaces
            |= list additionalBound
            |. spaces
            |. (symbol ")")
            |. spaces
            |= unaryExpressionNotPlusMinus
        , succeed CastExpression_Lambda
            |. (symbol "(")
            |. spaces
            |= referenceType
            |. spaces
            |= list additionalBound
            |. spaces
            |. (symbol ")")
            |. spaces
            |= lambdaExpression
        ]


data SwitchExpression
    = SwitchExpression Expression SwitchBlock
    deriving (Show)


switchExpression :: Parser SwitchExpression
switchExpression =
    succeed SwitchExpression
        |. (keyword "switch")
        |. spaces
        |. (symbol "(")
        |. spaces
        |= expression
        |. spaces
        |. (symbol ")")
        |. spaces
        |= switchBlock


data ConstantExpression
    = ConstantExpression Expression
    deriving (Show)


constantExpression :: Parser ConstantExpression
constantExpression =
    succeed ConstantExpression
        |= expression



-- }}}
