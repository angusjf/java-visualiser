module JavaParser exposing (..)

import Set exposing (Set)
import Parser as P exposing (Parser, (|=), (|.), Step)

-- types i {{{

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

-- }}}

-- types ii {{{

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
  , more : List (String, List TypeArgument)
  }

type TypeArgument
  = ReferenceTypeArgument ReferenceType
  | WildCardSuper ReferenceType
  | WildCardExtends ReferenceType

-- }}}

-- types iii {{{

type alias NonWildcardTypeArguments = List ReferenceType

type OrDiamond a = Diamond | Types a

type alias TypeArgumentsOrDiamond = OrDiamond (List TypeArgument)

type alias NonWildcardTypeArgumentsOrDiamond = OrDiamond NonWildcardTypeArguments

type alias TypeParameter =
  { identifier : String
  , extends : Maybe Bound
  }

type alias Bound =
  { type_ : ReferenceType
  , and : ReferenceType
  }

-- }}}

-- types iv {{{

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

-- }}}

-- types v {{{

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

-- }}}

-- types vi {{{

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

-- }}}

-- types vii {{{

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

-- }}}

-- types viii {{{

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
    , groups : List SwitchBlockStatementGroup
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
    ,   List Catch | 
        { (Maybe (List Catch))
        , finally : Block
        }
    }
  -}
  | StatementTryResource
    { spec : ResourceSpecification
    , block : Block
    , catches : List CatchClause
    , finally : Maybe Block
    }

-- }}}

-- types ix {{{

type alias CatchClause =
  { modifiers : List VariableModifier
  , type_ : CatchType
  , identifier : String
  , block : Block
  }

type alias CatchType =
  { identifier : String
  , more : List String
  }

type alias ResourceSpecification = List Resource

type alias Resource =
  { modifiers : List VariableModifier
  , type_ : ReferenceType
  , identifier: VariableDeclaratorId
  , expression : Expression
  }

-- }}}

-- types x {{{

type alias SwitchBlockStatementGroup =
  { labels : List SwitchLabel
  , blockStatements : List BlockStatement
  }

type SwitchLabel
  = SwitchLabelExp Expression
  | SwitchLabelDefault

type ForControl
  = FCForVarControl ForVarControl
  | FCForInit
    { init : ForInit 
    , condition : Maybe Expression
    , update : Maybe ForUpdate
    }

type alias ForVarControl =
  { modifiers : List VariableModifier
  , type_ : Type
  , decl : VariableDeclaratorId
  , rest : ForVarControlRest
  }

type ForVarControlRest
  = ForClassic
    { rest : ForVariableDeclaratorsRest
    , cond : Maybe Expression
    , loop : ForUpdate
    }
  | ForEach Expression

type alias ForVariableDeclaratorsRest =
  { init : Maybe VariableInitializer
  , vars : List VariableDeclarator
  }

type alias ForInit = ForUpdate

type alias ForUpdate = List Expression

-- }}}

-- types xi {{{

type Expression =
  Expression
    { exp1 : Expression1
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

-- }}}

-- types xii {{{

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
  | E3BracketedExpression
    { expression : Expression
    , exp3 : Expression3
    }
  | E3BracketedType
    { type_ : Type
    , exp3 : Expression3
    }
  | E3Primary
    { primary : Primary
    , selectors : List Selector
    , ops : List PostfixOp
    }

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

-- }}}

-- types xiii {{{

type Primary
  = PrimaryLiteral Literal
  | PrimaryParExpression ParExpression
  | PrimaryThis (List Expression)
  | PrimarySuper SuperSuffix
--| new Creator
--| NonWildcardTypeArguments (ExplicitGenericInvocationSuffix | this (List Expression))
  | PrimaryIdentifier
    { identifier : String
    , suffix : Maybe IdentifierSuffix
    }
  | BasicTypeDotClass
    { type_ : BasicType
    , arrays : Int
    }
  | VoidDotClass

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

-- }}}

-- types xiv {{{

{-
type Creator:  
    NonWildcardTypeArguments CreatedName ClassCreatorRest
    CreatedName (ClassCreatorRest | ArrayCreatorRest)

CreatedName:   
    Identifier [TypeArgumentsOrDiamond] { . Identifier [TypeArgumentsOrDiamond] }

-}

type alias ClassCreatorRest =
  { arguments : List Expression
  , body : Maybe ClassBody
  }

{-
ArrayCreatorRest:
    [ (] {[]} ArrayInitializer  |  Expression ] {[ Expression ]} {[]})

-}

type IdentifierSuffix
  = NoSuffix
  | IdSuffixArrayDotClass { arrays : Int }
  | IdSuffixExpression Expression
  | IdSuffixArguments (List Expression)
  | IdSuffixDotClass
  | IdSuffixDotEGI ExplicitGenericInvocation
  | IdSuffixDotThis
  | IdSuffixDotSuper (List Expression)
  | IdSuffixDotNew
    { typeArgs : Maybe NonWildcardTypeArguments
    , innerCreator : InnerCreator
    }

type alias ExplicitGenericInvocation =
  { typeArgs : NonWildcardTypeArguments
  , suffix : ExplicitGenericInvocationSuffix
  }

type alias InnerCreator =
  { identifier : String
  , typeArgs : Maybe NonWildcardTypeArgumentsOrDiamond
  , rest : ClassCreatorRest
  }

type Selector
  = SelectorId
    { identifier : String
    , arguments : List Expression
    }
--| SelectorEGI ExplicitGenericInvocation
  | SelectorThis
  | SelectorSuper SuperSuffix
--| SelectorId new [NonWildcardTypeArguments] InnerCreator
  | SelectorArray Expression

-- }}}

-- types xv {{{

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

type alias AnnotationTypeBody = List AnnotationTypeElementDeclarations

type alias AnnotationTypeElementDeclarations =
  List AnnotationTypeElementDeclaration

type alias AnnotationTypeElementDeclaration =
  { modifiers : List Modifier
  , rest : AnnotationTypeElementRest
  }

type AnnotationTypeElementRest = TODO0
{-
  = Type Identifier AnnotationMethodOrConstantRest ;
  | ClassDeclaration
  | InterfaceDeclaration
  | EnumDeclaration  
  | AnnotationTypeDeclaration
-}

type AnnotationMethodOrConstantRest
  = AnnotationRest AnnotationMethodRest
  | ConstantRest (List ConstantDeclaratorRest)

type alias AnnotationMethodRest =
  { brackets : Bool
  , default : Maybe ElementValue
  }

-- }}}

reserved : Set String
reserved =
  Set.fromList
    [ "abstract", "continue", "for", "new", "switch", "assert", "default"
    , "goto", "package", "synchronized", "boolean", "do", "if", "private"
    , "this", "break", "double", "implements", "protected", "throw", "byte"
    , "else", "import", "public", "throws", "case", "enum", "instanceof"
    , "return", "transient", "catch", "extends", "int", "short", "try", "char"
    , "final", "interface", "static", "void", "class", "finally", "long"
    , "strictfp", "volatile", "const", "float", "native", "super", "while"
    ]

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
qualifiedIdentifier =
  P.succeed (\first rest -> first ++ "." ++ String.join "." rest)
  |= identifier
  |= optionalList
     ( P.succeed identity
       |. P.symbol "."
       |= dotted identifier
     )

qualifiedIdentifierList : Parser (List String)
qualifiedIdentifierList =
  P.succeed (::)
  |= qualifiedIdentifier
  |. P.spaces
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
    |= qualifiedIdentifier
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
  |. P.spaces
  |= optionalList typeArguments
  |. P.spaces
  |= P.succeed [] -- TODO 

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
  |. P.spaces
  |= brackets

brackets : Parser Int
brackets =
  P.map List.length <| repeated (P.symbol "[" |. P.spaces |. P.symbol "]")

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

interfaceMemberDecl : Parser InterfaceMemberDecl
interfaceMemberDecl =
  P.oneOf
    [ P.map IMDMethodOrField interfaceMethodOrFieldDecl
    , P.map IMDGeneric interfaceGenericMethodDecl
    , P.map IMDClass (P.lazy (\_ -> classDeclaration))
    , P.map IMDInterface (P.lazy (\_ -> interfaceDeclaration))
    , P.succeed (\id rest -> IMDVoid { identifier = id , rest = rest })
      |. P.keyword "void"
      |. P.spaces
      |= identifier
      |. P.spaces
      |= voidInterfaceMethodDeclaratorRest
    ]

interfaceMethodOrFieldDecl : Parser InterfaceMethodOrFieldDecl
interfaceMethodOrFieldDecl =
  P.succeed InterfaceMethodOrFieldDecl
  |= type_
  |. P.spaces
  |= identifier
  |. P.spaces
  |= interfaceMethodOrFieldRest

interfaceGenericMethodDecl : Parser InterfaceGenericMethodDecl
interfaceGenericMethodDecl = P.oneOf []

voidInterfaceMethodDeclaratorRest : Parser VoidInterfaceMethodDeclaratorRest
voidInterfaceMethodDeclaratorRest = P.oneOf []

interfaceMethodOrFieldRest : Parser InterfaceMethodOrFieldRest
interfaceMethodOrFieldRest = P.oneOf []

memberDecl : Parser MemberDecl
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
  P.oneOf
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
  |. P.spaces
  |= optionalList
     ( P.succeed identity
       |. P.symbol ","
       |. P.spaces
       |= list variableDeclarator
       |. P.spaces
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
formalParameters = bracketed (optional formalParameterDecls)

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
  |. P.spaces
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

{-
ArrayInitializer:
    { [ VariableInitializer { , VariableInitializer } [,] ] }
-}
arrayInitializer : Parser ArrayInitializer
arrayInitializer =
  P.succeed identity
  |. P.symbol "{"
  |. P.spaces
  |= optionalList
      ( P.lazy
        (\_ -> commas variableInitializer
               |. optional (P.symbol ",")
        )
      )
  |. P.spaces
  |. P.symbol "}"

{-
Expression: 
    Expression1 [AssignmentOperator Expression1]
-}
expression : Parser Expression
expression =
  P.succeed (\exp rest -> Expression { exp1 = exp, rest = rest })
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

infixAndExp3 : Parser (InfixOp, Expression3)
infixAndExp3 =
  P.succeed Tuple.pair
  |= P.backtrackable infixOp
  |. P.spaces
  |= expression3

instanceof : Parser Type
instanceof =
  P.succeed identity
  |. P.keyword "instanceof"
  |. P.spaces
  |= type_

{-
Expression2Rest:
    { InfixOp Expression3 }
    instanceof Type
-}
expression2Rest : Parser Expression2Rest
expression2Rest = 
  P.oneOf
    [ P.map E2RInstanceof instanceof
    , P.map E2RInfixOp (list infixAndExp3)
    ]

infixOp : Parser InfixOp
infixOp =
  P.oneOf
    [ P.succeed LogicalOr        |. P.keyword "||"
    , P.succeed LogicalAnd       |. P.keyword "&&"
    , P.succeed BitwizeOr        |. P.keyword "|"
    , P.succeed BitwizeXor       |. P.keyword "^"
    , P.succeed BitwizeAnd       |. P.keyword "&"
    , P.succeed Equal            |. P.keyword "=="
    , P.succeed NotEqual         |. P.keyword "!="
    , P.succeed LessThanEqual    |. P.keyword "<="
    , P.succeed GreaterThanEqual |. P.keyword ">="
    , P.succeed LessThan         |. P.keyword "<"
    , P.succeed GreaterThan      |. P.keyword ">"
    , P.succeed TripleRightShift |. P.keyword ">>>"
    , P.succeed RightShift       |. P.keyword ">>"
    , P.succeed LeftShift        |. P.keyword "<<"
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
expression3 =
  P.oneOf
    [ P.succeed (\op exp3 ->
                 E3Prefix { op = op, exp3 = exp3 })
      |= prefixOp
      |. P.spaces
      |= P.lazy (\_ -> expression3)
    , P.succeed (\exp exp3 ->
                 E3BracketedExpression { expression = exp, exp3 = exp3 })
      |= bracketed (P.lazy (\_ -> expression))
      |. P.spaces
      |= P.lazy (\_ -> expression3)
    , P.succeed (\t exp3 ->
                 E3BracketedType { type_ = t , exp3 = exp3 })
      |= bracketed type_
      |. P.spaces
      |= P.lazy (\_ -> expression3)
    , P.succeed (\pri sels ops ->
                 E3Primary { primary = pri, selectors = sels, ops = ops })
      |= primary
      |. P.spaces
      |= P.lazy (\_ -> list selector)
      |. P.spaces
      |= list postfixOp
    ]

{-
Selector:
    . Identifier [Arguments]
    . ExplicitGenericInvocation
    . this
    . super SuperSuffix
    . new [NonWildcardTypeArguments] InnerCreator
    [ Expression ]
-}
selector : Parser Selector
selector =
  P.oneOf
    [ P.succeed SelectorArray 
      |. P.symbol "["
      |. P.spaces
      |= expression
      |. P.spaces
      |. P.symbol "]"
    , P.succeed identity
      |. P.symbol "."
      |= P.oneOf
         [ P.succeed (\id args -> SelectorId { identifier = id
                                             , arguments = args })
           |= identifier
           |. P.spaces
           |= optionalList arguments
         ]
    ]

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

identifierSuffix : Parser IdentifierSuffix
identifierSuffix =
  P.oneOf
    [ P.map IdSuffixArguments (P.lazy (\_ -> arguments))
    ]
    {-
  = NoSuffix
  | IdSuffixArrayDotClass { arrays : Int }
  | IdSuffixExpression Expression
  | IdSuffixDotClass
  | IdSuffixDotEGI ExplicitGenericInvocation
  | IdSuffixDotThis
  | IdSuffixDotSuper (List String)
  | IdSuffixDotNew
    { typeArgs : Maybe NonWildcardTypeArguments
    , innerCreator : InnerCreator
    }
    -}

primary : Parser Primary
primary =
  P.oneOf
    [ P.map PrimaryLiteral literal
    , P.map PrimaryParExpression (P.lazy (\_ -> parExpression))
    , P.succeed (\t_ arrays -> BasicTypeDotClass
                                  { type_ = t_,  arrays = arrays })
      |= basicType
      |. P.spaces
      |= brackets
      |. P.spaces
      |. P.symbol "."
      |. P.spaces
      |. P.keyword "class"
    , P.succeed PrimaryThis
      |. P.keyword "this"
      |. P.spaces
      |= optionalList (P.lazy (\_ -> arguments))
    , P.succeed VoidDotClass
      |. P.keyword "void"
      |. P.spaces
      |. P.symbol "."
      |. P.spaces
      |. P.keyword "class"
    , P.succeed (\id suffix -> PrimaryIdentifier
                 { identifier = id, suffix = suffix })
      |= qualifiedIdentifier
      |. P.spaces
      |= optional identifierSuffix
    ]

superSuffix : Parser SuperSuffix
superSuffix = P.oneOf [] -- use `arguments` function

explicitGenericInvocationSuffix : Parser ExplicitGenericInvocationSuffix
explicitGenericInvocationSuffix = P.oneOf [] -- use `arguments`

arguments : Parser (List Expression)
arguments = bracketed (commas expression)

bracketed : Parser a -> Parser a
bracketed parser =
  P.succeed identity
  |. P.symbol "("
  |. P.spaces
  |= parser
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
    , P.backtrackable <| P.map BlockVaraible localVariableDeclarationStatement
    , P.succeed (\label stmt -> BlockStatement { label = label, statement = stmt })
      |= optional
          (P.backtrackable (identifier |. P.spaces |. P.symbol ":" |. P.spaces))
      |= statement
    ]

statement : Parser Statement
statement =
  P.oneOf
   [ P.map StatementBlock (P.lazy (\_ -> block))
   , P.succeed StatementSemicolon |. P.symbol ";"
   , P.backtrackable <|
      P.succeed (\label stmt ->
                  StatementLabel { label = label, statement = stmt })
      |= identifier
      |. P.spaces
      |. P.symbol ":"
      |. P.spaces
      |= P.lazy (\_ -> statement)
   , P.map StatementExpression (expression |. P.spaces |. P.symbol ";")
   , P.succeed (\cond if_ else_ ->
                StatementIf { cond = cond, if_ = if_, else_ = else_ })
     |. P.keyword "if"
     |. P.spaces
     |= parExpression
     |. P.spaces
     |= P.lazy (\_ -> statement)
     |. P.spaces
     |= optional
        ( P.succeed identity
          |. P.keyword "else"
          |. P.spaces
          |= P.lazy (\_ -> statement)
        )
   , P.succeed (\exp throws -> StatementAssert
                                { expression = exp, throws = throws })
     |. P.keyword "assert"
     |. P.spaces
     |= expression
     |. P.spaces
     |= optional
        ( P.succeed identity 
          |. P.symbol ":"
          |. P.spaces
          |= expression
        )
     |. P.symbol ";"
   , P.succeed (\exp groups -> StatementSwitch
                                { expression = exp, groups = groups })
     |. P.keyword "switch"
     |. P.spaces
     |= parExpression
     |. P.spaces
     |. P.symbol "{"
     |. P.spaces
     |= list switchBlockStatementGroup
     |. P.spaces
     |. P.symbol "}"
     {-
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
   -}
   , P.succeed StatementBreak
     |. P.keyword "break"
     |. P.spaces
     |= optional identifier
     |. P.spaces
     |. P.symbol ";"
   , P.succeed StatementContinue
     |. P.keyword "continue"
     |. P.spaces
     |= optional identifier
     |. P.spaces
     |. P.symbol ";"
   , P.succeed StatementReturn
     |. P.keyword "return"
     |. P.spaces
     |= optional expression
     |. P.spaces
     |. P.symbol ";"
   , P.succeed StatementThrow
     |. P.keyword "throw"
     |. P.spaces
     |= expression
     |. P.spaces
     |. P.symbol ";"
   {-
   , P.succeed (\exp blok -> StatementSynchronized
                               { expression = exp, block = blok })
     |.
   -}
   ]

parExpression : Parser ParExpression
parExpression =
  P.succeed Par
  |. P.symbol "("
  |. P.spaces
  |= expression
  |. P.spaces
  |. P.symbol ")"

switchBlockStatementGroup : Parser SwitchBlockStatementGroup
switchBlockStatementGroup =
  P.succeed SwitchBlockStatementGroup
  |= switchLabels
  |. P.spaces
  |= list (P.lazy (\_ -> blockStatement))

switchLabels : Parser (List SwitchLabel)
switchLabels =
  P.succeed (::)
  |= switchLabel
  |. P.spaces
  |= list switchLabel

switchLabel =
  P.oneOf
    [ P.succeed SwitchLabelExp
      |. P.keyword "case"
      |. P.spaces
      |= expression
      |. P.spaces
      |. P.symbol ":"
    , P.succeed SwitchLabelDefault
      |. P.keyword "default"
      |. P.spaces
      |. P.symbol ":"
    ]

localVariableDeclarationStatement : Parser LocalVariableDeclarationStatement
localVariableDeclarationStatement =
  P.succeed LocalVariableDeclarationStatement
  |= list variableModifier
  |. P.spaces
  |= type_
  |. P.spaces
  |= list variableDeclarator
  |. P.spaces
  |. P.symbol ";"

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
  |. P.spaces
  |= commas enumConstant
  |. P.spaces
  |. optional (P.symbol ",")
  |. P.spaces
  |= commas classBodyDeclaration
  |. P.spaces
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
