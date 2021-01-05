module Package.JavaToGraph exposing (..) -- TODO (fromSources)

import Graph exposing (NodeId)
import Package.Graph exposing (..)
import Java15Parser as JP
import Java15AstHelpers as AstHelpers
import Parser
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Set

type alias Subgraph =
  { entity : Entity
  , parent : Maybe NodeId
  , interfaces : List NodeId
  , references : List NodeId
  }

fromSources : List String -> List (String, PackageGraph)
fromSources srcs =
  let
    packageSubgraphs : List (List Subgraph)
    packageSubgraphs =
      srcs
      |> List.filterMap toAst
      |> List.concatMap compilationUnitTS
      |> groupByPackage
  in
    packageSubgraphs
    |> List.filterMap (\subgraphs ->
        let
          nodes = List.map subgraphToNode subgraphs
          extensions = subgraphs |> List.filterMap subgraphToExtension
          implements = subgraphs |> List.concatMap subgraphToImplements
          references = subgraphs |> List.concatMap subgraphToReferences
          packageName =
            case List.head subgraphs of
              Just n -> Just <| getPackage n
              Nothing -> Nothing
        in
          Maybe.map
            (\name ->
                ( name
                , { nodes = nodes
                  , edges = extensions ++ implements ++ references
                  }
                )
            )
            packageName
      )

groupByPackage : List Subgraph -> List (List Subgraph)
groupByPackage =
  groupBy (\a b -> getPackage a == getPackage b)

getPackage : Subgraph -> String
getPackage { entity } = entity.pkg

groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xs =
  let
    uniq : List a
    uniq = nub eq xs
  in
    List.map (getSame eq xs) uniq

getSame : (a -> a -> Bool) -> List a -> a -> List a
getSame eq list item =
    List.filter (\x -> eq x item) list

nub : (a -> a -> Bool) -> List a -> List a
nub eq list =
  case list of
    x::xs -> x :: nub eq (List.filter (not << eq x) xs)
    [] -> []

toAst : String -> Maybe JP.CompilationUnit
toAst src = toMaybeLog (JP.parse JP.compilationUnit src)

toMaybeLog : Result a b -> Maybe b
toMaybeLog res =
  case res of
    Ok x ->
      always (Just x) (Debug.log "parse Ok" x)
    Err e ->
      always Nothing (Debug.log "parse Err" e)

getNodeId : Entity -> NodeId
getNodeId { pkg , name } = mkNodeId pkg name

mkNodeId : String -> String -> String
mkNodeId pkg name = pkg ++ "." ++ name

subgraphToExtension : Subgraph -> Maybe Edge
subgraphToExtension { entity, parent } =
  parent
  |> Maybe.map (\p -> { from = getNodeId entity, to = p, data = Extends })

subgraphToImplements : Subgraph -> List Edge
subgraphToImplements { entity, interfaces } =
  interfaces
  |> List.map (\i -> { from = getNodeId entity, to = i, data = Implements })

subgraphToReferences : Subgraph -> List Edge
subgraphToReferences { entity, references } =
  references
  |> List.map (\ref -> { from = getNodeId entity, to = ref, data = References })

subgraphToNode : Subgraph -> Node
subgraphToNode { entity } =
    { id = getNodeId entity, data = entity }

-- {{{ TO SUBGRAPHS

compilationUnitTS : JP.CompilationUnit -> List Subgraph
compilationUnitTS unit =
    case unit of
        JP.CompilationUnit_Ordinary u ->
            ordinaryCompilationUnitTS u
        JP.CompilationUnit_Modular u ->
            Debug.todo "TODO"

ordinaryCompilationUnitTS : JP.OrdinaryCompilationUnit -> List Subgraph
ordinaryCompilationUnitTS (JP.OrdinaryCompilationUnit pkg imports types) =
    let
        package =
            pkg
            |> Maybe.map (\(JP.PackageDeclaration _ ids) -> ids)
            |> Maybe.withDefault []
            |> List.map (\(JP.Identifier i) -> i)
            |> String.join "."
    in
        List.concatMap (typeDeclarationTS package) types

typeDeclarationTS : String -> JP.TypeDeclaration -> List Subgraph
typeDeclarationTS pkg typeDecl =
    case typeDecl of
        JP.TypeDeclaration_ClassDeclaration c ->
            classDeclarationTS pkg c
        JP.TypeDeclaration_InterfaceDeclaration i ->
            Debug.todo "TODO"
        JP.TypeDeclaration_Semi ->
            []

classDeclarationTS : String -> JP.ClassDeclaration -> List Subgraph
classDeclarationTS pkg classDecl =
    case classDecl of
        JP.ClassDeclaration_Normal n ->
            normalClassDeclarationTS pkg n
        JP.ClassDeclaration_Enum e ->
            Debug.todo "TODO"

normalClassDeclarationTS : String -> JP.NormalClassDeclaration -> List Subgraph
normalClassDeclarationTS pkg (JP.NormalClassDeclaration mods id typeParams superclass superinterfaces classBody) =
  { entity = { pkg = pkg
             , name = AstHelpers.typeIdentifierToString id
             , kind = Class 
             , access = if List.member JP.ClassModifier_Public mods
                          then Public
                          else if List.member JP.ClassModifier_Private mods
                            then Private
                            else Protected
             , static = List.member JP.ClassModifier_Static mods
             , final = List.member JP.ClassModifier_Final mods
             , abstract = List.member JP.ClassModifier_Abstract mods
             , publicAttributes = []
             , publicMethods = []
             , expansion = Not
             }
  , parent = Maybe.map (mkNodeId pkg << superclassToString) superclass
  , interfaces = [] --List.filterMap onlyRefTypes class.implements
  , references = getReferences pkg classBody
  } :: []

superclassToString : JP.Superclass -> String
superclassToString (JP.Superclass classtype) =
    case classtype of
        JP.ClassType_NoPackage _ id _ ->
            AstHelpers.typeIdentifierToString id
        _ ->
            "TODO" -- TODO

-- }}}

-- TODO could be a reference anywhere in here...
getReferences : String -> JP.ClassBody -> List NodeId
getReferences pkg body =
  let
    allReferences =
        AstHelpers.getRefsInClassBody body
        |> removeDuplicates
        |> List.map (prefixIfNotAlready pkg)
  in
    Debug.log "refs" allReferences

prefixIfNotAlready : String -> String -> String
prefixIfNotAlready pkg name =
    if String.contains "." name
        then name
        else pkg ++ "." ++ name

removeDuplicates : List comparable -> List comparable
removeDuplicates = Set.toList << Set.fromList

{-
getPublicAttributes : JP.ClassBody -> List Attribute
getPublicAttributes body =
  body.declarations
  |> List.filterMap onlyMembers
  |> List.map Tuple.second
  |> List.filterMap memberToAttribute

memberToAttribute : JP.MemberDecl -> Maybe Attribute
memberToAttribute memberDecl =
  case memberDecl of 
    JP.MDMethodOrField { type_, identifier, rest }
      -> Just { identifier = identifier
              , prettyTypeName = typeToPrettyString type_
              , typeIdentifiers = typeToIdentifiers type_
              , multiple = case type_ of
                             JP.ArrayType _ -> True
                             _              -> False
              }
    JP.MDVoidMethod { identifier, rest }
      -> Just { identifier = identifier
              , prettyTypeName = "void"
              , typeIdentifiers = []
              , multiple = False
              }
    JP.MDConstructor { identifier, rest }
      -> Just { identifier = ""
              , prettyTypeName = identifier
              , typeIdentifiers = []
              , multiple = False
              }
    JP.MDGenericMethodOrConstructor genericMethodOrConstructorDecl
      -> Nothing -- TODO
    JP.MDClass classDeclaration
      -> Nothing
    JP.MDInterface interfaceDeclaration
      -> Nothing

-- no brackets: typeToString (Array Int) = "int"
typeToIdentifiers : JP.Type -> List String
typeToIdentifiers type_ =
  case type_ of
    JP.BasicType JP.Byte    -> [ "byte" ]
    JP.BasicType JP.Short   -> [ "short" ]
    JP.BasicType JP.Char    -> [ "char" ]
    JP.BasicType JP.Int     -> [ "int" ]
    JP.BasicType JP.Long    -> [ "long" ]
    JP.BasicType JP.Float   -> [ "float" ]
    JP.BasicType JP.Double  -> [ "double" ]
    JP.BasicType JP.Boolean -> [ "boolean" ]
    JP.ArrayType t          -> typeToIdentifiers t
    JP.RefType refType      -> refTypeToIdentifiers refType

refTypeToIdentifiers : JP.ReferenceType -> List String
refTypeToIdentifiers refType =
 refType
 |> Nonempty.map (\(name, typeArguments) ->
     name :: List.concatMap (unnamed >> typeToIdentifiers) typeArguments)
 |> Nonempty.toList
 |> List.concat

-- no brackets: typeToString (Array Int) = "int[]"
typeToPrettyString : JP.Type -> String
typeToPrettyString type_ =
  case type_ of
    JP.BasicType JP.Byte    -> "byte"
    JP.BasicType JP.Short   -> "short"
    JP.BasicType JP.Char    -> "char"
    JP.BasicType JP.Int     -> "int"
    JP.BasicType JP.Long    -> "long"
    JP.BasicType JP.Float   -> "float"
    JP.BasicType JP.Double  -> "double"
    JP.BasicType JP.Boolean -> "boolean"
    JP.ArrayType t          -> typeToPrettyString t ++ "[]"
    JP.RefType refType      -> refTypeToPrettyString refType

nempJoin : String -> Nonempty String -> String
nempJoin join (Nonempty str rest) =
    case rest of 
        [] -> str
        _  -> str ++ join ++ String.join join rest

typeArgsToPrettyString : List JP.TypeArgument -> String
typeArgsToPrettyString args =
 args
 |> List.map (unnamed >> typeToPrettyString)
 |> String.join ", "
 |> (\str -> if String.isEmpty str then "" else "<" ++ str ++ ">")

refTypeToPrettyString : JP.ReferenceType -> String
refTypeToPrettyString refType =
 refType
 |> Nonempty.map (\(name, typeArguments) ->
     name ++ typeArgsToPrettyString typeArguments)
 |> nempJoin "."

unnamed : JP.TypeArgument -> JP.Type
unnamed typeArg =
  case typeArg of
    JP.ReferenceTypeArgument rt -> JP.RefType rt
    JP.WildCardSuper rt         -> JP.RefType rt
    JP.WildCardExtends rt       -> JP.RefType rt

getPublicMethods : JP.ClassBody -> List Method
getPublicMethods body = []

-- we don't care about basic or array types... they can't be extended
onlyRefTypes : JP.Type -> Maybe String
onlyRefTypes type_ =
  case type_ of
    JP.BasicType basic -> Nothing
    JP.ArrayType t -> Nothing
    JP.RefType (Nonempty (id, _) rest) ->
        Just <| id ++ String.join "." (List.map Tuple.first rest)

onlyMembers : JP.ClassBodyDeclaration -> Maybe (List JP.Modifier, JP.MemberDecl)
onlyMembers dec =
  case dec of
    JP.CBSemicolon -> Nothing 
    JP.CBMember { modifiers, decl } -> Just (modifiers, decl)
    JP.CBBlock { static, block } -> Nothing
-}
