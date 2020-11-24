module Package.JavaToGraph exposing (fromSources)

import Graph exposing (NodeId)
import Package.Graph exposing (..)
import JavaParser as JP
import Parser
import List.Nonempty as Nonempty exposing (Nonempty(..))

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = pkg ++ "." ++ class

type alias Subgraph =
  { entity : Entity
  , parent : Maybe NodeId
  , interfaces : List NodeId
  , references : List NodeId
  }

fromSources : List String -> PackageGraph
fromSources srcs =
  let
    subgraphs : List Subgraph
    subgraphs =
      srcs
      |> List.filterMap toAst
      --|> Debug.log "asts: "
      |> List.concatMap compUnitToSubgraph
  in
    { nodes =
        List.map subgraphToNode subgraphs
    , edges = 
        (List.filterMap subgraphToExtension subgraphs) ++
        (List.concatMap subgraphToImplements subgraphs) ++
        (List.concatMap subgraphToReferences subgraphs)
    }

toAst : String -> Maybe JP.CompilationUnit
toAst src = toMaybeLog <| JP.parse JP.compilationUnit src

toMaybeLog : Result a b -> Maybe b
toMaybeLog res =
  case res of
    Ok x ->
      Just x
    Err e ->
      always Nothing (Debug.log "toMaybeLog: " e)

compUnitToSubgraph : JP.CompilationUnit -> List Subgraph
compUnitToSubgraph unit =
  let
    pkg = Maybe.withDefault "" unit.package
  in
    List.filterMap (typeToSubgraph pkg) unit.types

typeToSubgraph : String -> JP.TypeDeclaration -> Maybe Subgraph
typeToSubgraph pkg t =
  case t of
    JP.ClassOrInterface coi ->
      case coi of
        JP.Class mod c ->
          case c of
            JP.NormalClass data ->
              Just <| normalClassToSubgraph data pkg mod
            JP.Enum data ->
              Just <| enumToSubgraph data pkg mod
        JP.Interface mod data ->
          Just <| interfaceToSubgraph data pkg mod
    JP.Semicolon ->
      Nothing

normalClassToSubgraph : JP.NormalClassDeclaration -> String
                            -> (List JP.Modifier) -> Subgraph
normalClassToSubgraph class pkg mod =
  { entity = { id = mkNodeId pkg class.identifier
             , name = class.identifier
             , kind = Class 
             , access = if List.member JP.Public mod
                          then Public
                          else if List.member JP.Private mod
                            then Private
                            else Protected
             , static = List.member JP.Static mod
             , final = List.member JP.Final mod
             , abstract = List.member JP.Abstract mod
             , publicAttributes = getPublicAttributes class.body
             , publicMethods = getPublicMethods class.body
             , expansion = Not
             }
  , parent = Maybe.map (mkNodeId pkg) (Maybe.andThen onlyRefTypes class.extends)
  , interfaces = List.filterMap onlyRefTypes class.implements
  , references = getReferences pkg class.body
  }

enumToSubgraph : JP.EnumDeclaration -> String -> (List JP.Modifier) -> Subgraph
enumToSubgraph enum pkg mod = Debug.todo "enumToSubgraph"

interfaceToSubgraph : JP.InterfaceDeclaration -> String -> (List JP.Modifier) -> Subgraph
interfaceToSubgraph enum pkg mod = Debug.todo "interfaceToSubgraph"

-- TODO could be a reference anywhere in here...
getReferences : String -> JP.ClassBody -> List NodeId
getReferences pkg body =
  body.declarations
  |> List.filterMap onlyMembers
  |> List.map Tuple.second
  |> List.filterMap memberToAttribute
  |> List.concatMap (attributeToNodeIds pkg)

-- TODO: I should check imports... package might be different
attributeToNodeIds : String -> Attribute -> List NodeId
attributeToNodeIds pkg { typeIdentifiers } =
    List.map (mkNodeId pkg) typeIdentifiers

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
 |> List.concatMap identity

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

subgraphToExtension : Subgraph -> Maybe Edge
subgraphToExtension { entity, parent } =
  parent
  |> Maybe.map (\p -> { from = entity.id, to = p, data = Extends })

subgraphToImplements : Subgraph -> List Edge
subgraphToImplements { entity, interfaces } =
  interfaces
  |> List.map (\i -> { from = entity.id, to = i, data = Implements })

subgraphToReferences : Subgraph -> List Edge
subgraphToReferences { entity, references } =
  references
  |> List.map (\ref -> { from = entity.id, to = ref, data = References })

subgraphToNode : Subgraph -> Node
subgraphToNode { entity } =
    { id = entity.id, data = entity }
