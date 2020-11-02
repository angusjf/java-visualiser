module JavaToGraph exposing (..)

import Graph exposing (Graph, NodeId, mkNodeId, Kind(..), Vertex)
import JavaParser as JP
import Parser

type alias ClassData = (Graph.Class, Maybe NodeId, List NodeId)

fromSources : List String -> Graph
fromSources srcs =
  let
    asts : List JP.CompilationUnit
    asts = List.filterMap toAst srcs

    toAst : String -> Maybe JP.CompilationUnit
    toAst src = Result.toMaybe (Parser.run JP.compilationUnit src)

    classes : List ClassData
    classes = List.concatMap compUnitToClasses asts

    extensions : List Vertex
    extensions =
      dropMaybes <|
        List.map
          (\(class, parent, _) ->
            case parent of 
              Just p ->
                Just { from = class.id, to = p }
              Nothing ->
                Nothing
          )
          classes
  in
    { classes = List.map (\(x, _, _) -> x) classes
    , extensions = extensions
    , implements = []
    }

compUnitToClasses : JP.CompilationUnit -> List ClassData
compUnitToClasses unit =
  List.filterMap (typeToClass unit.package) unit.types

typeToClass : Maybe String -> JP.TypeDeclaration -> Maybe ClassData
typeToClass pkg t =
  case t of
    JP.ClassOrInterface coi ->
      case coi of
        JP.Class mod c ->
          case c of
            JP.NormalClass data ->
              Just
                ({ id = mkNodeId (Maybe.withDefault "[NOPKG]" pkg) data.identifier
                 , name = data.identifier
                 , kind = if List.member JP.Public mod
                            then Public
                            else Normal
                 }
                , Maybe.map2
                    (\class p -> mkNodeId p class)
                    (onlyRefTypes data.extends)
                    pkg
                , []
                )
            JP.Enum _ ->
              Nothing
        JP.Interface _ _ ->
          Nothing -- TODO
    JP.Semicolon ->
      Nothing
      {-
getExtensions : List JP.CompilationUnit -> List (NodeId, NodeId)
getExtensions units =
  let
    typesAndUnit : List (JP.CompilationUnit, JP.TypeDeclaration)
    typesAndUnit = List.concatMap (\u -> List.map (\t -> (u, t)) u.types) units

    typeToPair : (JP.CompilationUnit, JP.TypeDeclaration) -> Maybe (NodeId, Maybe NodeId)
    typeToPair (unit, t) =
      Maybe.map
        (\class -> (class.id, ext unit t))
        (typeToClass unit.package t)

    exts : List (NodeId, NodeId)
    exts =
      List.filterMap
        (\(a, b) ->
          case b of 
            Just z -> Just (a, z)
            Nothing -> Nothing
        )
        <| List.filterMap typeToPair typesAndUnit
  in
    exts

ext : JP.CompilationUnit -> JP.TypeDeclaration -> Maybe NodeId
ext unit t =
  case t of
    JP.ClassOrInterface coi ->
      case coi of
        JP.Class mod c ->
          case c of
            JP.NormalClass data ->
              Maybe.map2
                (\class pkg -> mkNodeId pkg class)
                (onlyRefTypes data.extends)
                unit.package
            JP.Enum _ ->
              Nothing
        JP.Interface _ _ ->
          Nothing -- TODO
    JP.Semicolon ->
      Nothing
      -}

onlyRefTypes : Maybe JP.Type -> Maybe String
onlyRefTypes maybeType =
  case maybeType of
    Just (JP.ReferenceType (JP.RT name)) -> Just name
    _ -> Nothing

dropMaybes : List (Maybe a) -> List a
dropMaybes list =
  case list of
    (Just x) :: xs -> x :: dropMaybes xs
    Nothing :: xs -> dropMaybes xs
    [] -> []
