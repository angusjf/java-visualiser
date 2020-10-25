module JavaToGraph exposing (..)

import Graph exposing (Graph, NodeId, mkNodeId)
import JavaParser as JP
import Parser

fromSources : List String -> Graph
fromSources srcs =
  let
    asts : List JP.CompilationUnit
    asts = List.filterMap toAst srcs

    toAst : String -> Maybe JP.CompilationUnit
    toAst src = Result.toMaybe (Parser.run JP.compilationUnit src)

    classes : List Graph.Class
    classes = List.concatMap compUnitToClasses asts
  in
    { classes = classes
    }

compUnitToClasses : JP.CompilationUnit -> List Graph.Class
compUnitToClasses unit =
  List.filterMap (typeToClass unit.package) unit.types

typeToClass : Maybe String -> JP.TypeDeclaration -> Maybe Graph.Class
typeToClass pkg t =
  case t of
    JP.ClassOrInterface coi ->
      case coi of
        JP.Class mod c ->
          case c of
            JP.NormalClass data ->
              Just
                { id = mkNodeId (Maybe.withDefault "[NOPKG]" pkg) data.identifier
                , name = data.identifier
                , public = List.member JP.Public mod
                , extends = 
              Maybe.map2
                (\class p -> mkNodeId p class)
                (onlyRefTypes data.extends)
                pkg
                }
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
