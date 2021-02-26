module Project.JavaToGraph exposing (PartialData, fromSource, toGraph, fromSources)

import Graph exposing (NodeId)
import Project.Graph exposing (..)
import Java15Parser as JP
import Java15AstHelpers as AstHelpers
import Parser
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Set

type alias PartialData =
  { packageName : String
  , imports : List String
  }

fromSource : String -> Maybe PartialData
fromSource = compilationUnitToData

toGraph : List PartialData -> ProjectGraph
toGraph data =
        { nodes = partialDataToNodes data
        , edges = nub <| partialDataToEdges data
        }

fromSources : List String -> ProjectGraph
fromSources srcs = toGraph (List.filterMap fromSource srcs)

nub : List Edge -> List Edge
nub l =
    let
        isEqual a b = a.from == b.from && a.to == b.to && a.data == b.data
    in
        case l of
            [] -> []
            x::xs -> x :: nub (List.filter (not << (isEqual x)) xs)

partialDataToNodes : List PartialData -> List Node
partialDataToNodes data =
    let
        packages = 
            data
            |> List.map .packageName
            |> Set.fromList
            |> Set.toList
            |> List.map lastPackageIdentifier
            |> List.map (\name -> { data = { name = name
                                           , expanded = False
                                           , kind = Package
                                           }
                                  , id = name
                                  }
                        )
    in
        packages

lastPackageIdentifier : String -> String
lastPackageIdentifier =
    (String.split ".") >> listLast >> (Maybe.withDefault "")

listLast : List a -> Maybe a
listLast l =
    case l of
        [ x ] -> Just x
        x :: xs -> listLast xs
        [] -> Nothing

partialDataToEdges : List PartialData -> List Edge
partialDataToEdges data = List.concatMap helper data

helper : PartialData -> List Edge
helper { packageName, imports } =
  List.map (\imp -> { from = lastPackageIdentifier packageName
                    , to = lastPackageIdentifier imp
                    , data = Link
                    }
           )
           imports


compilationUnitToData : String -> Maybe PartialData
compilationUnitToData src =
    case JP.parseJustHeader src of
        Ok (pkgDecl, imports) ->
            case pkgDecl of
                Just (JP.PackageDeclaration _ pkg) ->
                    Just { packageName =
                            List.map AstHelpers.identifierToString pkg
                            |> String.join "."
                         , imports = importsToStrings imports
                         }
                Nothing ->
                    Just { packageName = "default package"
                         , imports = importsToStrings imports
                         }
        Err _ ->
            Nothing

importsToStrings : List JP.ImportDeclaration -> List String
importsToStrings imports =
    imports
    |> List.map AstHelpers.importDeclarationToString
    |> Set.fromList
    |> Set.toList
