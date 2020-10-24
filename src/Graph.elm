module Graph exposing (..)

import File exposing (File, Uri)
import JavaParser

type alias Graph =
  { classes : List Class
  }

type alias Class =
  { name : String
  , public : Bool
  }

fromFiles : List File -> Graph
fromFiles files =
  { classes =
      let
        classes = List.concatMap (\file -> getClassNames file.content) files
      in
        List.map (\(name, pub) -> { name = name, public = pub }) classes
  }

getClassNames : String -> List (String, Bool)
getClassNames src =
  case JavaParser.toAst src of
    Nothing -> [ ("oh no", False) ]
    Just unit -> List.map typeToName unit.types

typeToName t =
  case t of
    JavaParser.ClassOrInterface coi ->
      case coi of
        JavaParser.Class mod c ->
          case c of
            JavaParser.NormalClass data ->
              (data.identifier, List.member JavaParser.Public mod)
            _ ->
              ("", False)
        _ ->
          ("", False)
    _ ->
      ("", False)

getClassNamesRegex : String -> List String
getClassNamesRegex src =
  let
    lines = String.split "\n" src
    classNames = List.concatMap extractClassNames lines
    extractClassNames line =
        if String.startsWith "public abstract class" line
            then [ stripPublicAbstractClass line ]
            else if String.startsWith "public class" line
                 then [ stripPublicClass line ]
                 else if String.startsWith "class" line
                      then [ stripClass line ]
                      else []
    stripPublicAbstractClass str = untilSpace <| String.dropLeft 22 str
    stripPublicClass str = untilSpace <| String.dropLeft 13 str
    stripClass str = untilSpace <| String.dropLeft 6 str
    untilSpace str = String.fromList <| untilSpaceL <| String.toList str
    untilSpaceL xs =
      case xs of
        ' '::rest -> []
        x::rest -> x :: untilSpaceL rest
        [] -> []
  in
    classNames
