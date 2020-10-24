port module Main exposing (main)

import Browser
import Graphics as G exposing (Svg)
import Graph exposing (Graph)
import File exposing (File, Uri)

type alias Model =
  { graph : Graph
  , files : List File
  }

type Msg
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri

init : List File -> (Model, Cmd Msg)
init files =
  ({ graph = Graph.fromFiles files
   , files = files
   }
  , Cmd.none
  )

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newFiles = 
      case msg of 
        NewFile file ->
          file :: model.files
        UpdateFile file ->
          replace file model.files -- TODO
        DeleteFile uri ->
          model.files -- TODO
  in
    ({ model | files = newFiles
             , graph = Graph.fromFiles newFiles
     }
    , Cmd.none
    )

replace : File -> List File -> List File
replace file files =
  case files of
    x::xs -> if file.uri == x.uri then file :: xs else x :: replace file xs
    [] -> []

view : Model -> Browser.Document Msg
view model =
  { title = "I don't think you can see this"
  , body = [ G.render [ viewGraph model.graph ] ]
  }

viewGraph : Graph -> Svg Msg
viewGraph graph =
  let
    l = List.length graph.classes
    xs = List.map (\x -> x + 0) <| List.repeat l 0
    ys = List.map (\y -> y * 60 + 0) <| List.range 0 l
    classPos = zip3 xs ys graph.classes
  in
    G.group <| List.map viewClass classPos

viewClass : (Int, Int, Graph.Class) -> Svg Msg
viewClass (x, y, class) =
  let
    text = G.text (x + 10) (y + 30) class.name
    box = (if class.public then G.rect2 else G.rect1) x y 120 50
  in
    G.group [ box, text ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ newFile NewFile
    , updateFile UpdateFile
    , deleteFile DeleteFile
    ]

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 aList bList cList =
  case (aList, bList, cList) of
    (a::aa, b::bb, c::cc) -> (a, b, c) :: zip3 aa bb cc
    _ -> []

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
