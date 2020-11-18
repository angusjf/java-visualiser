port module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Input
import Visualiser
import Config exposing (Config)
import Graph exposing (Graph)

import Package.JavaToGraph
import Package.Visualiser
import Package.Graph

type alias Uri = String

type alias File =
  { uri : Uri
  , content : String
  }

type alias Model n v =
  { files : List (File, Bool)
  , visualiser : Visualiser.Model n v
  , config : Config
  , fromSources : List String -> Graph n v
  , selectFilesPopup : Bool
  }

type Msg n
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | VisualiserMsg (Visualiser.Msg n)
  | Tick Float
  | ViewSelectFiles Bool
  | SetFileSelected File Bool

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg
port renameFile : ((Uri, Uri) -> msg) -> Sub msg
port configChanged : (Config -> msg) -> Sub msg

current =
  { viewNode = Package.Visualiser.viewNode
  , viewVertex = Package.Visualiser.viewVertex
  , fromSources = Package.JavaToGraph.fromSources
  }

toGraph : (List String -> Graph n v) -> List (File, Bool) -> Graph n v
toGraph fromSources =
  List.filterMap (\(f, b) -> if b then Just f else Nothing)
  >> List.map .content
  >> fromSources

init : (Config, List File) -> ( Model Package.Graph.Entity Package.Graph.Link
                              , Cmd (Msg Package.Graph.Entity)
                              )
init (config, files) =
  ({ files = List.map (\f -> (f, True)) files
   , config = config
   , visualiser =
       Visualiser.init
       config
       (toGraph current.fromSources (List.map (\f -> (f, True)) files))
       current.viewNode
       current.viewVertex
   , fromSources = current.fromSources
   , selectFilesPopup = False
   }
  , Cmd.none
  )

update : Msg n -> Model n v -> (Model n v, Cmd (Msg n))
update msg model =
  case msg of 
    NewFile file ->
      ( setFiles (insert file model.files) model
      , Cmd.none
      )
    UpdateFile file ->
      ( setFiles (insert file model.files) model
      , Cmd.none
      )
    DeleteFile uri ->
      ( setFiles (delete uri model.files) model
      , Cmd.none
      )
    RenameFile (from, to) ->
      ( setFiles (rename from to model.files) model
      , Cmd.none
      )
    VisualiserMsg vMsg ->
      let
        (mo, me) = Visualiser.update vMsg model.visualiser
      in
        ( { model | visualiser = mo }
        , Cmd.map VisualiserMsg me
        )
    ConfigChanged cfg ->
      ( { model | config = cfg }
      , Cmd.none
      )
    Tick _ ->
      ( { model | visualiser = Visualiser.tick model.visualiser }
      , Cmd.none
      )
    ViewSelectFiles bool ->
      ( { model | selectFilesPopup = bool }
      , Cmd.none
      )
    SetFileSelected file selected ->
      let
        files =
          model.files
          |> List.map (\(f, s) -> if f.uri == file.uri
                                    then (file, selected) else (f, s))
      in
        ( setFiles files model
        , Cmd.none
        )

{-
matchUp : List (File, Bool) -> File -> (File, Bool)
matchUp oldFiles new =
  case List.filter (\(f, _) -> f.uri == new.uri) oldFiles of
    item::_ -> item
    [] -> (new, True)
-}

setFiles : List (File, Bool) -> Model n v -> Model n v
setFiles files model =
  { model
    | files = files
    , visualiser = Visualiser.withGraph
                     model.config
                     (toGraph model.fromSources files)
                     model.visualiser
  }

insert : File -> List (File, Bool) -> List (File, Bool)
insert file files =
  case files of
    (x, b)::xs ->
      if file.uri == x.uri
        then (file, True) :: xs
        else (x, b) :: insert file xs
    [] -> [ (file, True) ]

delete : Uri -> List (File, Bool) -> List (File, Bool)
delete uri files =
  case files of
    (x, b)::xs ->
      if x.uri == uri
        then xs
        else (x, b) :: delete uri xs
    [] -> []

rename : Uri -> Uri -> List (File, Bool) -> List (File, Bool)
rename from to files =
  case files of
    (x, b)::xs ->
      if x.uri == from
        then ({ x | uri = to }, b) :: xs
        else (x, b) :: rename from to xs
    [] -> []

view : Model n v -> Browser.Document (Msg n)
view model =
  { title = "Visualiser"
  , body = [ Element.layoutWith { options = [{-Element.noStaticStyleSheet-}] }
             [] <|
             if model.selectFilesPopup then
               viewSelectFilesPopup model.files
             else
               Element.column []
                 [ Element.Input.button 
                   []
                   { onPress = Just (ViewSelectFiles True)
                   , label = Element.text "Select Files..."
                   }
                 , Visualiser.view model.config model.visualiser
                   |> Element.map VisualiserMsg
                 ]
           ]
  }

viewSelectFilesPopup : List (File, Bool) -> Element (Msg n)
viewSelectFilesPopup files =
  Element.column
    [] <|
    (List.map viewFileSelect files) ++ 
    [ Element.Input.button
      []
      { onPress = Just (ViewSelectFiles False)
      , label = Element.text "back"
      }
    ]

viewFileSelect : (File, Bool) -> Element (Msg n)
viewFileSelect (file, sel) =
  Element.row
    []
    [ Element.text file.uri
    , toggleFileButton (file, sel)
    ]

toggleFileButton : (File, Bool) -> Element (Msg n)
toggleFileButton (file, sel) =
  Element.Input.button
    []
    { onPress = Just <| SetFileSelected file (not sel)
    , label = Element.text <| if sel then "yes" else "no"
    }

subscriptions : Model n v -> Sub (Msg n)
subscriptions model =
  Sub.batch
    [ newFile NewFile
    , updateFile UpdateFile
    , deleteFile DeleteFile
    , renameFile RenameFile
    , configChanged ConfigChanged
    , Browser.Events.onAnimationFrameDelta Tick
    ]

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
