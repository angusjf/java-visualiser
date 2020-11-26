port module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Input
import Element.Font
import Visualiser
import Config exposing (Config)
import Graph exposing (Graph)
import VsColor
import Instance exposing (Instance)
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

type alias Current n e =
  { instance : Instance n e (Visualiser.Msg n)
  , fromSources : List String -> Graph n e
  }

init = init2
      { instance = { viewNode = Package.Visualiser.viewNode
                   , viewEdge = Package.Visualiser.viewEdge
                   , onClick  = Package.Visualiser.onClick
                   , getRect  = Package.Visualiser.getRect
                   }
      , fromSources = Package.JavaToGraph.fromSources
      }

toGraph : (List String -> Graph n v) -> List (File, Bool) -> Graph n v
toGraph fromSources =
  List.filterMap (\(f, b) -> if b then Just f else Nothing)
  >> List.map .content
  >> fromSources

init2 : Current n v -> (Config, List File) -> (Model n v, Cmd (Msg n))
init2 current (config, files) =
  ({ files = List.map (\f -> (f, True)) files
   , config = config
   , visualiser =
       Visualiser.init
       config
       (toGraph current.fromSources (List.map (\f -> (f, True)) files))
       current.instance
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
        (mo, me) = Visualiser.update model.config vMsg model.visualiser
      in
        ( { model | visualiser = mo }
        , Cmd.map VisualiserMsg me
        )
    ConfigChanged cfg ->
      ( { model
          | config = cfg
          , visualiser = Visualiser.withConfig cfg model.visualiser
        }
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
  , body = [ Element.layout
             [ VsColor.fontColor VsColor.Foreground
             , Element.Font.size 13
             ] <|
               Element.column []
                 [ Visualiser.view model.config model.visualiser
                   |> Element.html
                   |> Element.map VisualiserMsg
                 , viewOverlay model
                 ]
           ]
  }

viewOverlay : Model n v -> Element (Msg n)
viewOverlay model =
 if model.selectFilesPopup then
   viewSelectFilesPopup model.files
 else
   Element.column
     []
     [ Element.Input.button 
       []
       { onPress = Just (ViewSelectFiles True)
       , label = Element.text "Select Files..."
       }
     , Visualiser.viewOverlay model.visualiser
           |> Element.map VisualiserMsg
     ]

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
    [ Element.text <| trimUntilRev (\c -> c == '/') file.uri
    , toggleFileButton (file, sel)
    ]

trimUntilRev fn str =
  let prefix = String.reverse <| trimUntil fn <| String.reverse str
  in String.replace prefix "" str

trimUntil : (Char -> Bool) -> String -> String
trimUntil f str =
  let
    helper : List Char -> List Char
    helper chars =
      case chars of 
        x::xs -> if f x then xs
                        else helper xs
        [] -> []
  in
    str
    |> String.toList
    |> helper
    |> String.fromList

toggleFileButton : (File, Bool) -> Element (Msg n)
toggleFileButton (file, sel) =
  Element.Input.button
    []
    { onPress = Just <| SetFileSelected file (not sel)
    , label = Element.text <| if sel then " [exclude]" else " [include]"
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
