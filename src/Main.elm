port module Main exposing (main)

import Browser
import Browser.Events
import Html
import File exposing (File, Uri)
import Visualiser
import Config exposing (Config)
import Graph

import Package.JavaToGraph
import Package.Visualiser

type alias Model n v =
  { files : List File
  , visualiser : Visualiser.Model n v
  , config : Config
  , nodeViewer : Visualiser.NodeViewer n
  , vertexViewer : Visualiser.VertexViewer n v
  , filesToGraph : List File -> Graph.Graph n v
  }

type Msg n
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | VisualiserMsg (Visualiser.Msg n)
  | Tick Float

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg
port renameFile : ((Uri, Uri) -> msg) -> Sub msg
port configChanged : (Config -> msg) -> Sub msg

current =
  { viewNode = Package.Visualiser.viewNode
  , viewVertex = Package.Visualiser.viewVertex
  , filesToGraph = \files -> Package.JavaToGraph.fromSources
                                    (List.map .content files)
  }

--init : (Config, List File) -> (Model n v, Cmd (Msg n))
init (config, files) =
  ({ files = files
   , config = config
   , visualiser =
       Visualiser.init
       config
       (current.filesToGraph files)
       current.viewNode
       current.viewVertex
   , nodeViewer = current.viewNode
   , vertexViewer = current.viewVertex
   , filesToGraph = current.filesToGraph
   }
  , Cmd.none
  )

update : Msg n -> Model n v -> (Model n v, Cmd (Msg n))
update msg model =
  case msg of 
    NewFile file ->
      ( setFiles (file :: model.files) model
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

setFiles : List File -> Model n v -> Model n v
setFiles files model =
  { model
    | files = files
    , visualiser = Visualiser.withGraph
                     model.config
                     (model.filesToGraph files)
                     model.visualiser
  }

insert : File -> List File -> List File
insert file files =
  case files of
    x::xs ->
      if file.uri == x.uri
        then file :: xs
        else x :: insert file xs
    [] -> [ file ]

delete : Uri -> List File -> List File
delete uri files =
  case files of
    x::xs ->
      if x.uri == uri
        then xs
        else x :: delete uri xs
    [] -> []

rename : Uri -> Uri -> List File -> List File
rename from to files =
  case files of
    x::xs ->
      if x.uri == from
        then { x | uri = to } :: xs
        else x :: rename from to xs
    [] -> []

view : Model n v -> Browser.Document (Msg n)
view model =
  { title = "I don't think you can see this"
  , body = [ Visualiser.view model.config model.visualiser
             |> Html.map VisualiserMsg
           ]
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
