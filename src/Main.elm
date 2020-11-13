port module Main exposing (main)

import Browser
import Html

import File exposing (File, Uri)
import JavaToGraph
import Visualiser
import Config exposing (Config)

type alias Model =
  { files : List File
  , visualiser : Visualiser.Model
  , config : Config
  }

type Msg
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | VisualiserMsg Visualiser.Msg

init : (Config, List File) -> (Model, Cmd Msg)
init (config, files) =
  ({ files = files
   , visualiser = Visualiser.init config (filesToGraph files)
   , config = config
   }
  , Cmd.none
  )

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg
port renameFile : ((Uri, Uri) -> msg) -> Sub msg
port configChanged : (Config -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
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

setFiles : List File -> Model -> Model
setFiles files model =
  { model
    | files = files
    , visualiser = Visualiser.withGraph
                     model.config
                     (filesToGraph files)
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

filesToGraph files = 
  List.map (\f -> f.content) files |> JavaToGraph.fromSources

view : Model -> Browser.Document Msg
view model =
  { title = "I don't think you can see this"
  , body = [ Visualiser.view model.config model.visualiser
             |> Html.map VisualiserMsg
           ]
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ newFile NewFile
    , updateFile UpdateFile
    , deleteFile DeleteFile
    , renameFile RenameFile
    , configChanged ConfigChanged
    ]

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
