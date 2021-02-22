port module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Element)
import Element.Input
import Element.Font
import Config exposing (Config)
import VsColor
import Combined
import File exposing (File, Uri)

type alias Model =
  { config : Config
  , combined : Combined.Model
  }

type Msg
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | Tick Float
  | CombinedMsg Combined.Msg

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg
port renameFile : ((Uri, Uri) -> msg) -> Sub msg
port configChanged : (Config -> msg) -> Sub msg
port infoMessage : String -> Cmd msg

init : Config -> (Model, Cmd (Msg))
init config =
  let
      (cModel, cMsg) = Combined.init config
  in
      ({ config = config
       , combined = cModel
       }
      , Cmd.map CombinedMsg cMsg
      )

view : Model -> Browser.Document (Msg)
view model =
    { title = "Visualiser"
    , body =
        [ Element.layout
            [ VsColor.fontColor VsColor.Foreground
            , Element.Font.size 13
            , Element.padding 4
            ]
            (Element.map CombinedMsg (Combined.view model.combined))
        ]
    }

update : Msg -> Model -> (Model, Cmd (Msg))
update msg model =
  case msg of 
    NewFile file ->
      setFiles (insert file model.combined.files) model
    UpdateFile file ->
      setFiles (insert file model.combined.files) model
    DeleteFile uri ->
      setFiles (delete uri model.combined.files) model
    RenameFile (from, to) ->
      setFiles (rename from to model.combined.files) model
    ConfigChanged cfg ->
      ( { model
          | config = cfg
          , combined =
              let
                  c = model.combined
              in
                { c | config = cfg }
        }
      , Cmd.none
      )
    Tick _ ->
      ( { model | combined = Combined.tick model.combined }
      , Cmd.none
      )
    CombinedMsg cMsg ->
      let
        (cModel, cMsg2) = Combined.update cMsg model.combined
      in
      ( { model | combined = cModel }
      , Cmd.map CombinedMsg cMsg2
      )

setFiles : List (File, Bool) -> Model -> (Model, Cmd (Msg))
setFiles files model =
    update (CombinedMsg (Combined.SetFiles files)) model

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

subscriptions : Model -> Sub (Msg)
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

