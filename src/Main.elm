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
import CustomElement as CElement

type alias Model =
  { config : Config
  , combined : Combined.Model
  , selectFiles : Bool
  }

type Msg
  = UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | Tick Float
  | CombinedMsg Combined.Msg
  | SelectFiles Bool
  | SetFileSelected File Bool

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
       , selectFiles = False
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
            ] <|
            CElement.column
                [ Element.map CombinedMsg (Combined.view model.combined)
                , viewOverlay model
                ]
        ]
    }

viewOverlay : Model -> Element (Msg)
viewOverlay model =
    if model.selectFiles then
       viewSelectFilesPopup model.combined.files
    else
       CElement.column <|
         [ CElement.button 
           { onPress = Just (SelectFiles True)
           , label = "Select Files..."
           }
         ] ++
         ( Combined.viewOverlay model.combined
           |> List.map (Element.map CombinedMsg)
         )

viewSelectFilesPopup : List (File, Bool) -> Element (Msg)
viewSelectFilesPopup files =
  CElement.column <|
    ( CElement.button
      { onPress = Just (SelectFiles False)
      , label = "← Back"
      }
    )
    :: (List.map viewFileSelect files)

viewFileSelect : (File, Bool) -> Element (Msg)
viewFileSelect (file, sel) =
  Element.row
    [ Element.spacing 8 ]
    [ CElement.text <| trimUntilRev (\c -> c == '/') file.uri
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

toggleFileButton : (File, Bool) -> Element (Msg)
toggleFileButton (file, sel) =
  CElement.button
    { onPress = Just <| SetFileSelected file (not sel)
    , label = if sel then "✓" else "✕"
    }

setFiles _ model = (model, Cmd.none) -- TODO

update : Msg -> Model -> (Model, Cmd (Msg))
update msg model =
  case msg of 
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
    SelectFiles b ->
      ( { model | selectFiles = b }
      , Cmd.none
      )
    SetFileSelected file selected ->
      let
        files =
          model.combined.files
          |> List.map (\(f, s) -> if f.uri == file.uri
                                    then (file, selected) else (f, s))
      in
        setFiles files model

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
    [ updateFile UpdateFile
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

