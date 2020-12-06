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

type Menu
 = Main
 | SelectFiles

type alias Model n e =
  { files : List (File, Bool)
  , config : Config
  , fromSources : List String -> List (String, Graph n e)
  , instance : Instance n e (Visualiser.Msg n)
  , menu : Menu
  , selectedGraphAndVisualiser : Maybe (String, (Visualiser.Model n e))
  , graphs : List (String, Graph n e)
  }

type Msg n e
  = NewFile File
  | UpdateFile File
  | DeleteFile Uri
  | RenameFile (Uri, Uri)
  | ConfigChanged Config
  | VisualiserMsg (Visualiser.Msg n)
  | Tick Float
  | ViewMenu Menu
  | SetFileSelected File Bool
  | GraphSelected (String, Graph n e)

type alias Current n e =
  { instance : Instance n e (Visualiser.Msg n)
  , fromSources : List String -> List (String, Graph n e)
  }

port newFile : (File -> msg) -> Sub msg
port updateFile : (File -> msg) -> Sub msg
port deleteFile : (Uri -> msg) -> Sub msg
port renameFile : ((Uri, Uri) -> msg) -> Sub msg
port configChanged : (Config -> msg) -> Sub msg
port infoMessage : String -> Cmd msg

toGraph : (List String -> List (String, Graph n e)) -> List (File, Bool)
                                          -> String -> Maybe (Graph n e)
toGraph fromSources files name =
  files
  |> filesToStrings
  |> fromSources
  |> List.filter (\(n, _) -> n == name)
  |> List.head
  |> Maybe.map Tuple.second

filesToStrings : List (File, Bool) -> List String
filesToStrings files =
  files
  |> List.filterMap (\(f, b) -> if b then Just f else Nothing)
  |> List.map .content

init =
  init2
    Package.JavaToGraph.fromSources
      { viewNode = Package.Visualiser.viewNode
      , viewEdge = Package.Visualiser.viewEdge
      , onClick  = Package.Visualiser.onClick
      , getRect  = Package.Visualiser.getRect
      }
          
init2 : (List String -> List (String, Graph n e))
     -> Instance n e (Visualiser.Msg n)
     -> (Config, List File)
     -> (Model n e, Cmd (Msg n e))
init2 fromSources instance (config, inFiles) =
  let
    files = List.map (\f -> (f, True)) inFiles
    graphs = fromSources <| filesToStrings files
  in
    ({ files = files
     , config = config
     , fromSources = fromSources
     , menu = Main
     , graphs = graphs
     , selectedGraphAndVisualiser =
         if List.length graphs == 1 then
            Maybe.map
              (\(n, graph) -> (n, Visualiser.init config graph instance))
              (List.head graphs)
         else Nothing
     , instance = instance
     }
    , Cmd.none
    )

update : Msg n e -> Model n e -> (Model n e, Cmd (Msg n e))
update msg model =
  case msg of 
    NewFile file ->
      setFiles (insert file model.files) model
    UpdateFile file ->
      setFiles (insert file model.files) model
    DeleteFile uri ->
      setFiles (delete uri model.files) model
    RenameFile (from, to) ->
      setFiles (rename from to model.files) model
    VisualiserMsg vMsg ->
      case model.selectedGraphAndVisualiser of
        Just (n, old) ->
          let
            (new, msg_) = Visualiser.update model.config vMsg old
          in
            ( { model
                | selectedGraphAndVisualiser = Just (n, new)
              }
            , Cmd.map VisualiserMsg msg_
            )
        Nothing ->
            ( model
            , Cmd.none
            )
    ConfigChanged cfg ->
      ( { model
          | config = cfg
          , selectedGraphAndVisualiser = 
              Maybe.map
                (\(s, v) -> (s, Visualiser.withConfig cfg v))
                model.selectedGraphAndVisualiser
        }
      , Cmd.none
      )
    Tick _ ->
      ( applyToVis Visualiser.tick model
      , Cmd.none
      )
    ViewMenu menu ->
      ( { model | menu = menu }
      , Cmd.none
      )
    SetFileSelected file selected ->
      let
        files =
          model.files
          |> List.map (\(f, s) -> if f.uri == file.uri
                                    then (file, selected) else (f, s))
      in
        setFiles files model
    GraphSelected (name, graph) ->
      ( { model
          | selectedGraphAndVisualiser =
             Just
               ( name
               , Visualiser.init
                   model.config
                   graph
                   model.instance
               )
        }
      , Cmd.none
      )

setFiles : List (File, Bool) -> Model n e -> (Model n e, Cmd (Msg n e))
setFiles files model =
  let
    newModel = 
      { model
        | files = files
        , graphs = model.fromSources <| filesToStrings <| files
      }
  in
    case model.selectedGraphAndVisualiser of
        Just (selectedGraph, vis) ->
            case toGraph model.fromSources files selectedGraph of
                Just graph ->
                    ( { newModel
                        | selectedGraphAndVisualiser =
                            Just <|
                                ( selectedGraph
                                , Visualiser.withGraph model.config graph vis
                                )
                      }
                    , Cmd.none
                    )
                Nothing ->
                    ( newModel
                    , let msg = "Compilation failed for package '"
                                 ++ selectedGraph ++ "'"
                      in infoMessage msg
                    )
        Nothing ->
            (newModel, Cmd.none)

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

applyToVis : (Visualiser.Model n e -> Visualiser.Model n e) -> Model n e
                                                            -> Model n e
applyToVis f model =
  case model.selectedGraphAndVisualiser of
    Just (selectedGraph, visualiser) ->
        { model
          | selectedGraphAndVisualiser = Just (selectedGraph, f visualiser)
        }
    Nothing -> model

view : Model n e -> Browser.Document (Msg n e)
view model =
  { title = "Visualiser"
  , body = [ Element.layout
             [ VsColor.fontColor VsColor.Foreground
             , Element.Font.size 13
             ] <|
             Element.column [] <|
               case model.selectedGraphAndVisualiser of
                 Just (_, vis) -> 
                   [ Visualiser.view model.config vis
                     |> Element.html
                     |> Element.map VisualiserMsg
                   , viewOverlay model
                   ]
                 Nothing ->
                   [ viewSelectGraph model.graphs
                   ]
           ]
  }

viewSelectGraph : List (String, Graph n e) -> Element (Msg n e)
viewSelectGraph options =
  Element.column
    [] <|
    [ Element.text "Multiple packages in workspace!"
    , Element.text "Please select one to continue:"
    , Element.column
        [] <|
        List.map
          (\(name, graph) ->
               Element.Input.button []
                 { onPress = Just <| GraphSelected (name, graph)
                 , label = Element.text name
                 }
          )
          options
    ]

viewOverlay : Model n e -> Element (Msg n e)
viewOverlay model =
 case model.menu of
   SelectFiles ->
       viewSelectFilesPopup model.files
   Main ->
       Element.column
         []
         [ Element.Input.button 
           []
           { onPress = Just (ViewMenu SelectFiles)
           , label = Element.text "Select Files..."
           }
         , case model.selectedGraphAndVisualiser of
             Just (selectedGraph, vis) ->
               Element.column []
                 [ Element.text <| "Current Graph: '" ++ selectedGraph ++ "'"
                 , Visualiser.viewOverlay vis
                       |> Element.map VisualiserMsg
                 ]
             Nothing ->
               Element.column [] []
         ]

viewSelectFilesPopup : List (File, Bool) -> Element (Msg n e)
viewSelectFilesPopup files =
  Element.column
    [] <|
    (List.map viewFileSelect files) ++ 
    [ Element.Input.button
      []
      { onPress = Just (ViewMenu Main)
      , label = Element.text "back"
      }
    ]

viewFileSelect : (File, Bool) -> Element (Msg n e)
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

toggleFileButton : (File, Bool) -> Element (Msg n e)
toggleFileButton (file, sel) =
  Element.Input.button
    []
    { onPress = Just <| SetFileSelected file (not sel)
    , label = Element.text <| if sel then " [exclude]" else " [include]"
    }

subscriptions : Model n e -> Sub (Msg n e)
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
