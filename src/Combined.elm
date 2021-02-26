module Combined exposing (..)

import Element exposing (Element)
import Element.Input
import Element.Font
import CustomElement as CElement
import Visualiser
import Graph exposing (Graph)
import VsColor
import Config exposing (Config)
import Instance exposing (Instance)
import Package.JavaToGraph
import Package.Visualiser
import Package.Graph
import Project.JavaToGraph
import Project.Visualiser
import Project.Graph
import File exposing (File)

type alias Model =
    { view : View
    , config : Config
    , files : List (File, Bool)
    }

type View
    = Project ProjectData
    | Package PackageData

type alias ProjectData =
  { vis : Visualiser.Model Project.Graph.Entity Project.Graph.Link
  }

type alias PackageData =
  { name : String
  , vis : Visualiser.Model Package.Graph.Entity Package.Graph.Link
  }

type Msg
    = PackageSelected String
    | BackToProject
    | SetFiles (List (File, Bool))
    | ProjectMsg (Visualiser.Msg Project.Graph.Entity)
    | PackageMsg (Visualiser.Msg Package.Graph.Entity)

filesToStrings : List (File, Bool) -> List String
filesToStrings files =
    files
    |> List.filterMap (\(f, b) -> if b then Just f else Nothing)
    |> List.map .content

init : Config -> (Model, Cmd (Msg))
init config =
    let
        graph = Project.JavaToGraph.toGraph []
        projectVis =
            Visualiser.init
                config
                graph 
                { viewNode = Project.Visualiser.viewNode
                , viewEdge = Project.Visualiser.viewEdge
                , onClick  = Project.Visualiser.onClick
                , getRect  = Project.Visualiser.getRect
                }
    in
        ( { view = Project { vis = projectVis }
          , config = config
          , files = []
          }
        , Cmd.none
        )

graphsMatching : String -> List (String, a) -> List (String, a)
graphsMatching name graphs =
    List.filter (\(gName, _) -> String.endsWith name gName) graphs 

update : Msg -> Model -> (Model, Cmd (Msg))
update msg model =
    case msg of
        PackageSelected name ->
            let
                graphs =
                    filesToStrings model.files
                    |> Package.JavaToGraph.fromSources
            in
                case graphsMatching name graphs of
                    (_, graph)::_ ->
                        ( { model
                            | view = Package
                                 { vis = 
                                    Visualiser.init
                                        model.config
                                        graph
                                        { viewNode = Package.Visualiser.viewNode
                                        , viewEdge = Package.Visualiser.viewEdge
                                        , onClick  = Package.Visualiser.onClick
                                        , getRect  = Package.Visualiser.getRect
                                        }
                                 , name = name
                                 }
                          }
                        , Cmd.none
                        )
                    [] ->
                        ( Debug.todo "todo"
                        , Cmd.none
                        )
        BackToProject ->
            ( { model | view =
                  let
                    graph =
                        filesToStrings model.files
                        |> Project.JavaToGraph.fromSources
                  in
                    Project <|
                      ProjectData <|
                        Visualiser.init
                            model.config
                            graph 
                            { viewNode = Project.Visualiser.viewNode
                            , viewEdge = Project.Visualiser.viewEdge
                            , onClick  = Project.Visualiser.onClick
                            , getRect  = Project.Visualiser.getRect
                            }
              }
            , Cmd.none
            )
        SetFiles files ->
            let
                newView =
                    case model.view of
                        Project data ->
                            let
                                graph =
                                    filesToStrings files
                                    |> Project.JavaToGraph.fromSources
                            in
                                Project <|
                                  ProjectData <|
                                    Visualiser.withGraph
                                        model.config
                                        graph
                                        data.vis
                        Package data ->
                            let
                                graphs =
                                    filesToStrings files
                                    |> Package.JavaToGraph.fromSources
                            in
                                case graphsMatching data.name graphs of
                                    (_, graph)::_ ->
                                        Package
                                          { data | vis =
                                              Visualiser.withGraph
                                                model.config
                                                graph
                                                data.vis
                                          }
                                    _ -> Debug.todo "todo"

            in
                ( { model
                    | files = files
                    , view = newView
                  }
                , Cmd.none
                )
        ProjectMsg vMsg ->
            case model.view of
                Project data -> 
                    let
                        (v2, msg2, click) =
                            Visualiser.update
                                model.config
                                vMsg
                                data.vis
                    in
                        case click of
                            Just n ->
                                update (PackageSelected n.data.data.name) model
                            Nothing ->
                                ( { model | view = Project { data | vis = v2 } }
                                , Cmd.map ProjectMsg msg2
                                )
                Package _ ->
                    ( model
                    , Cmd.none
                    )
        PackageMsg vMsg ->
            case model.view of
                Package data ->
                    let
                        (v2, msg2, click) =
                            Visualiser.update
                                model.config
                                vMsg
                                data.vis
                    in
                        case click of
                            Just node ->
                                ( { model
                                    | view =
                                      Package
                                        { data
                                          | vis = Visualiser.clickNode
                                                     model.config
                                                     node
                                                     v2
                                        }
                                  }
                                , Cmd.map PackageMsg msg2
                                )
                            Nothing ->
                                ( { model | view = Package { data | vis = v2 } }
                                , Cmd.map PackageMsg msg2
                                )
                Project v ->
                    ( model
                    , Cmd.none
                    )

view : Model -> Element (Msg)
view model =
    case model.view of
        Project { vis } ->
            vis
            |> Visualiser.view model.config
            |> Element.html
            |> Element.map ProjectMsg
        Package { vis } ->
            (Visualiser.view model.config vis)
            |> Element.html
            |> Element.map PackageMsg

viewOverlay : Model -> List (Element (Msg))
viewOverlay model =
    case model.view of
        Package { name, vis } ->
            [ CElement.button
              { onPress = Just BackToProject
              , label =
                  case name of
                    "" -> "← Unnamed Package"
                    _  -> "← Package: '" ++ name ++ "'"
              }
            ]
            ++
            (List.map (Element.map PackageMsg) (Visualiser.viewOverlay vis))
        Project { vis } ->
            List.map (Element.map ProjectMsg) (Visualiser.viewOverlay vis)

tick : Model -> Model
tick model =
    let
        newView =
            case model.view of
                Project data ->
                    Project { data | vis = Visualiser.tick data.vis }
                Package data ->
                    Package { data | vis = Visualiser.tick data.vis }
    in
        { model | view = newView }

