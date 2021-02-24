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
    = ProjectView (Visualiser.Model Project.Graph.Entity Project.Graph.Link)
    | PackageView String (Visualiser.Model Package.Graph.Entity Package.Graph.Link)

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
        graph = Project.JavaToGraph.fromSources (filesToStrings [])
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
        ( { view = ProjectView projectVis
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
                            | view = PackageView name <|
                                Visualiser.init
                                    model.config
                                    graph
                                    { viewNode = Package.Visualiser.viewNode
                                    , viewEdge = Package.Visualiser.viewEdge
                                    , onClick  = Package.Visualiser.onClick
                                    , getRect  = Package.Visualiser.getRect
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
                    ProjectView <|
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
                        ProjectView v ->
                            let
                                graph =
                                    filesToStrings files
                                    |> Project.JavaToGraph.fromSources
                            in
                                ProjectView <|
                                    Visualiser.withGraph
                                        model.config
                                        graph
                                        v
                        PackageView name v ->
                            let
                                graphs =
                                    filesToStrings files
                                    |> Package.JavaToGraph.fromSources
                            in
                                case graphsMatching name graphs of
                                    (_, graph)::_ ->
                                        PackageView name <|
                                            Visualiser.withGraph
                                                model.config
                                                graph
                                                v
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
                ProjectView v -> 
                    let
                        (v2, msg2, click) =
                            Visualiser.update
                                model.config
                                vMsg
                                v
                    in
                        case click of
                            Just n ->
                                update (PackageSelected n.data.data.name) model
                            Nothing ->
                                ( { model | view = ProjectView v2 }
                                , Cmd.map ProjectMsg msg2
                                )
                PackageView _ _ ->
                    ( model
                    , Cmd.none
                    )
        PackageMsg vMsg ->
            case model.view of
                PackageView n v ->
                    let
                        (v2, msg2, click) =
                            Visualiser.update
                                model.config
                                vMsg
                                v
                    in
                        case click of
                            Just node ->
                                ( { model
                                    | view =
                                      Visualiser.clickNode model.config node v2
                                      |> PackageView n
                                  }
                                , Cmd.map PackageMsg msg2
                                )
                            Nothing ->
                                ( { model | view = PackageView n v2 }
                                , Cmd.map PackageMsg msg2
                                )
                ProjectView v ->
                    ( model
                    , Cmd.none
                    )

view : Model -> Element (Msg)
view model =
    case model.view of
        ProjectView v ->
            (Visualiser.view model.config v)
            |> Element.html
            |> Element.map ProjectMsg
        PackageView _ v ->
            (Visualiser.view model.config v)
            |> Element.html
            |> Element.map PackageMsg

viewOverlay : Model -> List (Element (Msg))
viewOverlay model =
    case model.view of
        PackageView name v ->
            [ CElement.button
              { onPress = Just BackToProject
              , label =
                  case name of
                    "" -> "← Unnamed Package"
                    _  -> "← Package: '" ++ name ++ "'"
              }
            ]
            ++
            (List.map (Element.map PackageMsg) (Visualiser.viewOverlay v))
        ProjectView v ->
            List.map (Element.map ProjectMsg) (Visualiser.viewOverlay v)

tick : Model -> Model
tick model =
    let
        newView =
            case model.view of
                ProjectView v ->
                    ProjectView (Visualiser.tick v)
                PackageView n v ->
                    PackageView n (Visualiser.tick v)
    in
        { model | view = newView }

