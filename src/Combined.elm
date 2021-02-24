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
    | PackageView (Visualiser.Model Package.Graph.Entity Package.Graph.Link)

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

update : Msg -> Model -> (Model, Cmd (Msg))
update msg model =
    case msg of
        PackageSelected name ->
            let
                graphs =
                    filesToStrings model.files
                    |> Package.JavaToGraph.fromSources
            in
                case graphs of
                    (_, graph)::_ ->
                        ( { model
                            | view = PackageView <|
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
                        ( Debug.todo "AGGGGGG"
                        , Cmd.none
                        )
        BackToProject ->
            ( model
            , Cmd.none
            )
        SetFiles files ->
            let
                newView =
                    case model.view of
                        ProjectView v ->
                            let
                                graph =
                                    filesToStrings model.files
                                    |> Project.JavaToGraph.fromSources
                            in
                                ProjectView <|
                                    Visualiser.withGraph
                                        model.config
                                        graph
                                        v
                        PackageView v ->
                            Debug.todo "todo"
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
                            Just entity ->
                                update (PackageSelected entity.name) model
                            Nothing ->
                                ( { model | view = ProjectView v2 }
                                , Cmd.map ProjectMsg msg2
                                )
                PackageView v ->
                    ( model
                    , Cmd.none
                    )
        PackageMsg vMsg ->
            case model.view of
                PackageView v ->
                    let
                        (v2, msg2, click) =
                                Visualiser.update
                                    model.config
                                    vMsg
                                    v
                    in
                        case click of
                            Just entity ->
                                    {-
                                let
                                  newNodes = upsert (click model.instance entity) model.nodes 
                                  s = getInitialSimulation
                                          config
                                          model.static
                                          model.instance
                                          newNodes
                                          model.edges
                                in
                                          -}
                                ( { model | view = PackageView v2 }
                                , Cmd.map PackageMsg msg2
                                )
                            Nothing ->
                                ( { model | view = PackageView v2 }
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
        PackageView v ->
            (Visualiser.view model.config v)
            |> Element.html
            |> Element.map PackageMsg

tick : Model -> Model
tick model =
    let
        newView =
            case model.view of
                ProjectView v ->
                    ProjectView (Visualiser.tick v)
                PackageView v ->
                    PackageView (Visualiser.tick v)
    in
        { model | view = newView }
        

