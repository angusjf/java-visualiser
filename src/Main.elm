port module Main exposing (main)

-- IMPORTS

import Browser
import Browser.Events
import Config exposing (Config)
import CustomElement as CElement
import Element exposing (Element)
import Element.Font
import Element.Input
import File exposing (File, Uri)
import Graph exposing (Graph)
import Instance exposing (Instance)
import Package.Graph
import Package.JavaToGraph
import Package.Visualiser
import Project.Graph
import Project.JavaToGraph
import Project.Visualiser
import Visualiser
import VsColor



-- TYPES


type alias Model =
    { config : Config
    , selectFiles : Bool
    , mode : Mode
    , files : List FileData
    }


type Mode
    = Project ProjectData
    | Package PackageData
    | LoadingProject
    | LoadingPackage String
    | Error


type alias ProjectData =
    { vis : Visualiser.Model Project.Graph.Entity Project.Graph.Link
    }


type alias PackageData =
    { name : String
    , vis : Visualiser.Model Package.Graph.Entity Package.Graph.Link
    }


type alias FileData =
    { file : File
    , selected : Bool
    , cachedPackageData : Maybe (List Package.JavaToGraph.Subgraph)
    , cachedProjectData : Maybe Project.JavaToGraph.PartialData
    }


type Msg
    = UpdateFile File
    | DeleteFile Uri
    | RenameFile ( Uri, Uri )
    | ConfigChanged Config
    | Tick Float
    | SelectFiles Bool
    | SetFileSelected FileData Bool
    | ProjectMsg (Visualiser.Msg Project.Graph.Entity)
    | PackageMsg (Visualiser.Msg Package.Graph.Entity)
    | PackageSelected String
    | BackToProject



-- PORTS


port updateFile : (File -> msg) -> Sub msg


port deleteFile : (Uri -> msg) -> Sub msg


port renameFile : (( Uri, Uri ) -> msg) -> Sub msg


port configChanged : (Config -> msg) -> Sub msg


port infoMessage : String -> Cmd msg



-- INIT


init : Config -> ( Model, Cmd Msg )
init config =
    let
        graph =
            Project.JavaToGraph.toGraph []

        projectVis =
            Visualiser.init
                config
                graph
                { viewNode = Project.Visualiser.viewNode
                , viewEdge = Project.Visualiser.viewEdge
                , onClick = Project.Visualiser.onClick
                , getRect = Project.Visualiser.getRect
                }
    in
    ( { config = config
      , selectFiles = False
      , mode = Project { vis = projectVis }
      , files = []
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFile file ->
            ( resetMode { model | files = insert file model.files }
            , Cmd.none
            )

        DeleteFile uri ->
            ( resetMode { model | files = delete uri model.files }
            , Cmd.none
            )

        RenameFile ( from, to ) ->
            ( resetMode { model | files = rename from to model.files }
            , Cmd.none
            )

        SetFileSelected file selected ->
            ( resetMode
                { model
                    | files =
                        model.files
                            |> List.map
                                (\f ->
                                    if f.file.uri == file.file.uri then
                                        { file | selected = selected }

                                    else
                                        f
                                )
                }
            , Cmd.none
            )

        ConfigChanged cfg ->
            ( resetMode { model | config = cfg }
            , Cmd.none
            )

        Tick _ ->
            ( tick model
            , Cmd.none
            )

        SelectFiles b ->
            ( { model | selectFiles = b }
            , Cmd.none
            )

        PackageSelected name ->
            ( resetMode { model | mode = LoadingPackage name }
            , Cmd.none
            )

        BackToProject ->
            ( resetMode { model | mode = LoadingProject }
            , Cmd.none
            )

        ProjectMsg vMsg ->
            case model.mode of
                Project data ->
                    updateProjectVisualiser vMsg data model

                _ ->
                    ( model, Cmd.none )

        PackageMsg vMsg ->
            case model.mode of
                Package data ->
                    updatePackageVisualiser vMsg data model

                _ ->
                    ( model, Cmd.none )


resetMode : Model -> Model
resetMode model =
    { model
        | mode =
            case model.mode of
                LoadingProject ->
                    let
                        graph =
                            model.files
                                |> List.filterMap .cachedProjectData
                                |> Project.JavaToGraph.toGraph
                    in
                    Project <|
                        ProjectData <|
                            Visualiser.init
                                model.config
                                graph
                                { viewNode = Project.Visualiser.viewNode
                                , viewEdge = Project.Visualiser.viewEdge
                                , onClick = Project.Visualiser.onClick
                                , getRect = Project.Visualiser.getRect
                                }

                LoadingPackage name ->
                    let
                        graphs =
                            model.files
                                |> List.filterMap .cachedPackageData
                                |> List.concat
                                |> Package.JavaToGraph.toGraphs
                    in
                    case graphsMatching name graphs of
                        ( _, graph ) :: _ ->
                            Package
                                { vis =
                                    Visualiser.init
                                        model.config
                                        graph
                                        { viewNode = Package.Visualiser.viewNode
                                        , viewEdge = Package.Visualiser.viewEdge
                                        , onClick = Package.Visualiser.onClick
                                        , getRect = Package.Visualiser.getRect
                                        }
                                , name = name
                                }

                        [] ->
                            Error
    
                Project { vis } ->
                    let
                        graph =
                            model.files
                                |> List.filterMap .cachedProjectData
                                |> Project.JavaToGraph.toGraph
                    in
                    Project <|
                        ProjectData <|
                            Visualiser.withGraph model.config graph vis

                Package { name, vis } ->
                    let
                        graphs =
                            model.files
                                |> List.filterMap .cachedPackageData
                                |> List.concat
                                |> Package.JavaToGraph.toGraphs
                    in
                    case graphsMatching name graphs of
                        ( _, graph ) :: _ ->
                            Package
                                { vis = Visualiser.withGraph model.config graph vis
                                , name = name
                                }

                        [] ->
                            Error

                Error ->
                    Error
    }


updateProjectVisualiser msg data model =
    let
        ( v2, msg2, click ) =
            Visualiser.update
                model.config
                msg
                data.vis
    in
    case click of
        Just n ->
            update (PackageSelected n.data.data.name) model

        Nothing ->
            ( { model | mode = Project { data | vis = v2 } }
            , Cmd.map ProjectMsg msg2
            )


updatePackageVisualiser msg data model =
    let
        ( v2, msg2, click ) =
            Visualiser.update model.config msg data.vis
    in
    case click of
        Just node ->
            ( { model
                | mode = Package { data | vis = Visualiser.clickNode model.config node v2 }
              }
            , Cmd.map PackageMsg msg2
            )

        Nothing ->
            ( { model | mode = Package { data | vis = v2 } }
            , Cmd.map PackageMsg msg2
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Visualiser"
    , body =
        [ Element.layout
            [ VsColor.fontColor VsColor.Foreground
            , Element.Font.size 13
            , Element.padding 4
            ]
          <|
            CElement.column
                [ viewVis model
                , viewOverlay model
                ]
        ]
    }


viewVis : Model -> Element Msg
viewVis { mode, config } =
    case mode of
        Project { vis } ->
            vis
                |> Visualiser.view config
                |> Element.html
                |> Element.map ProjectMsg

        Package { vis } ->
            vis
                |> Visualiser.view config
                |> Element.html
                |> Element.map PackageMsg

        _ ->
            Element.column [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateFile UpdateFile
        , deleteFile DeleteFile
        , renameFile RenameFile
        , configChanged ConfigChanged
        , Browser.Events.onAnimationFrameDelta Tick
        ]



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- OVERLAY


viewOverlay : Model -> Element Msg
viewOverlay model =
    if model.selectFiles then
        viewSelectFilesPopup model.files

    else
        let
            title =
                CElement.text <|
                    case model.mode of
                        Package { name } -> if name == "" 
                                                then "Unnamed Package"
                                                else "'" ++ name ++ "'"
                        Project _ -> "Click a Package..."
                        LoadingProject -> ""
                        LoadingPackage _ -> ""
                        Error -> "Error!"

            selectFilesButton =
                CElement.button
                    { onPress = Just (SelectFiles True)
                    , label = "Select Files..."
                    }

            rest =
                case model.mode of
                    Package { name, vis } ->
                        [ CElement.button
                            { onPress = Just BackToProject
                            , label = "← Unnamed Package"
                            }
                        ]
                            ++ List.map (Element.map PackageMsg) (Visualiser.viewOverlay vis)

                    Project { vis } ->
                        List.map (Element.map ProjectMsg) (Visualiser.viewOverlay vis)

                    LoadingProject ->
                        [ CElement.text "loading project view..." ]

                    LoadingPackage n ->
                        [ CElement.text <| "loading package '" ++ n ++ "'..." ]

                    Error ->
                        [ CElement.button
                            { onPress = Just BackToProject
                            , label = "← Back To Project"
                            }
                        ]
        in
        CElement.column <|
            [ title
            , selectFilesButton
            ]
            ++ rest



-- RUBBISH


viewSelectFilesPopup : List FileData -> Element Msg
viewSelectFilesPopup files =
    CElement.column <|
        CElement.button
            { onPress = Just (SelectFiles False)
            , label = "← Back"
            }
            :: List.map viewFileSelect files


viewFileSelect : FileData -> Element Msg
viewFileSelect data =
    Element.row
        [ Element.spacing 8 ]
        [ CElement.text <| trimUntilRev (\c -> c == '/') data.file.uri
        , toggleFileButton data
        ]


trimUntilRev fn str =
    let
        prefix =
            String.reverse <| trimUntil fn <| String.reverse str
    in
    String.replace prefix "" str


trimUntil : (Char -> Bool) -> String -> String
trimUntil f =
    let
        helper : List Char -> List Char
        helper chars =
            case chars of
                x :: xs ->
                    if f x then
                        xs

                    else
                        helper xs

                [] ->
                    []
    in
    String.toList >> helper >> String.fromList


toggleFileButton : FileData -> Element Msg
toggleFileButton d =
    CElement.button
        { onPress = Just <| SetFileSelected d (not d.selected)
        , label =
            if d.selected then
                "✓"

            else
                "✕"
        }



--------------


insert : File -> List FileData -> List FileData
insert file files =
    case files of
        data :: xs ->
            if data.file.uri == file.uri then
                { file = file
                , selected = True
                , cachedPackageData = Package.JavaToGraph.fromSource file.content
                , cachedProjectData = Project.JavaToGraph.fromSource file.content
                }
                    :: xs

            else
                data :: insert file xs

        [] ->
            [ { file = file
              , selected = True
              , cachedPackageData = Package.JavaToGraph.fromSource file.content
              , cachedProjectData = Project.JavaToGraph.fromSource file.content
              }
            ]


delete : Uri -> List FileData -> List FileData
delete uri files =
    case files of
        data :: xs ->
            if data.file.uri == uri then
                xs

            else
                data :: delete uri xs

        [] ->
            []


rename : Uri -> Uri -> List FileData -> List FileData
rename from to files =
    case files of
        data :: xs ->
            if data.file.uri == from then
                let
                    f =
                        data.file
                in
                { data | file = { f | uri = to } } :: xs

            else
                data :: rename from to xs

        [] ->
            []



----------------


graphsMatching : String -> List ( String, a ) -> List ( String, a )
graphsMatching name graphs =
    List.filter (\( gName, _ ) -> name == gName) graphs


tick : Model -> Model
tick model =
    let
        newView =
            case model.mode of
                Project data ->
                    Project { data | vis = Visualiser.tick data.vis }

                Package data ->
                    Package { data | vis = Visualiser.tick data.vis }

                LoadingProject ->
                    LoadingProject

                LoadingPackage n ->
                    LoadingPackage n

                Error ->
                    Error
    in
    { model | mode = newView }


filesToStrings : List FileData -> List String
filesToStrings files =
    files
        |> List.filterMap
            (\f ->
                if f.selected then
                    Just f

                else
                    Nothing
            )
        |> List.map (\d -> d.file.content)
