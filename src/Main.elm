port module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import File exposing (..)
import File.Select as Select
import Graph exposing (..)
import Graph.DOT as DOT exposing (..)
import Html as Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import IntDict as IntDict
import Json.Decode as D
import ModuleGraph exposing (..)
import ModuleInfo exposing (..)
import Murmur3 exposing (hashString)
import Palette exposing (..)
import Set
import String.Extra exposing (leftOf)
import StyleHelpers exposing (..)
import SvgParser exposing (SvgNode(..), nodeToSvg, parse, parseToNode)
import Task exposing (perform)


port toRender : String -> Cmd msg


port svgStrSub : (String -> msg) -> Sub msg


type alias Model =
    { projInfo : List ModuleInfo
    , projGraph : Maybe (Graph NodeLabel EdgeLabel)
    , currentGraph : Maybe (Graph NodeLabel EdgeLabel)
    , orientation : DOT.Rankdir
    , grouping : Bool
    , edgeColors : Bool
    , svgStr : String
    , width : Int
    , height : Int
    , currentGroup : Maybe String
    , currentNode : Maybe Int
    , currentFilter : Filter
    }


type Filter
    = NoFilter
    | Group
    | Ancestors
    | Descendants


type alias Flags =
    { width : Int
    , height : Int
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = RequestFile
    | FileLoaded File
    | ContentLoaded String
    | SetOrientation DOT.Rankdir
    | SetFilter Filter
    | PickGroup String
    | PickNode Int
    | SvgStr String
    | WinResize Int Int
    | NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { projInfo = []
      , projGraph = Nothing
      , currentGraph = Nothing
      , orientation = DOT.LR
      , grouping = True
      , edgeColors = True
      , svgStr = ""
      , width = flags.width
      , height = flags.height
      , currentGroup = Nothing
      , currentNode = Nothing
      , currentFilter = NoFilter
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize WinResize
        , svgStrSub SvgStr
        ]



-------------------------------------------------------------------------------
----------------------
-- Update functions --
----------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestFile ->
            ( model
            , Select.file [] FileLoaded
            )

        FileLoaded f ->
            ( model, Task.perform ContentLoaded (File.toString f) )

        ContentLoaded json ->
            case D.decodeString (D.list moduleInfoDec) json of
                Ok data ->
                    let
                        newGraph =
                            makeGraph data

                        newModel =
                            { model
                                | projInfo = data
                                , projGraph = Just newGraph
                                , currentGraph = Just newGraph
                                , currentGroup = Nothing
                                , currentNode = Nothing
                                , currentFilter = NoFilter
                            }
                    in
                    ( newModel
                    , toRender <| graphDOTStr newModel
                    )

                Err _ ->
                    ( model, Cmd.none )

        SetOrientation o ->
            let
                newModel =
                    { model | orientation = o }
            in
            ( newModel, toRender <| graphDOTStr newModel )

        SetFilter f ->
            let
                currentGraph =
                    generateCurrentGraph
                        model.projGraph
                        f
                        model.currentNode
                        model.currentGroup

                newModel =
                    { model
                        | currentFilter = f
                        , currentGraph = currentGraph
                    }
            in
            ( newModel, toRender <| graphDOTStr newModel )

        PickGroup group ->
            let
                newGroup =
                    if model.currentGroup == Just group then
                        Nothing
                    else
                        Just group

                currentGraph =
                    generateCurrentGraph
                        model.projGraph
                        model.currentFilter
                        model.currentNode
                        (Just group)

                newModel =
                    { model
                        | currentGroup = newGroup
                        , currentGraph = currentGraph
                    }
            in
            ( newModel
            , toRender <| graphDOTStr newModel
            )

        PickNode nodeId ->
            let
                newNode =
                    if model.currentNode == Just nodeId then
                        Nothing
                    else
                        Just nodeId

                currentGraph =
                    generateCurrentGraph
                        model.projGraph
                        model.currentFilter
                        (Just nodeId)
                        model.currentGroup

                newModel =
                    { model
                        | currentNode = newNode
                        , currentGraph = currentGraph
                    }
            in
            ( newModel
            , toRender <| graphDOTStr newModel
            )

        SvgStr s ->
            ( { model | svgStr = trimXml s }
            , Cmd.none
            )

        WinResize width height ->
            ( { model
                | width = width
                , height = height
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


generateCurrentGraph :
    Maybe (Graph NodeLabel EdgeLabel)
    -> Filter
    -> Maybe Int
    -> Maybe String
    -> Maybe (Graph NodeLabel EdgeLabel)
generateCurrentGraph projGraph filter mbNodeId mbGroup =
    case ( filter, mbNodeId, mbGroup ) of
        ( Ancestors, Just id, _ ) ->
            Maybe.map (ancestorGraph id) projGraph

        ( Descendants, Just id, _ ) ->
            Maybe.map (descendantGraph id) projGraph

        ( Group, _, Just group ) ->
            Maybe.map (trimToGroup group) projGraph

        _ ->
            projGraph



-------------------------------------------------------------------------------
--------------------
-- View functions --
--------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Project Info visualizer"
    , body =
        [ Element.layout
            [ width fill
            , Font.size 16
            ]
            (column
                [ width fill
                , padding 15
                , spacing 15
                ]
                [ row
                    [ spacing 15
                    , width fill
                    ]
                    [ Input.button
                        (buttonStyle True)
                        { onPress = Just RequestFile
                        , label = text "Load project info"
                        }
                    , row
                        [ spacing 15
                        , width fill
                        ]
                        [ orientationView model
                        , projSizeView model
                        ]
                    ]
                , modulePickerView model
                , svgElement model
                ]
            )
        ]
    }


orientationView : Model -> Element Msg
orientationView model =
    Input.radioRow
        [ spacing 10 ]
        { onChange = SetOrientation
        , options =
            [ Input.option DOT.TB (el [] (text "TB"))
            , Input.option DOT.LR (el [] (text "LR"))
            , Input.option DOT.BT (el [] (text "BT"))
            , Input.option DOT.RL (el [] (text "RL"))
            ]
        , selected = Just model.orientation
        , label =
            Input.labelLeft
                [ paddingEach { sides | right = 10 } ]
                (text "Orientation:")
        }


projSizeView : Model -> Element Msg
projSizeView model =
    let
        size =
            model.projInfo
                |> List.map .nbrLoc
                |> List.sum
    in
    el
        [ alignRight ]
        (text <|
            "Project size: "
                ++ String.fromInt size
                ++ " LOC"
        )


filterView : Model -> Element Msg
filterView model =
    el
        [ alignTop ]
        (Input.radio
            [ spacing 10 ]
            { onChange = SetFilter
            , options =
                [ Input.option NoFilter (el [] (text "None"))
                , Input.option Ancestors (el [] (text "Imports"))
                , Input.option Descendants (el [] (text "Clients"))
                , Input.option Group (el [] (text "Group"))
                ]
            , selected = Just model.currentFilter
            , label =
                Input.labelAbove
                    [ paddingEach { sides | bottom = 10 } ]
                    (text "Filter:")
            }
        )


modulePickerView : Model -> Element Msg
modulePickerView model =
    let
        groups =
            model.projGraph
                |> Maybe.withDefault Graph.empty
                |> groupDict

        nodes =
            case model.currentGroup of
                Just g ->
                    Dict.get g groups
                        |> Maybe.withDefault []

                Nothing ->
                    Dict.values groups
                        |> List.concat

        groupView group =
            el
                [ Events.onClick (PickGroup group)
                , width fill
                , if model.currentGroup == Just group then
                    Background.color lightBlue
                  else
                    noAttr
                , mouseOver
                    [ if model.currentGroup == Just group then
                        Background.color lightBlue
                      else
                        Background.color lightGrey
                    ]
                , pointer
                , paddingXY 10 7
                ]
                (text group)

        nodeView node =
            el
                [ Events.onClick (PickNode node.id)
                , width fill
                , if model.currentNode == Just node.id then
                    Background.color lightBlue
                  else
                    noAttr
                , mouseOver
                    [ if model.currentNode == Just node.id then
                        Background.color lightBlue
                      else
                        Background.color lightGrey
                    ]
                , pointer
                , paddingXY 10 7
                ]
                (text
                    (Dict.get "label" node.label.attrs
                        |> Maybe.withDefault ""
                    )
                )

        groupsView =
            column
                [ width (px 400)
                , height (px 300)
                , scrollbarY
                , Border.width 1
                , Border.color grey
                ]
                (List.map groupView (Dict.keys groups))

        nodesView =
            column
                [ width (px 400)
                , height (px 300)
                , scrollbarY
                , Border.width 1
                , Border.color grey
                ]
                (List.map nodeView nodes)
    in
    row
        [ spacing 15 ]
        [ groupsView
        , nodesView
        , filterView model
        ]


svgElement : Config a -> Element msg
svgElement config =
    case parseToNode config.svgStr of
        Ok (SvgElement element) ->
            let
                attributes =
                    Dict.fromList element.attributes

                width =
                    Dict.get "width" attributes
                        |> Maybe.map (String.dropRight 2)
                        |> Maybe.andThen String.toFloat
                        |> Maybe.withDefault 1

                height =
                    Dict.get "height" attributes
                        |> Maybe.map (String.dropRight 2)
                        |> Maybe.andThen String.toFloat
                        |> Maybe.withDefault 1

                ratio =
                    width / height

                maxWidth =
                    config.width - 30

                newAttr =
                    Dict.insert "width" (String.fromInt maxWidth ++ "px") attributes
                        |> Dict.insert "height" (String.fromInt (toFloat maxWidth / ratio |> round) ++ "px")
                        |> Dict.toList
            in
            nodeToSvg (SvgElement { element | attributes = newAttr })
                |> (\svg ->
                        el
                            []
                            (html svg)
                   )

        _ ->
            Element.none
