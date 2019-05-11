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
import Graph.TGF as TGF exposing (..)
import Html as Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import Murmur3 exposing (hashString)
import Set
import String.Extra exposing (leftOf)
import SvgParser exposing (SvgNode(..), nodeToSvg, parse, parseToNode)
import Task exposing (perform)


port toRender : String -> Cmd msg


port svgStrSub : (String -> msg) -> Sub msg


type alias Model =
    { projInfo : List ModuleInfo
    , svgStr : String
    , width : Int
    , height : Int
    }


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
    | RenderGraph
    | SvgStr String
    | WinResize Int Int
    | NoOp


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { projInfo = []
      , svgStr = ""
      , width = flags.width
      , height = flags.height
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize WinResize
        , svgStrSub SvgStr
        ]


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
                        newModel =
                            { model | projInfo = data }
                    in
                    ( newModel
                    , toRender <| graphDOTStr newModel
                    )

                Err _ ->
                    ( model, Cmd.none )

        RenderGraph ->
            ( model, toRender <| graphDOTStr model )

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


view : Model -> Browser.Document Msg
view model =
    { title = "Project Info visualizer"
    , body =
        [ Element.layout
            [ width fill
            , Font.size 16
            ]
            (column
                [ width (px 1200) ]
                [ Input.button
                    []
                    { onPress = Just RequestFile
                    , label = text "Load projInfo"
                    }
                , svgElement model
                ]
            )
        ]
    }



-------------------------------------------------------------------------------


type alias ModuleInfo =
    { nbrLoc : Int
    , imports : List Import
    , exports : Export
    , modName : String
    }


type alias Import =
    { impName : String
    , impTypes : List String
    , impFun : List String
    }


type alias Export =
    { expTypes : List String
    , expFun : List String
    }


moduleInfoDec =
    D.map4 ModuleInfo
        (D.field "nbrLoc" D.int)
        (D.field "imports" (D.list importDec))
        (D.field "exports" exportDec)
        (D.field "modName" D.string)


importDec : D.Decoder Import
importDec =
    D.map3 Import
        (D.field "impName" D.string)
        (D.field "impTypes" (D.list D.string))
        (D.field "impFun" (D.list D.string))


exportDec : D.Decoder Export
exportDec =
    D.map2 Export
        (D.field "expTypes" (D.list D.string))
        (D.field "expFun" (D.list D.string))



-------------------------------------------------------------------------------


makeGraph : List ModuleInfo -> Graph NodeLabel EdgeLabel
makeGraph mis =
    let
        nodeDict =
            getNodes mis

        edges =
            getEdges mis nodeDict
    in
    fromNodesAndEdges (Dict.values nodeDict) edges


getNodes : List ModuleInfo -> Dict String (Node NodeLabel)
getNodes mis =
    List.indexedMap (\i mi -> Node i mi.modName) mis
        |> List.map (\n -> ( n.label, n ))
        |> Dict.fromList
        |> groupNodes


getEdges : List ModuleInfo -> Dict String (Node NodeLabel) -> List (Edge EdgeLabel)
getEdges mis nodes =
    List.foldr
        (\mi acc ->
            let
                currentEdges =
                    case Maybe.map .id (Dict.get mi.modName nodes) of
                        Just mId ->
                            List.map
                                (\i ->
                                    case Dict.get i.impName nodes of
                                        Just node ->
                                            Just <|
                                                Edge node.id
                                                    mId
                                                    (EdgeLabel node.label.group
                                                        (Dict.fromList
                                                            [ ( "label", "" )
                                                            , ( "color"
                                                              , Dict.get "fillcolor" node.label.attrs
                                                                    |> Maybe.withDefault ""
                                                              )
                                                            ]
                                                        )
                                                    )

                                        Nothing ->
                                            Nothing
                                )
                                mi.imports
                                |> List.filterMap identity

                        Nothing ->
                            []
            in
            currentEdges ++ acc
        )
        []
        mis


type alias NodeLabel =
    { group : String
    , attrs : Dict String String
    }


type alias EdgeLabel =
    { group : String
    , attrs : Dict String String
    }


groupNodes : Dict String (Node String) -> Dict String (Node NodeLabel)
groupNodes nodeDict =
    let
        getGroup s =
            if leftOf "." s == "" then
                s
            else
                leftOf "." s

        groups =
            Dict.foldr
                (\k _ acc ->
                    Set.insert (getGroup k) acc
                )
                Set.empty
                nodeDict
                |> Set.toList

        shapes =
            [ "box"
            , "octagon"
            , "cylinder"
            ]

        groupColors =
            List.indexedMap
                (\i g ->
                    ( g
                    , 1 + modBy 12 i |> String.fromInt
                    )
                )
                groups
                |> Dict.fromList
    in
    Dict.map
        (\k node ->
            let
                g =
                    getGroup k

                col =
                    Dict.get g groupColors
                        |> Maybe.withDefault "1"
            in
            { id = node.id
            , label =
                NodeLabel
                    g
                    (Dict.fromList
                        [ ( "label", k )
                        , ( "style", "filled" )
                        , ( "fillcolor", col )
                        ]
                    )
            }
        )
        nodeDict


graphStyle model =
    { defaultStyles
        | rankdir = DOT.LR
        , node = "shape = box, colorscheme = set312"
        , edge = "colorscheme = set312"
    }


graphDOTStr model =
    DOT.outputWithStylesAndAttributes
        (graphStyle model)
        .attrs
        .attrs
        (makeGraph model.projInfo)
        |> addSubgraphs model


svgElement : Model -> Element msg
svgElement model =
    case parseToNode model.svgStr of
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

                newAttr =
                    Dict.insert "width" (String.fromInt model.width ++ "px") attributes
                        |> Dict.insert "height" (String.fromInt (toFloat model.width / ratio |> round) ++ "px")
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


trimXml s =
    case String.indexes "<svg " s of
        [] ->
            s

        n :: xs ->
            String.dropLeft n s


addSubgraphs : Model -> String -> String
addSubgraphs model s =
    let
        groupedNodes =
            getNodes model.projInfo
                |> Dict.foldr
                    (\k n acc ->
                        Dict.update
                            n.label.group
                            (\mbNodes ->
                                case mbNodes of
                                    Just nodes ->
                                        Just <| String.fromInt n.id :: nodes

                                    Nothing ->
                                        Just [ String.fromInt n.id ]
                            )
                            acc
                    )
                    Dict.empty
                |> Dict.values

        makeSubGraph n xs_ =
            "  subgraph cluster_"
                ++ String.fromInt n
                ++ " {\n"
                ++ "  style = invis;\n"
                ++ "  rank = same"
                ++ String.fromInt n
                ++ "; "
                ++ (List.map (\node -> node ++ "; ") xs_
                        |> String.join ""
                   )
                ++ "\n  }\n\n"
    in
    "\n"
        ++ String.dropRight 2 s
        ++ "\n\n"
        ++ (Tuple.first <|
                List.foldr (\gs ( res, n ) -> ( makeSubGraph n gs ++ res, n + 1 ))
                    ( "", 0 )
                    groupedNodes
           )
        ++ "\n}\n"
