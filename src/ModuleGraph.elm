module ModuleGraph exposing (..)

import Dict exposing (..)
import Element exposing (Element, el, html)
import Graph exposing (..)
import Graph.DOT as DOT exposing (..)
import IntDict as IntDict
import ModuleInfo exposing (ModuleInfo)
import Set
import String.Extra exposing (leftOf)
import SvgParser exposing (SvgNode(..), nodeToSvg, parse, parseToNode)


type alias Config a =
    { a
        | projInfo : List ModuleInfo
        , projGraph : Maybe (Graph NodeLabel EdgeLabel)
        , currentGraph : Maybe (Graph NodeLabel EdgeLabel)
        , orientation : DOT.Rankdir
        , grouping : Bool
        , edgeColors : Bool
        , svgStr : String
        , width : Int
        , height : Int
    }



-------------------------------------------------------------------------------
---------------------
-- Graph functions --
---------------------


type alias NodeLabel =
    { group : String
    , attrs : Dict String String
    }


type alias EdgeLabel =
    { group : String
    , attrs : Dict String String
    }


makeGraph : List ModuleInfo -> Graph NodeLabel EdgeLabel
makeGraph mis =
    let
        nodeDict =
            getNodes mis

        edges =
            getEdges mis nodeDict
    in
    fromNodesAndEdges (Dict.values nodeDict) edges


ancestorGraph : NodeId -> Graph n e -> Graph n e
ancestorGraph id g =
    let
        ans =
            findAncestors id g
    in
    trimGraph ans g


descendantGraph : NodeId -> Graph n e -> Graph n e
descendantGraph id g =
    let
        des =
            findDescendants id g
    in
    trimGraph des g


trimGraph : List NodeId -> Graph n e -> Graph n e
trimGraph xs g =
    List.foldr
        (\n acc ->
            if not <| List.member n xs then
                Graph.remove n acc
            else
                acc
        )
        g
        (Graph.nodeIds g)


findDescendants : NodeId -> Graph n e -> List NodeId
findDescendants id g =
    findAncestors id (Graph.reverseEdges g)


findAncestors : NodeId -> Graph n e -> List NodeId
findAncestors id g =
    let
        go acc currents =
            case currents of
                [] ->
                    acc

                c :: cs ->
                    case Graph.get c g of
                        Just context ->
                            let
                                incomingsIds =
                                    IntDict.keys context.incoming
                            in
                            go (go (c :: acc) incomingsIds) cs

                        Nothing ->
                            acc
    in
    go [] [ id ]


trimToGroup : String -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
trimToGroup group g =
    groupDict g
        |> Dict.get group
        |> Maybe.withDefault []
        |> List.map .id
        |> (\ns -> trimGraph ns g)



-------------------------------------------------------------------------------
-----------
-- Nodes --
-----------


getNodes : List ModuleInfo -> Dict String (Node NodeLabel)
getNodes mis =
    List.indexedMap (\i mi -> Node i mi.modName) mis
        |> List.map (\n -> ( n.label, n ))
        |> Dict.fromList
        |> groupNodes


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


groupDict : Graph NodeLabel EdgeLabel -> Dict String (List (Node NodeLabel))
groupDict g =
    Graph.nodes g
        |> List.foldr
            (\n acc ->
                Dict.update
                    n.label.group
                    (\mbNodes ->
                        case mbNodes of
                            Just nodes ->
                                Just <| n :: nodes

                            Nothing ->
                                Just [ n ]
                    )
                    acc
            )
            Dict.empty



-------------------------------------------------------------------------------
-----------
-- Edges --
-----------


getEdges : List ModuleInfo -> Dict String (Node NodeLabel) -> List (Edge EdgeLabel)
getEdges mis nodes =
    List.foldr
        (\mi acc ->
            let
                mId =
                    Dict.get mi.modName nodes
                        |> Maybe.map .id
                        |> Maybe.withDefault -1

                currentEdges =
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
            in
            currentEdges ++ acc
        )
        []
        mis



-------------------------------------------------------------------------------
--------------------------
-- DOT String functions --
--------------------------


graphDOTStr : Config a -> String
graphDOTStr config =
    case config.currentGraph of
        Just graph ->
            DOT.outputWithStylesAndAttributes
                (graphStyle config)
                .attrs
                .attrs
                graph
                |> addSubgraphs config

        Nothing ->
            ""


graphStyle : Config a -> DOT.Styles
graphStyle config =
    { defaultStyles
        | rankdir = config.orientation
        , graph = "bgcolor = gray7"
        , node = "shape = box, colorscheme = set312"
        , edge = "colorscheme = set312"
    }


addSubgraphs : Config a -> String -> String
addSubgraphs config s =
    case config.currentGraph of
        Just graph ->
            let
                groupedNodes =
                    groupDict graph
                        |> Dict.values
                        |> List.map (List.map (String.fromInt << .id))

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

        Nothing ->
            ""


trimXml s =
    case String.indexes "<svg " s of
        [] ->
            s

        n :: xs ->
            String.dropLeft n s
