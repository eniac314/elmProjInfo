module ModuleInfo exposing (..)

import Json.Decode as D


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
