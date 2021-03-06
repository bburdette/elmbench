module Benchmarks exposing (..)

import Array.Hamt as Array
import BenchmarkUI
import Color
import Common exposing (buttonStyle, lightOrange, tagPill)
import Date exposing (Date)
import Dict
import Element exposing (Element)
import EveryDict exposing (EveryDict)
import EverySet exposing (EverySet)
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Random.Pcg exposing (Seed, initialSeed, step)
import TDict
import Time
import Util
import Uuid exposing (Uuid)


type alias DData =
    { dict : Dict.Dict Int String, keys : List Int }


dictBench : DData -> String
dictBench data =
    let
        result =
            List.map (\idx -> Dict.get idx data.dict) data.keys
    in
    "blah"


makeDictBenchmark : Int -> BenchmarkUI.Benchmark DData
makeDictBenchmark count =
    let
        dd =
            Dict.fromList (List.indexedMap (,) (List.repeat count "blah"))
    in
    BenchmarkUI.Benchmark
        { data = { dict = dd, keys = Dict.keys dd }
        , ftn = dictBench
        , name = "Dict Int String Benchmark"
        }


type alias EDData =
    { everyDict : EveryDict.EveryDict Uuid.Uuid String, keys : List Uuid.Uuid }


everyDictBench : EDData -> String
everyDictBench data =
    let
        result =
            List.map (\idx -> EveryDict.get idx data.everyDict) data.keys
    in
    "blah"


makeEveryDictBenchmark : List Uuid.Uuid -> BenchmarkUI.Benchmark EDData
makeEveryDictBenchmark uuids =
    let
        d =
            EveryDict.fromList (List.map (\id -> ( id, "blah" )) uuids)
    in
    BenchmarkUI.Benchmark
        { data = { everyDict = d, keys = uuids }
        , ftn = everyDictBench
        , name = "EveryDict Uuid String Benchmark"
        }


type TDId
    = TDId Int


ktc : TDId -> Int
ktc (TDId id) =
    id


type alias TDData =
    { tdict : TDict.TDict TDId Int String, keys : List TDId }


tDictBench : TDData -> String
tDictBench data =
    let
        result =
            List.map (\idx -> TDict.get idx data.tdict) data.keys
    in
    "blah"


makeTDictBenchmark : Int -> BenchmarkUI.Benchmark TDData
makeTDictBenchmark count =
    let
        dl =
            List.indexedMap (\i v -> ( TDId i, v )) (List.repeat count "blah")

        te : TDict.TDict TDId Int String
        te =
            TDict.empty ktc

        td : TDict.TDict TDId Int String
        td =
            TDict.insertList dl te
    in
    BenchmarkUI.Benchmark
        { data = { tdict = td, keys = List.map Tuple.first dl }
        , ftn = tDictBench
        , name = "TDict TDId Int String Benchmark"
        }



------------------------------------------------------------


type Benchmodel
    = DModel (BenchmarkUI.Model DData)
    | EDModel (BenchmarkUI.Model EDData)
    | TModel (BenchmarkUI.Model TDData)


elementCount =
    125000


initialBenchmarkStates =
    [ DModel (BenchmarkUI.initialModel (makeDictBenchmark elementCount))
    , EDModel (BenchmarkUI.initialModel (makeEveryDictBenchmark (Tuple.second (Util.uuidList (initialSeed 0) elementCount))))
    , TModel (BenchmarkUI.initialModel (makeTDictBenchmark elementCount))
    ]


type BMsg
    = BMsg Int BenchmarkUI.Msg


type BCmd
    = BCmd Int BenchmarkUI.Command


view : List Benchmodel -> Element BMsg
view bms =
    Element.column [] <|
        List.indexedMap
            (\idx bmodel ->
                case bmodel of
                    DModel dm ->
                        Element.map (BMsg idx) <| BenchmarkUI.view dm

                    EDModel edm ->
                        Element.map (BMsg idx) <| BenchmarkUI.view edm

                    TModel edm ->
                        Element.map (BMsg idx) <| BenchmarkUI.view edm
            )
            bms


update : BMsg -> List Benchmodel -> ( List Benchmodel, BCmd )
update (BMsg idx uimsg) models =
    case List.head (List.drop idx models) of
        Just model ->
            let
                ( numod, cmd ) =
                    case model of
                        DModel mod ->
                            let
                                ( nmod, cmd ) =
                                    BenchmarkUI.update uimsg mod
                            in
                            ( DModel nmod, BCmd idx cmd )

                        EDModel mod ->
                            let
                                ( nmod, cmd ) =
                                    BenchmarkUI.update uimsg mod
                            in
                            ( EDModel nmod, BCmd idx cmd )

                        TModel mod ->
                            let
                                ( nmod, cmd ) =
                                    BenchmarkUI.update uimsg mod
                            in
                            ( TModel nmod, BCmd idx cmd )
            in
            ( List.concat
                [ List.take idx models
                , numod
                    :: List.drop (idx + 1) models
                ]
            , cmd
            )

        Nothing ->
            ( models, BCmd 0 BenchmarkUI.None )
