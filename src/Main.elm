module Main exposing (..)

import BenchmarkUI
import Benchmarks
import Color
import Date exposing (Date)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import EverySet exposing (EverySet)
import Html exposing (programWithFlags)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Random.Pcg exposing (Seed, initialSeed, step)
import Task exposing (perform)
import Time
import Util exposing (httpErrorString)
import Uuid


type Msg
    = GetTime (Time.Time -> Msg) Time.Time
    | BenchmarkMsg Benchmarks.BMsg


type alias Model =
    { benchmarkStates : List Benchmarks.Benchmodel
    }


defaultmodel : Model
defaultmodel =
    { benchmarkStates = Benchmarks.initialBenchmarkStates
    }


init : ( Model, Cmd Msg )
init =
    ( defaultmodel
    , Cmd.none
    )


view : Model -> Element Msg
view m =
    Element.map BenchmarkMsg <| Benchmarks.view m.benchmarkStates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTime ftn time ->
            update (ftn time) model

        BenchmarkMsg bm ->
            updateBenchmarkState bm model


updateBenchmarkState : Benchmarks.BMsg -> Model -> ( Model, Cmd Msg )
updateBenchmarkState bm model =
    let
        ( bmodel, bcmd ) =
            Benchmarks.update bm model.benchmarkStates
    in
    case bcmd of
        Benchmarks.BCmd idx msg ->
            case msg of
                BenchmarkUI.Now ftn ->
                    ( { model | benchmarkStates = bmodel }
                    , Task.perform
                        (\time -> GetTime (\time -> BenchmarkMsg (Benchmarks.BMsg idx (ftn time))) time)
                        Time.now
                    )

                BenchmarkUI.None ->
                    ( { model | benchmarkStates = bmodel }, Cmd.none )


main =
    Html.program
        { init = init
        , view =
            \model ->
                Element.layout [] (view model)
        , update =
            update
        , subscriptions = \_ -> Sub.none

        --        , subscriptions = \_ -> Time.every (5 * Time.second) Tick
        }
