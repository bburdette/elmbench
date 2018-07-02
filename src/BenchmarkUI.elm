module BenchmarkUI exposing (..)

import Array.Hamt as Array
import Color
import Common exposing (buttonStyle, lightOrange, tagPill)
import Date exposing (Date)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import EveryDict exposing (EveryDict)
import EverySet exposing (EverySet)
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Time
import Uuid exposing (Uuid)


type Benchmark data
    = Benchmark
        { data : data
        , ftn : data -> String
        , name : String
        }


type Model bdata
    = Model
        { benchmark : Benchmark bdata
        , startTime : Maybe Time.Time
        , stopTime : Maybe Time.Time
        }


type Msg
    = StartBench Time.Time
    | StopBench Time.Time
    | Start
    | Noop


type Command
    = Now (Time.Time -> Msg)
    | None


initialModel : Benchmark bdata -> Model bdata
initialModel benchmark =
    Model
        { benchmark = benchmark
        , startTime = Nothing
        , stopTime = Nothing
        }


view : Model bdata -> Element Msg
view (Model model) =
    case model.benchmark of
        Benchmark benchmark ->
            column []
                [ el [ Font.bold ] (text benchmark.name)
                , el [ Font.bold ]
                    (text (toString model.startTime))
                , el
                    [ Font.bold ]
                    (text (toString model.stopTime))
                , case ( model.startTime, model.stopTime ) of
                    ( Just srt, Just spt ) ->
                        row []
                            [ text "milliseconds: "
                            , text <| toString <| Time.inMilliseconds (spt - srt)
                            ]

                    _ ->
                        el [] <| text ""
                , Input.button []
                    { onPress = Just Start
                    , label = text "start bench"
                    }
                ]


update :
    Msg
    -> Model bdata
    -> ( Model bdata, Command )
update msg (Model model) =
    case model.benchmark of
        Benchmark benchmark ->
            case msg of
                Start ->
                    ( Model model, Now StartBench )

                StartBench time ->
                    let
                        result =
                            benchmark.ftn benchmark.data

                        -- List.map (\idx -> Dict.get idx model.dictData) model.dictKeys
                    in
                    ( Model
                        { model
                            | startTime = Just time
                        }
                    , Now StopBench
                    )

                StopBench time ->
                    ( Model
                        { model
                            | stopTime = Just time
                        }
                    , None
                    )

                Noop ->
                    ( Model model, None )
