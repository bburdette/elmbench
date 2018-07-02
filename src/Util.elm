module Util
    exposing
        ( captchaQ
        , first
        , httpErrorString
        , monthInt
        , rest
        , uuidList
        )

import Color
import Date
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Random.Pcg exposing (Seed, initialSeed, int, step)
import Uuid exposing (Uuid)


httpErrorString : Http.Error -> String
httpErrorString e =
    case e of
        Http.BadUrl str ->
            "badurl" ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "networkerror"

        Http.BadStatus x ->
            "badstatus" ++ x.body

        Http.BadPayload s r ->
            "badpayload\nstring: " ++ s ++ "\nresponse body: " ++ r.body


rest : List a -> List a
rest list =
    case List.tail list of
        Nothing ->
            []

        Just elts ->
            elts


first : (a -> Maybe b) -> List a -> Maybe b
first f l =
    case List.head l of
        Just e ->
            case f e of
                Just x ->
                    Just x

                Nothing ->
                    first f (rest l)

        Nothing ->
            Nothing


monthInt : Date.Month -> Int
monthInt month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


captchaQ : Seed -> ( Seed, String, Int )
captchaQ seed =
    let
        ( a, seed1 ) =
            step (int 0 100) seed

        ( b, seed2 ) =
            step (int 0 100) seed1
    in
    ( seed2
    , "Whats " ++ toString a ++ " + " ++ toString b ++ "?"
    , a + b
    )


uuidList : Seed -> Int -> ( Seed, List Uuid.Uuid )
uuidList seed count =
    List.foldl
        (\_ ( seed, uuids ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed
            in
            ( newSeed, newUuid :: uuids )
        )
        ( seed, [] )
        (List.repeat count 0)
