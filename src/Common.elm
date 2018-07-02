module Common exposing (..)

import Char
import Color exposing (Color)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input


tagPill : List (Attribute msg) -> String -> Element msg
tagPill attribs tagname =
    Input.button
        (attribs
            ++ [ Font.color Color.white
               , Border.color Color.darkBlue
               , paddingXY 5 3
               , Border.rounded 5
               ]
        )
        { label =
            text <|
                String.map
                    (\c ->
                        if c == ' ' then
                            Char.fromCode 160
                        else
                            c
                    )
                    tagname
        , onPress = Nothing
        }


buttonStyle =
    [ Background.color Color.blue
    , Font.color Color.white
    , Border.color Color.darkBlue
    , paddingXY 10 5
    , Border.rounded 3
    ]


lightOrange =
    Color.rgb 255 181 64


lighterBlue : Color
lighterBlue =
    Color.rgb 171 238 255
