module CustomElement exposing (..)

import Element exposing (Element)
import Element.Input
import Element.Font
import Element.Border
import VsColor exposing (VsColor(..))

column : List (Element msg) -> Element msg
column = Element.column [ Element.spacing 4 ]

text : String -> Element msg
text str =
    Element.el
        [ VsColor.backgroundColor Background
        , VsColor.fontColor Foreground
        ]
        (Element.text str)

button : { onPress : Maybe msg, label : String } -> Element msg
button { onPress, label } =
    Element.Input.button
        [ VsColor.backgroundColor Red
        , VsColor.fontColor Background
        , Element.Font.bold
        , Element.Border.rounded 4
        , Element.padding 4
        ]
        { onPress = onPress, label = Element.text label }
