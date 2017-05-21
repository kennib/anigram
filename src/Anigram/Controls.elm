module Anigram.Controls exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

type Control msg
  = Button
    { tooltip : String
    , icon : Color -> Int -> Html msg
    , message : msg
    }
  | ColorSelector
    { tooltip : String
    , color : Color
    , icon : Color -> Int -> Html msg
    , message : Color -> msg
    , open : Bool
    , openMessage : msg
    }

newControl tooltip icon message =
  Button
  { tooltip = tooltip
  , icon = icon
  , message = message
  }

newColorSelector tooltip color icon message openMessage =
  ColorSelector
  { tooltip = tooltip
  , color = color
  , icon = icon
  , message = message
  , open = True
  , openMessage = openMessage
  }

controlsView config model =
  nav
    [ style
      [ ("padding", "5px")
      ]
    ]
  <| List.map controlView
  <| model.controls

controlView control =
  case control of
    Button control ->
      button
        [ title control.tooltip
        , onClick control.message
        ]
        [ control.icon black 20
        ]
    ColorSelector control ->
      span
        []
        [ button
          [ title control.tooltip
          , onClick (control.message control.color)
          ]
          [ control.icon control.color 20
          ]
        , button
          [ title control.tooltip
          , onClick control.openMessage
          ]
          [ Icon.caret_down black 20
          ]
        , if control.open then
            div []
              <| List.map (colorButton control) colors
          else
            text ""
        ]

colorButton control color =
  button
    [ title <| colorToHex color
    , onClick (control.message color)
    ]
    [ Icon.square color 20
    ]

colors =
  [ white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
  , red, orange, yellow, green, blue, purple, brown
  ]
