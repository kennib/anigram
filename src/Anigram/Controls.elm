module Anigram.Controls exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)

type alias Control msg =
  { tooltip : String
  , icon : Color -> Int -> Html msg
  , message : msg
  }

newControl tooltip icon message =
  { tooltip = tooltip
  , icon = icon
  , message = message
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
  button
    [ title control.tooltip
    , onClick control.message
    ]
    [ control.icon black 20
    ]
