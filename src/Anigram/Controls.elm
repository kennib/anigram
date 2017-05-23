module Anigram.Controls exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Component exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

import Anigram.Object as Obj exposing (Object(..), ObjectType(..), ShapeType(..))

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

type ControlMsg
  = Fill Color
  | Stroke Color
  | NewObject Object

controls =
  merge controlsView
    [ addObjectControl "Add a Circle" Icon.circle <| Obj.newShape Circle
    , addObjectControl "Add a Square" Icon.square <| Obj.newShape Square
    , addObjectControl "Add Text" Icon.file_text <| Obj.newText "Add Text here"
    , colorControl "Fill" Color.green Icon.dot_circle_o Fill
    , colorControl "Stroke" Color.grey Icon.circle_o Stroke
    ]

addObjectControl tooltip icon object =
  { init = (newControl tooltip icon <| NewObject object, Cmd.none)
  , update = \msg model -> (model, Cmd.none)
  , subscriptions = \_ -> Sub.none
  , view = controlView
  }

colorControl tooltip color icon msg =
  let
    model color = newColorSelector tooltip color icon msg
  in
    { init = (model color, Cmd.none)
    , update = colorUpdate msg model
    , subscriptions = \_ -> Sub.none
    , view = controlView
    }

colorUpdate ctrlMsg ctrlModel msg model =
  let
    update color =
      if msg == ctrlMsg color then
        (ctrlModel color, Cmd.none)
      else
        (model, Cmd.none)
  in
    case msg of
      Fill color ->
        update color
      Stroke color ->
        update color
      _ ->
        (model, Cmd.none)

newControl tooltip icon message =
  Button
  { tooltip = tooltip
  , icon = icon
  , message = message
  }

newColorSelector tooltip color icon message =
  ColorSelector
  { tooltip = tooltip
  , color = color
  , icon = icon
  , message = message
  , open = True
  }

controlsView controls =
  nav
    [ style
      [ ("padding", "5px")
      ]
    ]
    controls

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
