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
    { id : Int
    , tooltip : String
    , color : Color
    , icon : Color -> Int -> Html msg
    , message : Color -> msg
    , open : Bool
    }

type ControlMsg
  = NewObject Object
  | Fill Color
  | Stroke Color
  | OpenClose Int Bool

controls =
  merge controlsView
    [ addObjectControl "Add a Circle" Icon.circle <| Obj.newShape Circle
    , addObjectControl "Add a Square" Icon.square <| Obj.newShape Square
    , addObjectControl "Add Text" Icon.file_text <| Obj.newText "Add Text here"
    , colorControl 0 "Fill" Color.green Icon.dot_circle_o Fill
    , colorControl 1 "Stroke" Color.grey Icon.circle_o Stroke
    ]

addObjectControl tooltip icon object =
  { init = (newControl tooltip icon <| NewObject object, Cmd.none)
  , update = \msg model -> (model, Cmd.none)
  , subscriptions = \_ -> Sub.none
  , view = controlView
  }

colorControl id tooltip color icon msg =
  let
    model color = newColorSelector id tooltip color icon msg
  in
    { init = (model color, Cmd.none)
    , update = colorUpdate
    , subscriptions = \_ -> Sub.none
    , view = controlView
    }

colorUpdate msg model =
  case model of
    ColorSelector control ->
      let
        update color =
          if msg == control.message color then
            (ColorSelector { control | color = color }, Cmd.none)
          else
            (model, Cmd.none)
        openClose id state =
          if id == control.id then
            (ColorSelector { control | open = state }, Cmd.none)
          else if state == True then
            (ColorSelector { control | open = False }, Cmd.none)
          else
            (model, Cmd.none)
      in
        case msg of
          Fill color ->
            update color
          Stroke color ->
            update color
          OpenClose id state ->
            openClose id state
          _ ->
            (model, Cmd.none)
    _ ->
      (model, Cmd.none)

newControl tooltip icon message =
  Button
  { tooltip = tooltip
  , icon = icon
  , message = message
  }

newColorSelector id tooltip color icon message =
  ColorSelector
  { id = id
  , tooltip = tooltip
  , color = color
  , icon = icon
  , message = message
  , open = False
  }

controlsView controls =
  nav
    [ style
      [ ("padding", "5px")
      , ("background-color", "#333")
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
          , style
            [ ("margin-right", "0px")
            ]
          ]
          [ control.icon control.color 20
          ]
        , button
          [ title control.tooltip
          , onClick (OpenClose control.id <| not control.open)
          , style
            [ ("margin-left", "0px")
            , ("padding-left", "0px")
            , ("padding-right", "0px")
            ]
          ]
          [ if control.open then
              Icon.caret_up black 20
            else
              Icon.caret_down black 20
          ]
        , if control.open then
            span
              [ style
                  [ ("position", "absolute")
                  , ("top", "34px")
                  , ("width", "320px")
                  , ("margin-left", "-26px")
                  , ("padding","5px")
                  , ("background-color", "#eee")
                  ]
              ]
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
