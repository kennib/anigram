module Anigram.Controls exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

import Anigram.Common exposing (..)
import Anigram.Object as Obj

model =
  [ addObjectControl "Add a Circle" Icon.circle <| Obj.newShape Circle
  , addObjectControl "Add a Square" Icon.square <| Obj.newShape Square
  , addObjectControl "Add Text" Icon.file_text <| Obj.newText "Add Text here"
  , colorControl 0 "Fill" Color.green FillSelector
  , colorControl 1 "Stroke" Color.grey StrokeSelector
  ]

addObjectControl tooltip icon object =
  newControl tooltip (icon Color.black 20) object

colorControl id tooltip color kind =
  newColorSelector id tooltip color kind

update msg model =
  let
    updateObjectIds objectId =
      { model | controls = List.map (updateObjectId objectId) model.controls }
    updateObjectId objectId control =
      case control of
        ObjectAdder control ->
          ObjectAdder { control | objectId = objectId }
        _ -> control

    setColorOf kind color =
      { model | controls = List.map (setColor kind color) model.controls }
    setColor kind color control =
      case control of
        ColorSelector selector ->
          if selector.kind == kind then
            ColorSelector { selector | color = color }
          else
            control
        _ -> control

    openCloseAt id state =
      { model | controls = List.map (openClose id state) model.controls }
    openClose id state control =
      case control of
        ColorSelector selector ->
          if selector.id == id then
            ColorSelector { selector | open = state }
          else
            control
        _ -> control

    closeAll =
      { model | controls = List.map close model.controls }
    close control =
      case control of
        ColorSelector selector ->
          ColorSelector { selector | open = False }
        _ -> control
  in
    case msg of
      AddObject _ ->
        (updateObjectIds <| List.length model.objects + 1, Cmd.none)
      Selection (Fill color) ->
        (setColorOf FillSelector color, Cmd.none)
      Selection (Stroke color) ->
        (setColorOf StrokeSelector color, Cmd.none)
      Control (OpenClose id state) ->
        (openCloseAt id state, Cmd.none)
      _ ->
        (closeAll, Cmd.none)

newControl tooltip icon object =
  ObjectAdder
  { tooltip = tooltip
  , icon = icon
  , objectId = 0
  , object = object
  }

newColorSelector id tooltip color kind =
  ColorSelector
  { id = id
  , kind = kind
  , tooltip = tooltip
  , color = color
  , open = False
  }

view model =
  nav
    [ style
      [ ("padding", "5px")
      , ("background-color", "#333")
      ]
    ]
    <| List.map controlView model.controls

controlView control =
  case control of
    ObjectAdder control ->
      let
        object = control.object
      in
        button
          [ title control.tooltip
          , onClick <| AddObject { object | id = control.objectId }
          ]
          [ control.icon
          ]
    ColorSelector control ->
      span
        []
        [ button
          [ title control.tooltip
          , onClick <|
            case control.kind of
              FillSelector -> Selection <| Fill control.color
              StrokeSelector -> Selection <| Stroke control.color
          , style
            [ ("margin-right", "0px")
            ]
            ]
            [ case control.kind of
              FillSelector -> Icon.dot_circle_o control.color 20
              StrokeSelector -> Icon.circle_o control.color 20
            ]
        , button
          [ title control.tooltip
          , onClick (Control <| OpenClose control.id <| not control.open)
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
    , onClick <|
      case control.kind of
        FillSelector -> Selection <| Fill color
        StrokeSelector -> Selection <| Stroke color
    ]
    [ Icon.square color 20
    ]

colors =
  [ white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
  , red, orange, yellow, green, blue, purple, brown
  ]
