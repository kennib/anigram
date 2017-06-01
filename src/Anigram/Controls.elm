module Anigram.Controls exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

import Anigram.Common exposing (..)
import Anigram.Object as Obj
import Anigram.Store as Store

model =
  [ addObjectControl "Add a Circle" Icon.circle <| Obj.select <| Obj.newShape Circle
  , addObjectControl "Add a Square" Icon.square <| Obj.select <| Obj.newShape Square
  , addObjectControl "Add an Arrow" Icon.long_arrow_right <| Obj.select <| Obj.newArrow
  , addObjectControl "Add Text" Icon.file_text <| Obj.select <| Obj.newText "Add Text here"
  , colorControl 0 "Fill" Color.green FillSelector
  , colorControl 1 "Stroke" Color.grey StrokeSelector
  , buttonControl "Add Frame" Icon.plus_square AddFrame
  , buttonControl "Save" Icon.cloud_upload SaveAnigram
  , buttonControl "Load" Icon.cloud_download LoadAnigram
  ]

buttonControl tooltip icon message =
  newButton tooltip (icon Color.black 20) message

addObjectControl tooltip icon object =
  newObjectAdder tooltip (icon Color.black 20) object

colorControl id tooltip color kind =
  newColorSelector id tooltip color kind

newButton tooltip icon message =
  Button
  { tooltip = tooltip
  , icon = icon
  , message = message
  }

newObjectAdder tooltip icon object =
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

update msg model =
  let
    updateObjectIds objectId =
      { model | controls = List.map (updateObjectId objectId) model.controls }
    updateObjectId objectId control =
      case control of
        ObjectAdder control ->
          ObjectAdder { control | objectId = objectId }
        _ -> control

    setAnigram anigram model =
      { model | objects = anigram.objects, frames = anigram.frames }

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
      SaveAnigram ->
        (model, Store.saveAnigram { objects = model.objects, frames = model.frames })
      LoadAnigram ->
        (model, Store.loadAnigram)
      AnigramLoaded anigram ->
        (setAnigram anigram model, Cmd.none)
      DeselectAll ->
        (closeAll, Cmd.none)
      Selection (Fill color) ->
        (setColorOf FillSelector color, Cmd.none)
      Selection (Stroke color) ->
        (setColorOf StrokeSelector color, Cmd.none)
      Control (OpenClose id state) ->
        (openCloseAt id state, Cmd.none)
      _ ->
        (closeAll, Cmd.none)

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
    Button control ->
      button
        [ title control.tooltip
        , onClick control.message 
        ]
        [ control.icon
        ]
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
