module Anigram.Controls exposing (..)

import Dict
import Maybe.Extra as Maybe

import Json.Decode as Json
import Json.Encode exposing (encode)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

import Anigram.Common exposing (..)
import Anigram.Object as Obj exposing (defaultTextStyle)
import Anigram.Frames as Frames
import Anigram.Store as Store
import Anigram.StyleSets as StyleSets
import Anigram.Decode as Decode exposing (decodeChange)
import Anigram.Encode as Encode exposing (encodeChange)

model =
  [ addObjectControl "Add a Circle" Icon.circle <| Shape Circle
  , addObjectControl "Add a Square" Icon.square <| Shape Square
  , addObjectControl "Add an Arrow" Icon.long_arrow_right <| Arrow
  , addObjectControl "Add an Arc Arrow" Icon.reply <| ArcArrow 100
  , addObjectControl "Add Text" Icon.file_text <| Text "Add Text here" defaultTextStyle
  , listControl "Style Sets" Icon.paint_brush defaultStyleSet styleSets
  , buttonControl "Show" Icon.eye <| Selection <| Hide False 
  , buttonControl "Hide" Icon.eye_slash <| Selection <| Hide True
  , buttonControl "Duplicate" Icon.copy Duplicate
  , colorControl 0 "Fill color" Color.green FillSelector
  , colorControl 1 "Stroke color" Color.grey StrokeSelector
  , colorControl 2 "Text color" Color.red TextSelector
  , listControl "Text size" Icon.text_height defaultTextSize textSizes
  , buttonControl "Add Frame" Icon.plus_square AddFrame
  , buttonControl "Save" Icon.cloud_upload SaveAnigram
  , buttonControl "Load" Icon.cloud_download LoadAnigram
  ]

buttonControl tooltip icon message =
  newButton tooltip (icon Color.black 20) message

listControl tooltip icon default choices =
  newListPicker tooltip (icon Color.black 20) default choices

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

newListPicker tooltip icon default choices =
  ListPicker
  { tooltip = tooltip
  , icon = icon
  , choices = choices
  , choice = default
  }

newObjectAdder tooltip icon object =
  ObjectAdder
  { tooltip = tooltip
  , icon = icon
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

defaultTextSize =
  TextSizeTo defaultTextStyle.size

textSizes =
  List.map
    (\size -> (toString size, TextSizeTo size))
    [8, 12, 16, 24, 36, 48, 72, 106]

defaultStyleSet =
  AddStyleSet StyleSets.default

styleSets =
  StyleSets.sets
    |> Dict.toList
    |> List.map (Tuple.mapSecond AddStyleSet)

update msg model =
  let
    setAnigram anigram model =
      { model | objects = Frames.objectIds anigram.frames |> List.map Obj.newState, frames = anigram.frames }

    setChoiceOf choice =
      { model | controls = List.map (setChoice choice) model.controls }
    setChoice choice control =
      case control of
        ListPicker picker ->
          if List.member choice <| List.map Tuple.second picker.choices then
            ListPicker { picker | choice = choice }
          else
            control
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

    openCloseAt id state model =
      { model | controls = List.map (openClose id state) model.controls }
    openClose id state control =
      case control of
        ColorSelector selector ->
          if selector.id == id then
            ColorSelector { selector | open = state }
          else
            control
        _ -> control

    closeAll model =
      { model | controls = List.map close model.controls }
    close control =
      case control of
        ColorSelector selector ->
          ColorSelector { selector | open = False }
        _ -> control
  in
    case msg of
      SetCursor mode ->
        ({ model | cursorMode = mode }, Cmd.none)
      SaveAnigram ->
        (model, Store.saveAnigram { frames = model.frames })
      LoadAnigram ->
        (model, Store.loadAnigram)
      AnigramLoaded anigram ->
        (setAnigram anigram model, Cmd.none)
      DeselectAll ->
        (closeAll model, Cmd.none)
      Selection (AddStyleSet styleSet) ->
        (setChoiceOf <| AddStyleSet styleSet, Cmd.none)
      Selection (Fill color) ->
        (setColorOf FillSelector color, Cmd.none)
      Selection (Stroke color) ->
        (setColorOf StrokeSelector color, Cmd.none)
      Control (OpenClose id state) ->
        (model |> closeAll |> openCloseAt id state, Cmd.none)
      _ ->
        (closeAll model, Cmd.none)

view model =
  nav
    [ style
      [ ("position", "absolute")
      , ("padding", "5px")
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
    ListPicker control ->
      span
        [ title control.tooltip
        , style
          [ ("display", "inline-block")
          , ("box-sizing", "border-box")
          , ("height", "27px")
          , ("margin", "2px")
          , ("padding", "2px")
          , ("vertical-align", "bottom")
          , ("background-color", "#eee")
          , ("border", "1px solid grey")
          ]
        ]
        [ control.icon
        , select
          [ style
            [ ("width", "auto")
            , ("margin", "4px")
            , ("margin-top", "0px")
            , ("vertical-align", "bottom")
            , ("font-size", "14px")
            ]
          , on "change"
            <| Json.map
              (Maybe.withDefault NoOp << Maybe.map Selection << Maybe.join << Result.toMaybe << Json.decodeString decodeChange)
              (Json.at ["target", "value"] Json.string)
          ]
          <| List.map (\(label, choice) -> option [ value <| encode 0 <| encodeChange choice, default <| choice == control.choice ] [ text label ])
          <| control.choices
        ]
    ObjectAdder control ->
      button
        [ title control.tooltip
        , onClick <| AddObject control.object
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
              TextSelector -> Selection <| TextColor control.color
          , style
            [ ("margin-right", "0px")
            ]
            ]
            [ case control.kind of
              FillSelector -> Icon.dot_circle_o control.color 20
              StrokeSelector -> Icon.circle_o control.color 20
              TextSelector -> Icon.font control.color 20
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
        TextSelector -> Selection <| TextColor color
    ]
    [ Icon.square color 20
    ]

colors =
  [ white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
  , red, orange, yellow, green, blue, purple, brown
  ]
