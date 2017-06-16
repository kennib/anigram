module Html.Events.Extra exposing
  ( onShiftPositionMouseDown
  , onShiftMouseDown
  , onPositionMouseDown
  , onPositionMouseUp
  , onPositionMouseMove
  , onComboKeyDown
  )

import Maybe.Extra as Maybe

import Mouse exposing (Position)
import Keyboard.Key exposing (Key)

import Json.Decode as Json

import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions, defaultOptions)

onShiftPositionMouseDown : (Bool -> Position -> msg) -> Attribute msg
onShiftPositionMouseDown msg =
  on "mousedown"
    <| Json.map2 msg
      shiftDecode
      positionDecode

onShiftMouseDown : (Bool -> msg) -> Attribute msg
onShiftMouseDown msg =
  on "mousedown"
    <| Json.map msg 
    <| shiftDecode

onPositionMouseDown : (Position -> msg) -> Attribute msg
onPositionMouseDown msg =
  on "mousedown"
    <| Json.map msg
    <| positionDecode

onPositionMouseUp : (Position -> msg) -> Attribute msg
onPositionMouseUp msg =
  on "mouseup"
    <| Json.map msg
    <| positionDecode

onPositionMouseMove : (Position -> msg) -> Attribute msg
onPositionMouseMove msg =
  on "mousemove"
    <| Json.map msg
    <| positionDecode

onComboKeyDown : ((Bool, Bool, Key) -> msg) -> Attribute msg
onComboKeyDown msg =
  onWithOptions "keydown" { defaultOptions | preventDefault = True }
    <| Json.map msg
    <| comboKeyDecode

shiftDecode : Json.Decoder Bool
shiftDecode =
  Json.field "shiftKey" Json.bool

positionDecode : Json.Decoder Position
positionDecode =
  Json.map2 Position
    (Json.field "offsetX" Json.int)
    (Json.field "offsetY" Json.int)

comboKeyDecode : Json.Decoder (Bool, Bool, Key)
comboKeyDecode =
  let
    combo os ctrl cmd shift key keyCode =
      (if String.contains "Mac" os then cmd else ctrl
      , shift
      , Maybe.or key keyCode |> Maybe.map Keyboard.Key.fromCode |> Maybe.withDefault (Keyboard.Key.Unknown 0)
      )
  in
    Json.map6 combo
      (Json.at ["view", "navigator", "platform"] Json.string)
      (Json.field "ctrlKey" Json.bool)
      (Json.field "metaKey" Json.bool)
      (Json.field "shiftKey" Json.bool)
      (Json.maybe <| Json.field "key" Json.int)     -- Newest version of keyboardEvent spec
      (Json.maybe <| Json.field "keyCode" Json.int) -- Version of the spec Chrome and Safari conform to
