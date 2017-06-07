module Html.Events.Extra exposing
  ( onShiftMouseDown
  , onPositionMouseDown
  )

import Mouse exposing (Position)

import Json.Decode as Json

import Html exposing (Attribute)
import Html.Events exposing (on)

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

shiftDecode : Json.Decoder Bool
shiftDecode =
  Json.field "shiftKey" Json.bool

positionDecode : Json.Decoder Position
positionDecode =
  Json.map2 Position
    (Json.field "offsetX" Json.int)
    (Json.field "offsetY" Json.int)
