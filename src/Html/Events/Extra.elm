module Html.Events.Extra exposing
  ( onShiftMouseDown
  )

import Json.Decode as Json

import Html exposing (Attribute)
import Html.Events exposing (on)

onShiftMouseDown : (Bool -> msg) -> Attribute msg
onShiftMouseDown msg =
  on "mousedown"
    <| Json.map msg 
    <| shiftDecode

shiftDecode : Json.Decoder Bool
shiftDecode =
  Json.field "shiftKey" Json.bool
