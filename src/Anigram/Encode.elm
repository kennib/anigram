module Anigram.Encode exposing (..)

import Dict
import Dict.Extra as Dict

import Json.Encode as Json

import Color exposing (Color)
import ColorMath exposing (colorToHex)

import Anigram.Common exposing (..)

encodeAnigram : Anigram -> Json.Value
encodeAnigram anigram =
  Json.object
    [ ( "frames", Json.list <| List.map encodeFrame anigram.frames )
    ]

encodeFrame : Frame -> Json.Value
encodeFrame frame =
  Json.object
    <| Dict.toList
    <| Dict.mapKeys toString
    <| Dict.map (\id changes -> encodeChanges changes)
    <| frame

encodeChanges : List Change -> Json.Value
encodeChanges changes =
  Json.list
    <| List.map encodeChange changes

encodeChange : Change -> Json.Value
encodeChange change =
  case change of
    ChangeType objectType ->
      Json.object
        [ ( "change", Json.string "changeType" )
        , ( "type", Json.string <| toString objectType )
        ]
    Hide state ->
      Json.object
        [ ( "change", Json.string "hide" )
        , ( "state", Json.bool state )
        ]
    SetText string ->
      Json.object
        [ ( "change", Json.string "setText" )
        , ( "text", Json.string string )
        ]
    Move position ->
      Json.object
        [ ( "change", Json.string "move" )
        , ( "position", encodePosition position )
        ]
    Resize corner position ->
      Json.object
        [ ( "change", Json.string "resize" )
        , ( "corner", encodeCorner corner )
        , ( "delta", encodePosition position )
        ]
    Fill color ->
      Json.object
        [ ("change", Json.string "fill" )
        , ("color", encodeColor color)
        ]
    Stroke color ->
      Json.object
        [ ("change", Json.string "Stroke" )
        , ("color", encodeColor color)
        ]

encodeColor : Color -> Json.Value
encodeColor color =
  Json.string
    <| colorToHex
    <| color

encodePosition : Position -> Json.Value
encodePosition position =
  Json.object
    [ ( "x", Json.int position.x )
    , ( "y", Json.int position.y )
    ]

encodeCorner : Corner -> Json.Value
encodeCorner (x, y) =
  Json.string
    <| toString x ++ toString y
