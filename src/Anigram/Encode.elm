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
    AddStyleSet styleSet ->
      Json.object
        [ ( "change", Json.string "addStyleSet" )
        , ( "styleSet", Json.string styleSet )
        ]
    ChangeType objectType ->
      Json.object
        [ ( "change", Json.string "changeType" )
        , ( "type", encodeObjectType objectType )
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
    MoveTo position ->
      Json.object
        [ ( "change", Json.string "moveTo" )
        , ( "position", encodePosition position )
        ]
    Resize corner position ->
      Json.object
        [ ( "change", Json.string "resize" )
        , ( "corner", encodeCorner corner )
        , ( "delta", encodePosition position )
        ]
    SizeTo size ->
      Json.object
        [ ( "change", Json.string "sizeTo" )
        , ( "size", encodeSize size )
        ]
    Fill color ->
      Json.object
        [ ("change", Json.string "fill" )
        , ("color", encodeColor color)
        ]
    Stroke color ->
      Json.object
        [ ("change", Json.string "stroke" )
        , ("color", encodeColor color)
        ]
    TextColor color ->
      Json.object
        [ ("change", Json.string "textColor" )
        , ("color", encodeColor color)
        ]
    TextSizeTo size ->
      Json.object
        [ ("change", Json.string "textSize" )
        , ("size", Json.int size)
        ]

encodeObjectType : ObjectType -> Json.Value
encodeObjectType objectType =
  Json.object <|
    case objectType of
      Shape shape ->
        [ ( "type", Json.string "shape" )
        , ( "shape", Json.string <| toString shape)
        ]
      Text text style ->
        [ ( "type", Json.string "text" )
        , ( "text", Json.string text )
        , ( "style", encodeTextStyle style )
        ]
      Arrow ->
        [ ( "type", Json.string "arrow" )
        ]
      ArcArrow radius ->
        [ ( "type", Json.string "arcArrow" )
        , ( "radius", Json.float radius )
        ]

encodeTextStyle : TextStyle -> Json.Value
encodeTextStyle style =
  Json.object
    [ ( "size", Json.int style.size )
    , ( "font", Json.string style.font )
    , ( "color", encodeColor style.color )
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

encodeSize : Size -> Json.Value
encodeSize size =
  Json.object
    [ ( "width", Json.int size.width )
    , ( "height", Json.int size.height )
    ]

encodeCorner : Corner -> Json.Value
encodeCorner (x, y) =
  Json.string
    <| toString x ++ toString y
