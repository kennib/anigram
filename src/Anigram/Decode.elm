module Anigram.Decode exposing (..)

import Result
import Dict.Extra as Dict
import Maybe.Extra as Maybe

import Json.Decode as Json

import Color exposing (Color)
import ColorMath exposing (hexToColor)

import Anigram.Common exposing (..)

decodeAnigram : Json.Decoder Anigram
decodeAnigram =
  Json.map Anigram
    (Json.field "frames" <| Json.list decodeFrame)

decodeFrame : Json.Decoder Frame
decodeFrame =
  Json.dict decodeChanges
    |> Json.map (Dict.mapKeys (String.toInt >> Result.withDefault -1))

decodeChanges : Json.Decoder (List Change)
decodeChanges =
  Json.list (Json.lazy <| \_ -> decodeChange)
    |> Json.map Maybe.values

decodeChange : Json.Decoder (Maybe Change)
decodeChange =
  Json.field "change" Json.string
  |> Json.andThen (\change ->
    case change of
      "addStyleSet" ->
        Json.map (Just << AddStyleSet)
          (Json.field "styleSet" Json.string)
      "changeType" ->
        Json.map (Just << ChangeType)
          (Json.field "type" decodeObjectType)
      "hide" ->
        Json.map (Just << Hide)
          (Json.field "state" Json.bool)
      "setText" ->
        Json.map (Just << SetText)
          (Json.field "text" Json.string)
      "move" ->
        Json.map (Just << Move)
          (Json.field "position" decodePosition)
      "moveTo" ->
        Json.map (Just << MoveTo)
          (Json.field "position" decodePosition)
      "resize" ->
        Json.map2 (\corner delta -> Just <| Resize corner delta)
          (Json.field "corner" decodeCorner)
          (Json.field "delta" decodePosition)
      "sizeTo" ->
        Json.map (Just << SizeTo)
          (Json.field "size" decodeSize)
      "fill" ->
        Json.map (Just << Fill)
          (Json.field "color" decodeColor)
      "stroke" ->
        Json.map (Just << Stroke)
          (Json.field "color" decodeColor)
      "strokeWidth" ->
        Json.map (Just << StrokeWidth)
          (Json.field "width" Json.int)
      "textColor" ->
        Json.map (Just << TextColor)
          (Json.field "color" decodeColor)
      "textSize" ->
        Json.map (Just << TextSizeTo)
          (Json.field "size" Json.int)
      _ ->
        Json.succeed Nothing
  )

decodeObjectType : Json.Decoder ObjectType
decodeObjectType =
  Json.field "type" Json.string
  |> Json.andThen (\objectType ->
    case objectType of
      "shape" ->
        Json.map Shape
          (Json.field "shape" decodeShape)
      "text" ->
        Json.map2 Text
          (Json.field "text" Json.string)
          (Json.field "style" decodeTextStyle)
      "arrow" ->
        Json.succeed Arrow
      "arcArrow" ->
        Json.map ArcArrow
          (Json.field "radius" Json.float)
      otherType ->
        Json.fail <| otherType ++ " is not a type of object"
  )

decodeShape : Json.Decoder ShapeType
decodeShape =
  Json.string
  |> Json.andThen (\shape ->
    case shape of
      "Square" -> Json.succeed Square
      "Circle" -> Json.succeed Circle
      otherShape -> Json.fail <| otherShape ++ " is not a valid shape"
  )

decodeTextStyle : Json.Decoder TextStyle
decodeTextStyle =
  Json.map3 TextStyle
    (Json.field "size" Json.int)
    (Json.field "font" Json.string)
    (Json.field "color" decodeColor)

decodeColor : Json.Decoder Color
decodeColor =
  Json.string
    |> Json.map (Result.withDefault Color.lightBlue << hexToColor)

decodePosition : Json.Decoder Position
decodePosition =
  Json.map2 Position
    (Json.field "x" Json.int)
    (Json.field "y" Json.int)

decodeSize : Json.Decoder Size
decodeSize =
  Json.map2 Size
    (Json.field "width" Json.int)
    (Json.field "height" Json.int)

decodeCorner : Json.Decoder Corner
decodeCorner =
  Json.string
    |> Json.map (\corner ->
      case corner of
        "LeftTop" -> (Left, Top)
        "RightTop" -> (Right, Top)
        "LeftBottom" -> (Left, Bottom)
        "RightBottom" -> (Right, Bottom)
        _ -> (Left, Top)
  )
