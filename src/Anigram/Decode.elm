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
  Json.list decodeChange
    |> Json.map Maybe.values

decodeChange : Json.Decoder (Maybe Change)
decodeChange =
  Json.field "change" Json.string
  |> Json.andThen (\change ->
    case change of
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
      "resize" ->
        Json.map2 (\corner delta -> Just <| Resize corner delta)
          (Json.field "corner" decodeCorner)
          (Json.field "delta" decodePosition)
      "fill" ->
        Json.map (Just << Fill)
          (Json.field "color" decodeColor)
      "stroke" ->
        Json.map (Just << Stroke)
          (Json.field "color" decodeColor)
      _ ->
        Json.succeed Nothing
  )

decodeObjectType : Json.Decoder ObjectType
decodeObjectType =
  Json.string
  |> Json.map (\objectType ->
    case objectType of
      "Shape Circle" -> Shape Circle
      "Shape Square" -> Shape Square
      "Arrow" -> Arrow
      text -> 
        if String.startsWith "Text " text then
          Text <| String.dropLeft 6 <| String.dropRight 1 <| text
        else
          Text text
  )

decodeColor : Json.Decoder Color
decodeColor =
  Json.string
    |> Json.map (Result.withDefault Color.lightBlue << hexToColor)

decodePosition : Json.Decoder Position
decodePosition =
  Json.map2 Position
    (Json.field "x" Json.int)
    (Json.field "y" Json.int)

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
