module Anigram.Object exposing (..)

import Json.Decode as Json

import Svg exposing (..)
import Svg.Events exposing (..)
import Html.Events exposing (onWithOptions)
import Svg.Attributes as Attr exposing (..)

import Color exposing (Color)
import ColorMath exposing (colorToHex)

type Object =
  Object ObjectType ObjectStyle

type ObjectType
  = Shape ShapeType
  | Text String
  | Arrow Position Position

type ShapeType
  = Circle
  | Square

type alias ObjectStyle =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  , fill : Color
  , stroke : Color
  }

type alias Position =
  { x : Float
  , y : Float
  }

defaultStyle =
  { x = 50
  , y = 50
  , width = 100
  , height = 100
  , fill = Color.lightBlue
  , stroke = Color.black
  }

newShape shape =
  Object (Shape shape) defaultStyle

newText text =
  Object (Text text) defaultStyle

moveObject object delta =
  let
    (Object obj style) = object
  in
    Object obj
      { style
      | x = style.x + toFloat delta.x
      , y = style.y + toFloat delta.y
      }

corners (Object object style) =
  [ { x = style.x, y = style.y }
  , { x = style.x+style.width, y = style.y }
  , { x = style.x+style.width, y = style.y+style.height }
  , { x = style.x, y = style.y+style.height }
  ]

view config object =
  case object of
    Object (Shape Circle) style ->
      circle
        [ cx <| toString (style.x + style.width/2)
        , cy <| toString (style.y + style.width/2)
        , r <| toString <| style.width/2
        , fill <| "#" ++ colorToHex style.fill
        , stroke <| "#" ++ colorToHex style.stroke
        , onMouseDown (config object).mouseDown
        , onClick (config object).click
        , Attr.cursor (config object).cursor
        ]
        []
    Object (Shape Square) style ->
      rect
        [ x <| toString style.x
        , y <| toString style.y
        , width <| toString style.width
        , height <| toString style.height
        , fill <| "#" ++ colorToHex style.fill
        , stroke <| "#" ++ colorToHex style.stroke
        , onMouseDown (config object).mouseDown
        , onClick (config object).click
        , Attr.cursor (config object).cursor
        ]
        []
    Object (Text string) style ->
      text_
        [ x <| toString style.x
        , y <| toString style.y
        , dy "10"
        , onWithOptions
          "mousedown"
          { stopPropagation = True
          , preventDefault = True
          }
          (Json.succeed (config object).mouseDown)
        , onClick (config object).click
        , Attr.cursor (config object).cursor
        ]
        [text string]
    object ->
      text_
        []
        [text <| toString object]

selectedView config object =
  let
    corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "5"
        , fill "white", stroke "black" ] []
  in
    g [] <|
    [ view config object
    ] ++ List.map corner (corners object)
