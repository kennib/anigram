module Anigram.Object exposing (..)

import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

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
  }

type alias Position =
  { x : Float
  , y : Float
  }

defaultStyle =
  { x = 0
  , y = 0
  , width = 100
  , height = 100
  }

newShape shape =
  Object (Shape shape) defaultStyle

view config object =
  case object of
    Object (Shape Circle) style ->
      circle
        [ cx <| toString style.x
        , cy <| toString style.y
        , r <| toString style.width
        , onMouseDown (config object).mouseDown
        , Attr.cursor (config object).cursor
        ]
        []
    Object (Shape Square) style ->
      rect
        [ x <| toString style.x
        , y <| toString style.y
        , width <| toString style.width
        , height <| toString style.height
        , onMouseDown (config object).mouseDown
        , Attr.cursor (config object).cursor
        ]
        []
    object ->
      text_
        []
        [text <| toString object]
