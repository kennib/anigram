module Anigram.Common exposing (..)

import Dict exposing (Dict)

import Color exposing (Color)
import Html exposing (Html)

import DragDrop

type alias Object =
  { objectType : ObjectType
  , id : ObjectId
  , selected : Bool
  , dragDrop : DragDrop.DragDrop
  , x : Int 
  , y : Int
  , width : Int
  , height : Int
  , fill : Color
  , stroke : Color
  }

type ObjectType
  = Shape ShapeType
  | Text String
  | Arrow Position Position

type ShapeType
  = Circle
  | Square

type alias Position =
  { x : Int
  , y : Int
  }

type alias ObjectId = Int

type alias Frame =
  Dict ObjectId (List Change)

type Control msg
  = ObjectAdder
    { tooltip : String
    , icon : Html msg
    , objectId : ObjectId
    , object : Object
    }
  | ColorSelector
    { id : Int
    , kind : ColorSelectorKind
    , tooltip : String
    , color : Color
    , open : Bool
    }

type ColorSelectorKind
  = FillSelector
  | StrokeSelector

type alias Model =
  { objects : List Object
  , frames : List Frame
  , frameIndex : Int
  , controls : List (Control Msg)
  }

type Msg
  = AddObject Object
  | SelectObject Object
  | DragDrop DragDrop.DragDrop
  | AddFrame
  | SelectFrame Int
  | Selection Change
  | Control ControlMsg

type Change
  = SetText String
  | Move Position
  | Fill Color
  | Stroke Color

type ControlMsg
  = OpenClose Int Bool
