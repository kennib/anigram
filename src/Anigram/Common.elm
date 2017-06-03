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
  , dragResize : (Corner, DragDrop.DragDrop)
  , hidden : Bool
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
  | Arrow

type ShapeType
  = Circle
  | Square

type alias Position =
  { x : Int
  , y : Int
  }

type alias Corner = (XSide, YSide)
type XSide = Left | Right
type YSide = Top | Bottom

type alias ObjectId = Int

type alias Frame =
  Dict ObjectId (List Change)

type Control msg
  = Button
    { tooltip : String
    , icon : Html msg
    , message : msg
    }
  | ObjectAdder
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

type alias Anigram =
  { objects : List Object
  , frames : List Frame
  }

type Msg
  = AddObject Object
  | SelectObject Object
  | SelectAddObject Object
  | DeselectAll
  | DragDrop DragDrop.DragDrop
  | DragResize Corner DragDrop.DragDrop
  | AddFrame
  | SelectFrame Int
  | SaveAnigram
  | AnigramSaved
  | LoadAnigram
  | AnigramLoaded Anigram
  | Selection Change
  | Control ControlMsg
  | NoOp

type Change
  = Hide Bool
  | SetText String
  | Move Position
  | Resize Corner Position
  | Fill Color
  | Stroke Color

type ControlMsg
  = OpenClose Int Bool
