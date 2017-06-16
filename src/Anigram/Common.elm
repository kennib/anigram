module Anigram.Common exposing (..)

import Dict exposing (Dict)

import Color exposing (Color)
import Html exposing (Html)

import DragDrop

type alias Object =
  { id : ObjectId
  , state : State
  , style : Style
  }

type alias ObjectState =
  { id : ObjectId
  , state : State
  }

type alias ObjectStyle =
  { id : ObjectId
  , style : Style
  }

type alias State =
  { selected : Bool
  }

type alias Style =
  { objectType : ObjectType
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
  | Text String TextStyle
  | Arrow
  | ArcArrow Float

type ShapeType
  = Circle
  | Square

type alias TextStyle =
  { size : Int
  , font : String
  , color : Color
  }

type alias Position =
  { x : Int
  , y : Int
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Corner = (XSide, YSide)
type XSide = Left | Right
type YSide = Top | Bottom

type alias ObjectId = Int

type SnapLine
  = HorizontalSnap Int
  | VerticalSnap Int

type alias Frame =
  Dict ObjectId (List Change)

type Control msg
  = Button
    { tooltip : String
    , icon : Html msg
    , message : msg
    }
  | ListPicker
    { tooltip : String
    , icon : Html msg
    , choices : List (String, Change)
    , choice : Change
    }
  | ObjectAdder
    { tooltip : String
    , icon : Html msg
    , object : ObjectType
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
  | TextSelector

type alias Model =
  { objects : List ObjectState
  , frames : List Frame
  , frameIndex : Int
  , controls : List (Control Msg)
  , focus : Focus
  , cursorMode : CursorMode
  , history : History (List Frame, List ObjectState)
  }

type alias Anigram =
  { frames : List Frame
  }

type Focus
  = ObjectArea
  | FrameArea

type  CursorMode
  = SelectMode
  | DragSelectMode DragDrop.DragDrop
  | DragMode DragDrop.DragDrop
  | DragResizeMode Corner DragDrop.DragDrop
  | PlaceObjectMode ObjectType

type alias History a =
  { past : List a
  , future : List a
  }

type Msg
  = AddObject ObjectType
  | PlaceObject ObjectType Position
  | SelectObject ObjectId
  | SelectAddObject ObjectId
  | DeselectAll
  | SelectAll
  | SetCursor CursorMode
  | DragSelect Position Position
  | SelectDragDrop ObjectId DragDrop.DragDrop
  | DragDrop DragDrop.DragDrop
  | DragResize Corner DragDrop.DragDrop
  | Duplicate
  | Undo
  | Redo
  | AddFrame
  | SelectFrame Int
  | PreviousFrame
  | NextFrame
  | DeleteFrame
  | SaveAnigram
  | AnigramSaved
  | LoadAnigram
  | AnigramLoaded Anigram
  | Selection Change
  | Control ControlMsg
  | NoOp

type Change
  = AddStyleSet StyleSet
  | ChangeType ObjectType
  | Hide Bool
  | SetText String
  | Move Position
  | MoveTo Position
  | Resize Corner Position
  | SizeTo Size
  | Fill Color
  | Stroke Color
  | TextColor Color
  | TextSizeTo Int

type alias StyleSet = List Change

type ControlMsg
  = OpenClose Int Bool
