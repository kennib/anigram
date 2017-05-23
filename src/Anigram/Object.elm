module Anigram.Object exposing (..)

import Mouse
import DragDrop exposing (DragDrop(..))

import Json.Decode as Json

import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Component exposing (..)

import Color exposing (Color)
import ColorMath exposing (colorToHex)

type alias Object =
  { objectType : ObjectType
  , id : ObjectId
  , selected : Bool
  , dragDrop : DragDrop
  , x : Float
  , y : Float
  , width : Float
  , height : Float
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

type alias ObjectId = Int

type alias Position =
  { x : Float
  , y : Float
  }

type ObjectMsg
  = Create (ObjectId -> Object)
  | Click ObjectId
  | PickUp ObjectId
  | Drag Mouse.Position
  | Drop Mouse.Position
  | Fill Color
  | Stroke Color

objectsComponent : List (Component Object ObjectMsg) -> Component (List Object) ObjectMsg
objectsComponent objects =
  merge objectsView objects

object model =
  { init = (model, Cmd.none)
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

defaultObject =
  { objectType = Shape Circle
  , id = -1
  , selected = True
  , dragDrop = Unselected
  , x = 50
  , y = 50
  , width = 100
  , height = 100
  , fill = Color.lightBlue
  , stroke = Color.black
  }

newShape shape id =
  { defaultObject
  | objectType = Shape shape
  , id = id
  }

newText text id =
  { defaultObject
  | objectType = Text text
  , id = id
  }

update msg object =
  let
    select id =
      if id == object.id then
        ({ object | selected = True }, Cmd.none)
      else
        ({ object | selected = False, dragDrop = Unselected }, Cmd.none)

    pickup id =
      if id == object.id then
        ({ object | selected = True, dragDrop = PickedUp }, Cmd.none)
      else
        ({ object | selected = False, dragDrop = Unselected }, Cmd.none)

    updateFilter predicate updated =
      if predicate object then
        (updated, Cmd.none)
      else
        (object, Cmd.none)
  in
     case msg of
        Create _ ->
          (object, Cmd.none)
        Click id ->
          select id
        PickUp id ->
          pickup id
        Drag pos ->
          updateFilter .selected <| drag object pos
        Drop pos ->
          updateFilter .selected <| drop <| drag object pos
        Fill color ->
          updateFilter .selected { object | fill = color }
        Stroke color ->
          updateFilter .selected { object | stroke = color }

subscriptions object =
  if DragDrop.isDragged object.dragDrop then
    Sub.batch
      [ Mouse.moves Drag
      , Mouse.ups Drop
      ]
  else
    Sub.none

moveObject object delta =
  { object
  | x = object.x + toFloat delta.x
  , y = object.y + toFloat delta.y
  }

drag object pos =
  { object | dragDrop = DragDrop.drag object.dragDrop pos }

drop object =
  case DragDrop.drop object.dragDrop of
    Nothing ->
      object
    Just delta ->
      let
        movedObject = moveObject object delta
      in
        { movedObject | dragDrop = Unselected }

corners object =
  [ { x = object.x, y = object.y }
  , { x = object.x+object.width, y = object.y }
  , { x = object.x+object.width, y = object.y+object.height }
  , { x = object.x, y = object.y+object.height }
  ]

objectsView objects =
  g [] objects

view object =
  if DragDrop.isDragged object.dragDrop then
    unselectedView <| drop object
  else if object.selected then
    selectedView object
  else
    unselectedView object

unselectedView object =
  case object.objectType of
    Shape Circle ->
      circle
        [ cx <| toString (object.x + object.width/2)
        , cy <| toString (object.y + object.width/2)
        , r <| toString <| object.width/2
        , fill <| "#" ++ colorToHex object.fill
        , stroke <| "#" ++ colorToHex object.stroke
        , onMouseDown (PickUp object.id)
        , onClick (Click object.id)
        , Attr.cursor "move"
        ]
        []
    Shape Square ->
      rect
        [ x <| toString object.x
        , y <| toString object.y
        , width <| toString object.width
        , height <| toString object.height
        , fill <| "#" ++ colorToHex object.fill
        , stroke <| "#" ++ colorToHex object.stroke
        , onMouseDown (PickUp object.id)
        , onClick (Click object.id)
        , Attr.cursor "move"
        ]
        []
    Text string ->
      text_
        [ x <| toString object.x
        , y <| toString object.y
        , dy "10"
        , onMouseDown (PickUp object.id)
        , onClick (Click object.id)
        , Attr.cursor "move"
        ]
        [text string]
    object ->
      text_
        []
        [text <| toString object]

selectedView object =
  let
    corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "5"
        , fill "white", stroke "black" ] []
  in
    g [] <|
    [ unselectedView object
    ] ++ List.map corner (corners object)
