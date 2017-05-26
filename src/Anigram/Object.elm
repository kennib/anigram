module Anigram.Object exposing (..)

import Mouse
import DragDrop exposing (DragDrop(..))

import Json.Decode as Json

import Html exposing (div, textarea)
import Html.Attributes exposing (attribute, autofocus)
import Html.Events exposing (onInput)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Cmd
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
  = Create Object
  | NextId ObjectId
  | Set (List Object)
  | Click ObjectId
  | PickUp ObjectId
  | Drag Mouse.Position
  | Drop Mouse.Position
  | EditText String
  | Fill Color
  | Stroke Color


objects : Component (List Object) ObjectMsg
objects =
  { init = (objectsComponent []).init
  , update = updateObjects
  , subscriptions = \model -> (objectsComponent <| List.map object model).subscriptions  model
  , view = \model -> (objectsComponent <| List.map object model).view model |> objectDisplayView model
  }

updateObjects msg model =
  let
    (newObjects, cmd) = (objectsComponent <| List.map object model).update msg model
    unselectObjects = List.map (\object -> { object | selected = False }) newObjects
  in
    case msg of
      Create object ->
        (unselectObjects ++ [object], Cmd.message <| NextId <| List.length model + 1)
      Set objects ->
        (objects, cmd)
      _ ->
        (newObjects, cmd)

objectDisplayView objects objectsHtml =
  div
    [ Attr.style "height: 100vh; flex-grow: 1;" ] <|
    List.map textEditView objects ++
    [ svg
      [ width "100%"
      , height "100%"
      ]
      [ objectsHtml
      ]
    ]

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
        Click id ->
          select id
        PickUp id ->
          pickup id
        Drag pos ->
          updateFilter .selected <| drag object pos
        Drop pos ->
          updateFilter .selected <| drop <| drag object pos
        EditText string ->
          case object.objectType of
            Text _ ->
              updateFilter .selected { object | objectType = Text string }
            _ ->
              (object, Cmd.none)
        Fill color ->
          updateFilter .selected { object | fill = color }
        Stroke color ->
          updateFilter .selected { object | stroke = color }
        _ ->
          (object, Cmd.none)

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
        , dx "2"
        , dy "12"
        , fontSize "12"
        , fontFamily "sans-serif"
        , onMouseDown (PickUp object.id)
        , onClick (Click object.id)
        , Attr.cursor "text"
        ]
        [text string]
    object ->
      text_
        []
        [text <| toString object]

textEditView object =
  let
    obj =
      if DragDrop.isDragged object.dragDrop then
        drop object
      else
        object
  in
    case (obj.objectType, obj.selected) of
      (Text string, True) ->
        div
          [ Html.Attributes.style
            [ ("position", "absolute")
            , ("left", toString obj.x ++ "px")
            , ("top", toString (obj.y + 40) ++ "px")
            , ("width", toString obj.width ++ "px")
            , ("height", toString obj.height ++ "px")
            ]
          , onMouseDown (PickUp obj.id)
          , Attr.cursor "move"
          ]
          [ textarea
            [ Html.Attributes.style
              [ ("resize", "none")
              , ("box-sizing", "border-box")
              , ("width", "100%")
              , ("height", "100%")
              , ("margin-top", "0px")
              , ("font-size", "12px")
              , ("font-family", "sans-serif")
              , ("border", "none")
              , ("background", "none")
              ]
            , autofocus True
            , onInput EditText
            ]
            [ text string
            ]
          ]
      _ ->
        text ""

selectedView object =
  let
    corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "5"
        , fill "white", stroke "black" ] []
  in
    g [] <|
    [ case object.objectType of
        Text _ ->
          text ""
        _ ->
          unselectedView object
    ] ++ List.map corner (corners object)
