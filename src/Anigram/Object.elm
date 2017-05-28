module Anigram.Object exposing (..)

import List.Extra as List

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

import Color exposing (Color)
import ColorMath exposing (colorToHex)

import Anigram.Common exposing (..)

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

newShape shape =
  { defaultObject
  | objectType = Shape shape
  }

newText text =
  { defaultObject
  | objectType = Text text
  }

update msg model =
  let
    mapSelection function =
      { model | objects = List.updateIf .selected function model.objects }
    applyDragDrop dragDrop object =
      { object | dragDrop = dragDrop }
  in
    case msg of
      DragDrop dragDrop ->
        (mapSelection (applyDragDrop dragDrop)
        , if DragDrop.isDropped dragDrop then Cmd.message <| Selection <| Move <| DragDrop.delta dragDrop else Cmd.none)
      _ ->
        (model, Cmd.none)

view objects =
  div
    [ Attr.style "height: 100vh; flex-grow: 1;" ]
    [ div
      []
      <| List.map textEditView objects
    , svg
      [ width "100%"
      , height "100%"
      ]
      <| List.map objectView objects
    ]

subscriptions model =
  Sub.batch
    <| List.map objectSubscriptions model.objects

objectSubscriptions object =
  if DragDrop.isDragged object.dragDrop then
    Sub.batch
      [ Mouse.moves <| \pos -> DragDrop <| DragDrop.drag object.dragDrop pos
      , Mouse.ups <| \pos -> DragDrop <| DragDrop.drop <| DragDrop.drag object.dragDrop pos
      ]
  else
    Sub.none

move object delta =
  { object
  | x = object.x + delta.x
  , y = object.y + delta.y
  }

drag object pos =
  { object | dragDrop = DragDrop.drag object.dragDrop pos }

drop object =
  let
    delta = DragDrop.delta object.dragDrop
    movedObject = move object delta
  in
    { movedObject | dragDrop = Unselected }

corners object =
  [ { x = object.x, y = object.y }
  , { x = object.x+object.width, y = object.y }
  , { x = object.x+object.width, y = object.y+object.height }
  , { x = object.x, y = object.y+object.height }
  ]

selection =
  List.filter .selected

noInteraction object =
  { object | selected = False, dragDrop = Unselected }

objectView object =
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
        [ cx <| toString (object.x + object.width//2)
        , cy <| toString (object.y + object.width//2)
        , r <| toString <| object.width//2
        , fill <| "#" ++ colorToHex object.fill
        , stroke <| "#" ++ colorToHex object.stroke
        , onMouseDown (DragDrop <| PickedUp)
        , onClick (SelectObject object)
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
        , onMouseDown (DragDrop <| PickedUp)
        , onClick (SelectObject object)
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
        , onMouseDown (DragDrop <| PickedUp)
        , onClick (SelectObject object)
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
          , onMouseDown (DragDrop <| PickedUp)
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
            , onInput (Selection << SetText)
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
