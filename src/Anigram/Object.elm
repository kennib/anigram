module Anigram.Object exposing (..)

import List.Extra as List

import Mouse
import Keyboard
import Keyboard.Key
import DragDrop exposing (DragDrop(..))

import Json.Decode as Json

import Html exposing (div, textarea)
import Html.Attributes exposing (attribute, autofocus)
import Html.Events exposing (onInput)
import Html.Events.Extra exposing (onShiftMouseDown)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Path exposing (..)

import Cmd

import Color exposing (Color)
import ColorMath exposing (colorToHex)

import Anigram.Common exposing (..)

defaultObject =
  { objectType = Shape Circle
  , id = -1
  , selected = False
  , dragDrop = Unselected
  , dragResize = ((Left, Top), Unselected)
  , hidden = False
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

newArrow =
  { defaultObject
  | objectType = Arrow
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
    applyDragResize corner dragResize object =
      { object | dragResize = (corner, dragResize), dragDrop = Unselected }
    setSelection selected model =
      { model
      | objects =
            List.map (select False >> applyDragDrop Unselected) model.objects
         |> List.updateIf (\object -> object.id == selected.id) (select True >> applyDragDrop PickedUp)
      }
    addSelection selected model =
      { model | objects = List.updateIf (\object -> object.id == selected.id) (select True >> applyDragDrop PickedUp) model.objects }
    unselectAll model =
      { model | objects = List.map (select False) model.objects }
    select state object =
      { object | selected = state }
  in
    case msg of
      AddObject _ ->
        (mapSelection noInteraction, Cmd.none)
      SelectObject object ->
        (setSelection object model, Cmd.none)
      SelectAddObject object ->
        (addSelection object model, Cmd.none)
      DeselectAll ->
        (unselectAll model, Cmd.none)
      DragDrop dragDrop ->
        (mapSelection (applyDragDrop dragDrop)
        , if DragDrop.isDropped dragDrop then Cmd.message <| Selection <| Move <| DragDrop.delta dragDrop else Cmd.none)
      DragResize corner dragDrop ->
        (mapSelection (applyDragResize corner dragDrop)
        , if DragDrop.isDropped dragDrop then Cmd.message <| Selection <| Resize corner <| DragDrop.delta dragDrop else Cmd.none)
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
      ] <|
      [ rect
        [ onClick DeselectAll
        , fill "#00000000"
        , opacity "0"
        , width "10000"
        , height "10000"
        , stroke "none"
        ]
        []
      ]
      ++ List.map objectView objects
    ]

subscriptions model =
  let
    dragDrops =
      List.map .dragDrop model.objects
      |> List.find DragDrop.isDragged
    dragResizes =
      List.map .dragResize model.objects
      |> List.find (Tuple.second >> DragDrop.isDragged)
  in
    case (dragResizes, dragDrops) of
      (Just (corner, dragDrop), _) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragResize corner <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> DragResize corner <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      (_, Just dragDrop) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragDrop <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> DragDrop <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      _ ->
        Keyboard.presses <| \key ->
          case Keyboard.Key.fromCode key of
            Keyboard.Key.Delete -> Selection <| Hide True
            Keyboard.Key.Backspace -> Selection <| Hide True
            Keyboard.Key.Unknown 61 {- Plus -} -> Selection <| Hide False 
            _ -> NoOp

move object delta =
  { object
  | x = object.x + delta.x
  , y = object.y + delta.y
  }

resize object corner delta =
  let
    width =
      case corner of
        (Left, _) -> object.width - delta.x
        (Right, _) -> object.width + delta.x
    height =
      case corner of
        (_, Top) -> object.height - delta.y
        (_, Bottom) -> object.height + delta.y
    x =
      case corner of
        (Left, _) -> object.x + delta.x
        (Right, _) -> object.x
    y =
      case corner of
        (_, Top) -> object.y + delta.y
        (_, Bottom) -> object.y
  in
    { object
    | width = width
    , height = height
    , x = x
    , y = y
    }

select object =
  { object | selected = True }

drag object pos =
  { object | dragDrop = DragDrop.drag object.dragDrop pos }

drop object =
  let
    delta = DragDrop.delta object.dragDrop
    movedObject = move object delta
  in
    { movedObject | dragDrop = Unselected }

resizeDrop object =
  let
    (corner, dragResize) = object.dragResize
    delta = DragDrop.delta dragResize
    resizedObject = resize object corner delta
  in
    { resizedObject | dragResize = (corner, Unselected) }

applyCurrentChanges object =
  let
    ifDragged property function object =
      if DragDrop.isDragged <| property object then
        function object
      else
        object
  in
    object
      |> ifDragged .dragDrop drop
      |> ifDragged (.dragResize >> Tuple.second) resizeDrop

corners object =
  case object.objectType of
    Arrow ->
      List.map2 (,)
        [(Left, Top), (Right, Bottom)]
        [ { x = object.x, y = object.y }
        , { x = object.x+object.width, y = object.y+object.height }
        ]
    _ ->
      List.map2 (,)
        [(Left, Top), (Right, Top), (Right, Bottom), (Left, Bottom)]
        [ { x = object.x, y = object.y }
        , { x = object.x+object.width, y = object.y }
        , { x = object.x+object.width, y = object.y+object.height }
        , { x = object.x, y = object.y+object.height }
        ]

selection =
  List.filter .selected

noInteraction object =
  { object | selected = False, dragDrop = Unselected }

selectClick object shiftClick =
  if shiftClick then
    SelectAddObject object
  else
    SelectObject object

flip object =
  transform <|
    (if object.width < 0 then "scale(-1 1) translate("++toString (-2*object.x)++" 0)" else "")
    ++
    (if object.height < 0 then "scale(1 -1) translate(0 "++toString (-2*object.y)++")" else "")

objectView object =
  if object.selected then
    object
    |> applyCurrentChanges
    |> selectedView
  else if not object.hidden then
    unselectedView object
  else
    text ""

unselectedView object =
  case object.objectType of
    Shape Circle ->
      ellipse
        [ cx <| toString (object.x + object.width//2)
        , cy <| toString (object.y + object.height//2)
        , rx <| toString <| abs <| object.width//2
        , ry <| toString <| abs <| object.height//2
        , flip object
        , fill <| "#" ++ colorToHex object.fill
        , stroke <| "#" ++ colorToHex object.stroke
        , onShiftMouseDown <| selectClick object
        ]
        []
    Shape Square ->
      rect
        [ x <| toString object.x
        , y <| toString object.y
        , width <| toString <| abs object.width
        , height <| toString <| abs object.height
        , flip object
        , fill <| "#" ++ colorToHex object.fill
        , stroke <| "#" ++ colorToHex object.stroke
        , Attr.cursor "move"
        , onShiftMouseDown <| selectClick object
        ]
        []
    Arrow ->
      let
        linePath =
          subpath
            (startAt (toFloat <| object.x, toFloat <| object.y)) open
            [ lineTo (toFloat <| object.x + abs object.width, toFloat <| object.y + abs object.height)
            ]
        trianglePath = "M0,0 V6 L3,3 Z"
      in
        g
          []
          [ defs
            []
            [ marker
              [ id <| "head-"++toString object.id
              , orient "auto"
              , markerWidth "4"
              , markerHeight "8"
              , refX "2.5"
              , refY "3"
              ]
              [ Svg.path
                [ d <| trianglePath
                , fill <| "#" ++ colorToHex object.stroke
                ]
                []
              ]
            ]
          , Svg.path
            [ d <| pathToString [linePath]
            , flip object
            , attribute "marker-end" <| "url(#head-"++toString object.id++")"
            , stroke <| "#" ++ colorToHex object.stroke
            , fill "none"
            , strokeWidth "3"
            , onShiftMouseDown <| selectClick object
            ]
            [
            ]
          ]
    Text string ->
      text_
        [ x <| toString object.x
        , y <| toString object.y
        , dx "2"
        , dy "12"
        , flip object
        , fontSize "12"
        , fontFamily "sans-serif"
        , Attr.cursor "text"
        , onShiftMouseDown <| selectClick object
        ]
        [text string]

textEditView object =
  let
    obj = object |> applyCurrentChanges
  in
    case (obj.objectType, obj.selected) of
      (Text string, True) ->
        div
          [ Html.Attributes.style
            [ ("position", "absolute")
            , ("left", toString obj.x ++ "px")
            , ("top", toString (obj.y + 40) ++ "px")
            , ("width", toString (abs obj.width) ++ "px")
            , ("height", toString (abs obj.height) ++ "px")
            ]
          , if object.selected then
              onMouseDown (DragDrop <| PickedUp)
            else
              onClick <| SelectObject object
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
    cornerSvg corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "6"
        , fill "white", stroke "black"
        , onMouseDown (DragResize corner <| PickedUp)
        ] []
    cornersSvg =
      List.map (\(corner, pos) -> cornerSvg corner pos) (corners object)
    box =
      rect
        [ x <| toString object.x
        , y <| toString object.y
        , width <| toString <| abs object.width
        , height <| toString <| abs object.height
        , flip object
        , fill "#00000000"
        , opacity "0"
        ]
        []
  in
    g
    [ if object.selected then
        onMouseDown (DragDrop <| PickedUp)
      else
        onClick <| SelectObject object
    ] <|
    [ case object.objectType of
        Text _ ->
          text ""
        _ ->
          if not object.hidden then
            unselectedView object
          else
            faded <| unselectedView object
    , box
    ] ++ cornersSvg

faded svg =
  g
    [ opacity "0.2" ]
    [ svg ]
