module Anigram.Object exposing (..)

import List.Extra as List

import DragDrop exposing (DragDrop(..))

import Json.Decode as Json

import Html exposing (Html, div, textarea)
import Html.Attributes exposing (attribute, autofocus)
import Html.Events exposing (onInput, onWithOptions, defaultOptions)
import Html.Events.Extra exposing (onShiftMouseDown, onPositionMouseDown, onPositionMouseUp, onPositionMouseMove)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)
import Svg.Path exposing (..)

import Cmd

import Color exposing (Color)
import ColorMath exposing (colorToHex)

import Vector2 as Vec2 exposing (Vec2)

import Anigram.Common exposing (..)
import Anigram.Selection as Selection

defaultStyle : Style
defaultStyle =
  { objectType = Shape Circle
  , hidden = False
  , x = 50
  , y = 50
  , width = 100
  , height = 100
  , fill = Color.lightBlue
  , stroke = Color.black
  }

newState : ObjectId -> ObjectState 
newState id = 
  { id = id
  , state =
    { selected = True
    , dragDrop = Unselected
    , dragResize = ((Left, Top), Unselected)
    }
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    mapSelection function =
      { model
      | objects = List.updateIf (.state >> .selected) function model.objects
      , focus = ObjectArea
      }
    setSelection id model =
      { model
      | objects =
            List.map (select False >> setDragDrop Unselected) model.objects
         |> List.updateIf (\object -> object.id == id) (select True >> setDragDrop PickedUp)
      , focus = ObjectArea
      }
    addSelection id model =
      { model
      | objects = List.updateIf (\object -> object.id == id) (select True >> setDragDrop PickedUp) model.objects
      , focus = ObjectArea
      }
    selectAll state model =
      { model
      | objects = List.map (select state) model.objects
      , focus = ObjectArea
      , cursorMode = SelectMode
      }
    setCursorMode mode model = { model | cursorMode = mode }
  in
    case msg of
      AddObject _ ->
        (mapSelection noInteraction, Cmd.none)
      SelectObject id ->
        (setSelection id model, Cmd.none)
      SelectAddObject id ->
        (addSelection id model, Cmd.none)
      SelectAll ->
        (selectAll True model, Cmd.none)
      DeselectAll ->
        (selectAll False model, Cmd.none)
      DragDrop dragDrop ->
        (mapSelection (setDragDrop dragDrop) |> setCursorMode (if DragDrop.isDragged dragDrop then DragMode else SelectMode)
        , if DragDrop.isDropped dragDrop then Cmd.message <| Selection <| Move <| DragDrop.delta dragDrop else Cmd.none)
      DragResize corner dragDrop ->
        (mapSelection (setDragResize corner dragDrop) |> setCursorMode (if DragDrop.isDragged dragDrop then DragResizeMode corner else SelectMode)
        , if DragDrop.isDropped dragDrop then Cmd.message <| Selection <| Resize corner <| DragDrop.delta dragDrop else Cmd.none)
      _ ->
        (model, Cmd.none)

view : Model -> List Object -> Html Msg
view model objects =
  div
    [ Attr.style <| "height: 100vh; flex-grow: 1; cursor: "
      ++ (case model.cursorMode of
        DragMode -> "move"
        DragResizeMode corner ->
          case corner of
            (Left , Top   ) -> "nw-resize"
            (Left , Bottom) -> "sw-resize"
            (Right, Top   ) -> "ne-resize"
            (Right, Bottom) -> "se-resize"
        PlaceObjectMode _ -> "crosshair"
        _ -> "default"
      ) ++ ";"
    ]
    [ div
      []
      <| List.map (\object -> textEditView model.cursorMode object.state object.style)
      <| objects
    , svg
      [ width "100%"
      , height "100%"
      ] <|
      [ rect
        ( ( case model.cursorMode of
              PlaceObjectMode objectType ->
                [ onPositionMouseDown <| \pos -> PlaceObject objectType pos ]
              _ ->
                [ onPositionMouseDown <| \pos -> SetCursor <| DragSelectMode <| StartDrag pos ]
          ) ++
          [ fill "#00000000"
          , opacity "0"
          , width "10000"
          , height "10000"
          , stroke "none"
          ]
        )
        []
      ]
      ++ List.map (objectView model.cursorMode) objects ++
      [ case model.cursorMode of
        DragSelectMode dragDrop ->
          case DragDrop.startend dragDrop of
            Just (start, end) ->
              let
                pos =
                  { x = Basics.min start.x end.x
                  , y = Basics.min start.y end.y
                  }
                size =
                  { x = (Basics.max start.x end.x) - pos.x
                  , y = (Basics.max start.y end.y) - pos.y
                  }
              in
                  rect
                    [ fill "blue"
                    , opacity "0.2"
                    , stroke "none"
                    , x <| toString pos.x
                    , y <| toString pos.y
                    , width <| toString size.x
                    , height <| toString size.y
                    ]
                    []
            Nothing ->
              text ""
        _ -> text ""
      ]
    ]

move : Position -> Style -> Style
move delta style =
  { style
  | x = style.x + delta.x
  , y = style.y + delta.y
  }

resize : Corner -> Position -> Style -> Style
resize corner delta style =
  let
    width =
      case corner of
        (Left, _) -> style.width - delta.x
        (Right, _) -> style.width + delta.x
    height =
      case corner of
        (_, Top) -> style.height - delta.y
        (_, Bottom) -> style.height + delta.y
    x =
      case corner of
        (Left, _) -> style.x + delta.x
        (Right, _) -> style.x
    y =
      case corner of
        (_, Top) -> style.y + delta.y
        (_, Bottom) -> style.y
  in
    { style
    | width = width
    , height = height
    , x = x
    , y = y
    }

setStyle set object =
  { object | style = set object.style }

setState set object =
  { object | state = set object.state }

select selectState =
  setState <| \state -> { state | selected = selectState }

setDragDrop dragDropState =
  setState <| \state -> { state | dragDrop = dragDropState }

setDragResize corner dragDropState =
  setState <| \state -> { state | dragResize = (corner, dragDropState) }

drag object pos =
  { object | dragDrop = DragDrop.drag object.dragDrop pos }

drop : State -> Style -> Style
drop state =
  move <| DragDrop.delta state.dragDrop

resizeDrop : State -> Style -> Style
resizeDrop state =
  let
    (corner, dragResize) = state.dragResize
    delta = DragDrop.delta dragResize
  in
    resize corner delta

applyState : State -> Style -> Style
applyState state style =
  let
    doDrop style =
      if DragDrop.isDragged state.dragDrop then
        drop state style
      else
        style
    doResizeDrop style =
      if DragDrop.isDragged <| Tuple.second state.dragResize then
        resizeDrop state style
      else
        style
  in
    style
      |> doDrop
      |> doResizeDrop

currentObjectStyles : List ObjectState -> List ObjectStyle
currentObjectStyles objects =
  objects
    |> List.map (\object ->
      { id = object.id
      , style = applyState object.state defaultStyle
      })

corners object =
  case object.objectType of
    Arrow ->
      List.map2 (,)
        [(Left, Top), (Right, Bottom)]
        [ { x = object.x, y = object.y }
        , { x = object.x+object.width, y = object.y+object.height }
        ]
    ArcArrow _ ->
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

selectedIds : List ObjectState -> List ObjectId
selectedIds =
  List.filter (.state >> .selected) >> List.map .id

noInteraction =
  setState <| \state -> { state | selected = False, dragDrop = Unselected, dragResize = ((Left, Top), Unselected) }

selectClick id shiftClick =
  if shiftClick then
    SelectAddObject id 
  else
    SelectObject id

onCursor cursorMode objectId =
  case cursorMode of
    SelectMode ->
      onShiftMouseDown <| selectClick objectId
    PlaceObjectMode objectType ->
      onPositionMouseDown <| PlaceObject objectType
    _ ->
      attribute "none" ""

flip object =
  transform <|
    (if object.width < 0 then "scale(-1 1) translate("++toString (-2*object.x)++" 0)" else "")
    ++
    (if object.height < 0 then "scale(1 -1) translate(0 "++toString (-2*object.y)++")" else "")

objectView : CursorMode -> Object -> Html Msg
objectView cursorMode object =
  if object.state.selected then
    selectedView cursorMode object.id object.style
  else
    unselectedView cursorMode object.id object.style

unselectedView : CursorMode -> ObjectId -> Style -> Html Msg
unselectedView cursorMode objectId object =
  if not object.hidden then
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
          , onCursor cursorMode objectId
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
          , onCursor cursorMode objectId
          ]
          []
      Arrow ->
        arrowView cursorMode objectId object
          <| \startVec lengthVec ->
            [ lineTo (startVec |> Vec2.add lengthVec)
            ]
      ArcArrow radius ->
        let
          orthogonal (x, y) = (y, -x)
          orthogonalVec lengthVec = lengthVec |> orthogonal |> Vec2.normalize |> Vec2.scale radius
        in
          arrowView cursorMode objectId object
            <| \startVec lengthVec ->
              [ quadraticTo
                (startVec |> Vec2.add (lengthVec |> Vec2.divideBy 2) |> Vec2.add (orthogonalVec lengthVec))
                (startVec |> Vec2.add lengthVec)
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
          , onCursor cursorMode objectId
          ]
          [text string]
  else
    text ""

arrowView : CursorMode -> ObjectId -> Style -> (Vec2.Float2 -> Vec2.Float2 -> List Instruction) -> Svg Msg
arrowView cursorMode objectId object path =
  let
    lengthVec = (toFloat <| abs object.width, toFloat <| abs object.height)
    startVec = (toFloat object.x, toFloat object.y)
    linePath = subpath (startAt startVec) open <| path startVec lengthVec
    trianglePath = "M0,0 V6 L3,3 Z"
  in
    g
      []
      [ defs
        []
        [ marker
          [ id <| "head-"++toString objectId
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
        , attribute "marker-end" <| "url(#head-"++toString objectId++")"
        , stroke <| "#" ++ colorToHex object.stroke
        , fill "none"
        , strokeWidth "3"
        , onCursor cursorMode objectId
        ]
        [
        ]
      ]

textEditView : CursorMode -> State -> Style -> Html Msg
textEditView cursorMode state object =
  case (object.objectType, state.selected) of
    (Text string, True) ->
      div
        [ Html.Attributes.style
          [ ("position", "absolute")
          , ("left", toString object.x ++ "px")
          , ("top", toString (object.y + 40) ++ "px")
          , ("width", toString (abs object.width) ++ "px")
          , ("height", toString (abs object.height) ++ "px")
          ]
        , onMouseDown <| if cursorMode == SelectMode then (DragDrop <| PickedUp) else NoOp
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
            , ("opacity", if object.hidden then "0.2" else "1")
            ]
          , autofocus True
          , Html.Attributes.attribute "onfocus" "this.select()" -- This is kind of cheating, but is the least nasty of several options
          , onInput (Selection << SetText)
          , onWithOptions "keydown" { defaultOptions | stopPropagation = True } <| Json.succeed NoOp -- prevent Delete/Ctrl A/etc from affecting the rest of the Anigram
          , onWithOptions "mousedown" { defaultOptions | stopPropagation = True } <| Json.succeed NoOp -- prevent mouse selection from dragging the text box
          ]
          [ text string
          ]
        ]
    _ ->
      text ""

selectedView : CursorMode -> ObjectId -> Style -> Html Msg
selectedView cursorMode id object =
  let
    cornerSvg corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "6"
        , fill "white", stroke "black"
        , onMouseDown <| if cursorMode == SelectMode then (DragResize corner <| PickedUp) else NoOp
        ] []
    cornersSvg =
      List.map (\(corner, pos) -> cornerSvg corner pos) (corners object)
    box =
      rect
        [ x <| toString <| object.x - 10
        , y <| toString <| object.y - 10
        , width <| toString <| abs object.width + 20
        , height <| toString <| abs object.height + 20
        , flip object
        , fill "#00000000"
        , opacity "0"
        , onMouseDown <| if cursorMode == SelectMode then (DragDrop <| PickedUp) else NoOp
        ]
        []
  in
    g
    [
    ] <|
    [ case object.objectType of
        Text _ ->
          text ""
        _ ->
          if not object.hidden then
            unselectedView cursorMode id object
          else
            faded <| unselectedView cursorMode id object
    , box
    ] ++ cornersSvg

faded svg =
  g
    [ opacity "0.2" ]
    [ svg ]
