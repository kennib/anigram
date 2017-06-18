module Anigram.Object exposing (..)

import List.Extra as List

import DragDrop exposing (DragDrop(..))

import Json.Decode as Json

import Html exposing (Html, div, textarea)
import Html.Attributes exposing (attribute, autofocus)
import Html.Events exposing (onInput, onWithOptions, defaultOptions)
import Html.Events.Extra exposing (onShiftPositionMouseDown, onShiftMouseDown, onPositionMouseDown, onPositionMouseUp, onPositionMouseMove)
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
import Anigram.Shapes as Shapes

defaultStyle : Style
defaultStyle =
  { objectType = Placeholder
  , hidden = False
  , x = 50
  , y = 50
  , width = 100
  , height = 100
  , fill = Color.lightBlue
  , stroke = Color.black
  , strokeWidth = 3
  }

defaultTextStyle : TextStyle
defaultTextStyle =
  { size = 16
  , font = "sans-serif"
  , color = Color.black
  }

newState : ObjectId -> ObjectState
newState id =
  { id = id
  , state =
    { selected = True
    }
  }

newStyle : ObjectState -> ObjectStyle
newStyle object =
  { id = object.id
  , style = defaultStyle
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    setSelection id model =
      { model
      | objects =
            List.map (select False) model.objects
         |> List.updateIf (\object -> object.id == id) (select True)
      , focus = ObjectArea
      }
    addSelection id model =
      { model
      | objects = List.updateIf (\object -> object.id == id) (select True) model.objects
      , focus = ObjectArea
      }
    selectAll state model =
      { model
      | objects = List.map (select state) model.objects
      , focus = ObjectArea
      , cursorMode = SelectMode
      }
  in
    case msg of
      AddObject _ ->
        (selectAll False model, Cmd.none)
      SelectObject id ->
        (setSelection id model, Cmd.none)
      SelectAddObject id ->
        (addSelection id model, Cmd.none)
      SelectDragDrop id dragDrop ->
        (setSelection id { model | cursorMode = DragMode dragDrop}, Cmd.none)
      SelectAll ->
        (selectAll True model, Cmd.none)
      DeselectAll ->
        (selectAll False model, Cmd.none)
      _ ->
        (model, Cmd.none)

view : Model -> List Object -> Html Msg
view model objects =
  div
    [ Attr.style <| "height: 100vh; flex-grow: 1; cursor: "
      ++ (case model.cursorMode of
        DragMode _ -> "move"
        DragSizeMode _ corner _ ->
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
      [ Html.Attributes.style
        [ ("position", "absolute")
        ]
      ]
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

drop : DragDrop.DragDrop -> Style -> Style
drop dragDrop =
  move <| DragDrop.delta dragDrop

resizeDrop : Corner -> DragDrop.DragDrop -> Style -> Style
resizeDrop corner dragResize =
  resize corner <| DragDrop.delta dragResize

applyCursorMode : CursorMode -> Style -> Style
applyCursorMode mode =
  case mode of
    DragMode dragDrop -> drop dragDrop
    DragSizeMode _ corner dragResize -> resizeDrop corner dragResize
    _ -> identity

applyCursor : CursorMode -> List Object -> List Object
applyCursor cursorMode objects =
  objects
    |> List.updateIf (.state >> .selected) (setStyle <| applyCursorMode cursorMode)

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

selectClick id shiftClick pos =
  if shiftClick then
    SelectAddObject id
  else
    SelectDragDrop id <| StartDrag pos

onCursor cursorMode objectId =
  case cursorMode of
    SelectMode ->
      onShiftPositionMouseDown <| selectClick objectId
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
      Placeholder -> text ""
      Shape Circle ->
        ellipse
          [ cx <| toString (object.x + (abs <| object.width//2))
          , cy <| toString (object.y + (abs <| object.height//2))
          , rx <| toString <| abs <| object.width//2
          , ry <| toString <| abs <| object.height//2
          , flip object
          , fill <| "#" ++ colorToHex object.fill
          , stroke <| "#" ++ colorToHex object.stroke
          , strokeWidth <| toString object.strokeWidth
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
          , strokeWidth <| toString object.strokeWidth
          , onCursor cursorMode objectId
          ]
          []
      Shape Star ->
        Svg.g
          [ x <| toString object.x
          , y <| toString object.y
          , width <| toString <| abs object.width
          , height <| toString <| abs object.height
          , flip object
          , onCursor cursorMode objectId
          ]
          [ Svg.path 
            [ d <| Shapes.toPath <| Shapes.star object.x object.y object.width object.height 5
            , fill <| "#" ++ colorToHex object.fill
            , stroke <| "#" ++ colorToHex object.stroke
            , strokeWidth <| toString object.strokeWidth
            ]  
            []
          ]
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
      Text string style ->
        foreignObject
          [ x <| toString object.x
          , y <| toString object.y
          , width <| toString <| abs <| object.width
          , height <| toString <| abs <| object.height
          , flip object
          , onCursor cursorMode objectId
          ]
          [ Html.textarea
              [ Html.Attributes.style
                <| textStyle style
              , Html.Attributes.readonly True
              ]
              [ text string ]
          ]
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
        , strokeWidth <| toString object.strokeWidth
        , onCursor cursorMode objectId
        ]
        [
        ]
      ]

textEditView : CursorMode -> State -> Style -> Html Msg
textEditView cursorMode state object =
  case (object.objectType, state.selected) of
    (Text string style, True) ->
      div
        [ Html.Attributes.style
          [ ("position", "absolute")
          , ("left", toString object.x ++ "px")
          , ("top", toString object.y ++ "px")
          , ("width", toString (abs object.width) ++ "px")
          , ("height", toString (abs object.height) ++ "px")
          ]
        , onPositionMouseDown <| if cursorMode == SelectMode then SetCursor << DragMode << StartDrag else \_ -> NoOp
        , Attr.cursor "move"
        ]
        [ textarea
          [ Html.Attributes.style
            <| (::) ("opacity", if object.hidden then "0.2" else "1")
            <| textStyle style
          , Html.Attributes.readonly <|
              case cursorMode of
                DragMode (Drag _ _) -> True
                _ -> False
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

textStyle : TextStyle -> List (String, String)
textStyle style =
  [ ("resize", "none")
  , ("box-sizing", "border-box")
  , ("width", "100%")
  , ("height", "100%")
  , ("margin-top", "0px")
  , ("padding", "2px")
  , ("line-height", "1.0")
  , ("font-size", toString style.size++ "px")
  , ("font-family", style.font)
  , ("color", "#" ++ colorToHex style.color)
  , ("border", "none")
  , ("background", "none")
  ]

selectedView : CursorMode -> ObjectId -> Style -> Html Msg
selectedView cursorMode id object =
  let
    cornerSvg corner pos =
      circle
        [ cx <| toString pos.x, cy <| toString pos.y, r "6"
        , fill "white", stroke "black"
        , onPositionMouseDown <| if cursorMode == SelectMode then SetCursor << DragSizeMode False corner << StartDrag else \_ -> NoOp
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
        , onPositionMouseDown <| if cursorMode == SelectMode then SetCursor << DragMode << StartDrag else \_ -> NoOp
        ]
        []
  in
    g
    [
    ] <|
    [ case object.objectType of
        Text _ _ ->
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
