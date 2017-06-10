module Anigram exposing (..)

import List.Extra as List

import Mouse
import DragDrop
import Keyboard.Key exposing (Key)

import Html exposing (Html, program, div)
import Html.Attributes exposing (style, tabindex)
import Html.Events.Extra exposing (onComboKeyDown)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects
import Anigram.Frames as Frames
import Anigram.Controls as Ctrls
import Anigram.Snapping as Snap
import Anigram.History as History

main =
  program
    { init = (model, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

model : Model
model =
  { objects = []
  , frames = [ Frames.empty ]
  , frameIndex = 0
  , controls = Ctrls.model
  , focus = ObjectArea
  , cursorMode = SelectMode
  , history = { past = [], future = [] }
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    mergeUpdate nextUpdate (model, cmds) =
      nextUpdate msg model
        |> Tuple.mapSecond (\newCmd -> newCmd :: cmds)

    batchUpdate updates =
      List.foldl mergeUpdate (model, []) updates
        |> Tuple.mapSecond Cmd.batch
  in
    batchUpdate
      [ History.update
      , Ctrls.update
      , Objects.update
      , Frames.update
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    dragDrops =
      List.map (.state >> .dragDrop) model.objects
        |> List.find DragDrop.isDragged
    dragResizes =
      List.map (.state >> .dragResize) model.objects
        |> List.find (Tuple.second >> DragDrop.isDragged)
    snap =
      Snap.snapDragDrop
        (Frames.getFrameObjectsWithoutState model.frameIndex model.frames model.objects |> Maybe.withDefault [])
        (Objects.selectedIds model.objects)
    snapResize =
      Snap.snapResize
        (Frames.getFrameObjectsWithoutState model.frameIndex model.frames model.objects |> Maybe.withDefault [])
        (Objects.selectedIds model.objects)
  in
    case (model.cursorMode, dragResizes, dragDrops) of
      (DragSelectMode dragDrop, _, _) ->
        Sub.batch
          [ Mouse.moves <| \pos -> SetCursor <| DragSelectMode <| DragDrop.drag dragDrop pos
          , Mouse.ups   <| \pos -> Maybe.withDefault NoOp <| Maybe.map (uncurry DragSelect) <| DragDrop.startend <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      (_, Just (corner, dragDrop), _) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragResize corner <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> uncurry DragResize <| curry snapResize corner <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      (_, _, Just dragDrop) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragDrop <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> DragDrop <| DragDrop.mapDropped snap <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      _ ->
        Sub.none

keyboardCombo : Model -> (Bool, Bool, Key) -> Msg
keyboardCombo model (ctrl, shift, key) =
  case model.focus of
    ObjectArea ->
      case (ctrl, shift, key) of
        (True, False, Keyboard.Key.Z) -> Undo
        (True, True, Keyboard.Key.Z) -> Redo
        (True, False, Keyboard.Key.Y) -> Redo
        (True, False, Keyboard.Key.A) -> SelectAll
        (True, False, Keyboard.Key.D) -> DeselectAll
        (_, _, Keyboard.Key.Escape) -> DeselectAll
        (_, _, Keyboard.Key.Delete) -> Selection <| Hide True
        (_, _, Keyboard.Key.Backspace) -> Selection <| Hide True
        (_, _, Keyboard.Key.Unknown 61 {- Plus -}) -> Selection <| Hide False
        (_, _, Keyboard.Key.Left)  -> Selection <| Move { x = -1, y =  0 }
        (_, _, Keyboard.Key.Right) -> Selection <| Move { x =  1, y =  0 }
        (_, _, Keyboard.Key.Up)    -> Selection <| Move { x =  0, y = -1 }
        (_, _, Keyboard.Key.Down)  -> Selection <| Move { x =  0, y =  1 }
        _ -> NoOp
    FrameArea ->
      case (ctrl, shift, key) of
        (True, False, Keyboard.Key.Z) -> Undo
        (True, True, Keyboard.Key.Z) -> Redo
        (True, False, Keyboard.Key.Y) -> Redo
        (_, _, Keyboard.Key.Up)    -> PreviousFrame
        (_, _, Keyboard.Key.Down)  -> NextFrame
        (_, _, Keyboard.Key.Delete) -> DeleteFrame
        (_, _, Keyboard.Key.Backspace) -> DeleteFrame
        _ -> NoOp

view : Model -> Html Msg
view model =
  div
    [ style
      [ ("height", "100vh")
      , ("overflow", "hidden")
      ]
    , tabindex 1 -- Allows us to capture keyboard events
    , onComboKeyDown (keyboardCombo model)
    ]
    [ Ctrls.view model
    , anigramView model
    ]

anigramView : Model -> Html Msg
anigramView model =
  div
    [ style
      [ ("display", "flex")
      ]
    ]
    [ Objects.view model
      <| Maybe.withDefault []
      <| Frames.getFrameObjects model.frameIndex model.frames model.objects
    , Frames.view model
    ]
