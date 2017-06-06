module Anigram exposing (..)

import List.Extra as List

import Mouse
import Keyboard
import Keyboard.Key
import DragDrop

import Html exposing (Html, program, div)
import Html.Attributes exposing (style)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects
import Anigram.Frames as Frames
import Anigram.Controls as Ctrls
import Anigram.Snapping as Snap

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
      [ Ctrls.update
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
    case (dragResizes, dragDrops) of
      (Just (corner, dragDrop), _) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragResize corner <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> uncurry DragResize <| curry snapResize corner <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      (_, Just dragDrop) ->
        Sub.batch
          [ Mouse.moves <| \pos -> DragDrop <| DragDrop.drag dragDrop pos
          , Mouse.ups <| \pos -> DragDrop <| snap <| DragDrop.drop <| DragDrop.drag dragDrop pos
          ]
      _ ->
        Sub.batch
          [ keyboardSubscriptions
          ]

keyboardSubscriptions : Sub Msg
keyboardSubscriptions =
  Keyboard.presses <| \key ->
    case Keyboard.Key.fromCode key of
      Keyboard.Key.Delete -> Selection <| Hide True
      Keyboard.Key.Backspace -> Selection <| Hide True
      Keyboard.Key.Unknown 61 {- Plus -} -> Selection <| Hide False
      _ -> NoOp

view : Model -> Html Msg
view model =
  div
    [ style
      [ ("height", "100vh")
      ]
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
    [ Objects.view
      <| Maybe.withDefault []
      <| Frames.getFrameObjects model.frameIndex model.frames model.objects
    , Frames.view model
    ]
