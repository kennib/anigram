module Anigram exposing (..)

import List.Extra as List

import Mouse
import DragDrop exposing (..)

import Html exposing (program)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attrs exposing (..)

import Anigram.Object as Obj exposing (Object(..))


type alias Model =
  { objects : List Object
  , dragDrop : DragDrop
  }

type Msg
  = PickupObject Object
  | DragObject Mouse.Position
  | DropObject Mouse.Position


main =
  program
    { init = (model, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


model =
  { objects =
    [ Object (Obj.Shape Obj.Circle) Obj.defaultStyle
    ]
  , dragDrop = DragDrop.empty
  }


update msg model =
  let
    dragDrop = model.dragDrop
  in
    case msg of
      PickupObject object ->
        let
          (Object obj style) = object
          pos = { x = round style.x, y = round style.y }
        in
          ( { model
            | objects = List.remove object model.objects
            , dragDrop = pickup object
            }
          , Cmd.none
          )
      DragObject pos ->
        ( { model
          | dragDrop = drag model.dragDrop pos
          }
        , Cmd.none
        )
      DropObject pos ->
        case drop model.dragDrop of
          Just (object, delta) ->
            ( { model
              | dragDrop = DragDrop.empty
              , objects = moveObject object delta :: model.objects
              }
            , Cmd.none
            )
          _ ->
            (model, Cmd.none) 

moveObject object delta =
  let
    (Object obj style) = object
  in
    Object obj
      { style
      | x = style.x + toFloat delta.x
      , y = style.y + toFloat delta.y
      }


view model =
  let
    config object =
      { mouseDown = PickupObject object
      , cursor = "move"
      }

    objects = 
      case drop model.dragDrop of
        Just (object, delta) ->
           moveObject object delta :: model.objects
        Nothing ->
           model.objects
  in
    svg [width "100%", height "100%"]
      <| List.map (Obj.view config)
      <| objects
 

subscriptions model =
  case draggedItem model.dragDrop of
    Just object ->
      Sub.batch
      [ Mouse.moves DragObject
      , Mouse.ups DropObject
      ]
    Nothing ->
      Sub.none
