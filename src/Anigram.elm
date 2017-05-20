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
  , cursor : Cursor
  }

type Cursor
  = Select
  | DragDropObject (DragDrop Object)

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
  , cursor = Select
  }


update msg model =
  case (model.cursor, msg) of
    (Select, PickupObject object) ->
      let
        (Object obj style) = object
        pos = { x = round style.x, y = round style.y }
      in
        ( { model
          | objects = List.remove object model.objects
          , cursor = DragDropObject <| pickup object
          }
        , Cmd.none
        )
    (DragDropObject dragDrop, DragObject pos) ->
      ( { model
        | cursor = DragDropObject <| drag dragDrop pos
        }
      , Cmd.none
      )
    (DragDropObject dragDrop, DropObject pos) ->
      case drop dragDrop of
        Just (object, delta) ->
          ( { model
            | cursor = Select 
            , objects = moveObject object delta :: model.objects
            }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none) 
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
  in
    svg [width "100%", height "100%"]
      [ objectView config model
      , cursorView config model
      ]

cursorView config model =
  case model.cursor of
    DragDropObject dragDrop ->
      case drop dragDrop of
        Just (object, delta) ->
           (Obj.view config) <| moveObject object delta
        Nothing ->
           text ""
    _ ->
      text "" 

objectView config model =
  g [] <| List.map (Obj.view config) model.objects
 

subscriptions model =
  case model.cursor of
    (DragDropObject dragDrop) ->
      case draggedItem dragDrop of
        Just object ->
          Sub.batch
          [ Mouse.moves DragObject
          , Mouse.ups DropObject
          ]
        Nothing ->
          Sub.none
    _ ->
      Sub.none
