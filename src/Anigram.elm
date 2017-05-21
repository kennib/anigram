module Anigram exposing (..)

import List.Extra as List

import Mouse
import DragDrop exposing (..)

import Html exposing (program, div)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attrs exposing (..)

import Color exposing (Color)
import FontAwesome as Icon

import Anigram.Object as Obj exposing (Object(..), ObjectType(..), ShapeType(..))
import Anigram.Controls exposing (..)


type alias Model =
  { objects : List Object
  , cursor : Cursor
  , controls : List (Control Msg)
  }

type Cursor
  = Select
  | Selected Object
  | DragDropObject (DragDrop Object)

type Msg
  = SelectObject Object
  | PickupObject Object
  | DragObject Mouse.Position
  | DropObject Mouse.Position
  | CreateObject Object
  | Fill Color
  | ToggleControl


main =
  program
    { init = (model, Cmd.none)
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


model =
  { objects = []
  , cursor = Select
  , controls = controls
  }

controls =
  [ newControl "Circle" Icon.circle <| CreateObject <| Obj.newShape Circle
  , newControl "Square" Icon.square <| CreateObject <| Obj.newShape Square
  , newControl "Text" Icon.file_text <| CreateObject <| Obj.newText "Add text here"
  , newColorSelector "Fill" Color.green Icon.dot_circle_o Fill ToggleControl
  ]


update msg model =
  case (model.cursor, msg) of
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
            | cursor = Selected <| Obj.moveObject object delta
            }
          , Cmd.none
          )
        _ ->
          (model, Cmd.none)
    (DragDropObject _, _) ->
          (model, Cmd.none)
    (Selected selection, SelectObject object) ->
      ( { model
        | cursor = Selected object
        , objects = selection :: model.objects
        }
      , Cmd.none)
    (_, SelectObject object) ->
      ( { model
        | cursor = Selected object
        }
      , Cmd.none)
    (Selected selection, PickupObject object) ->
      ( { model
        | objects =
          List.remove object model.objects
          ++ if object /= selection then [selection] else []
        , cursor = DragDropObject <| pickup object
        }
      , Cmd.none
      )
    (_, PickupObject object) ->
      ( { model
        | objects = List.remove object model.objects
        , cursor = DragDropObject <| pickup object
        }
      , Cmd.none
      )
    (Selected selection, CreateObject object) ->
      ( { model
        | objects = selection :: model.objects
        , cursor = Selected object
        }
      , Cmd.none
      )
    (_, CreateObject object) ->
      ( { model
        | cursor = Selected object
        }
      , Cmd.none
      )
    (Selected (Object object style), Fill color) ->
      ( { model
        | cursor = Selected <|
          Object object
            { style
            | fill = color
            }
        }
      , Cmd.none)
    _ ->
      (model, Cmd.none)


view model =
  let
    config object =
      { mouseDown = PickupObject object
      , click = SelectObject object
      , cursor = "move"
      }
  in
    div [Attrs.style "height: 100vh"]
      [ controlsView config model
      , svg [width "100%", height "100%"]
        [ objectView config model
        , cursorView config model
        ]
      ]

cursorView config model =
  case model.cursor of
    DragDropObject dragDrop ->
      case drop dragDrop of
        Just (object, delta) ->
           (Obj.view config) <| Obj.moveObject object delta
        Nothing ->
           text ""
    Selected object ->
      Obj.selectedView config object
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
