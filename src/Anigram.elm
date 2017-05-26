module Anigram exposing (..)

import List.Extra as List

import Html exposing (program, div)
import Html.Attributes exposing (style)

import Component exposing (..)

import Anigram.Object as Obj exposing (..)
import Anigram.Frames as Frame exposing (..)
import Anigram.Controls as Ctrl exposing (..)


main =
  program
    <| combine view
       controls2Anigram anigram2Controls
       controls anigram

controls2Anigram msg =
  case msg of
    Ctrl.Fill color -> Just <| Left <| Obj.Fill color
    Ctrl.Stroke color -> Just <| Left <| Obj.Stroke color
    Ctrl.NewObject createObject -> Just <| Left <| Obj.Create createObject
    _ -> Nothing

anigram2Controls msg =
  case msg of
    Left (Obj.NextId id) -> Just <| Ctrl.NextObjectId id
    _ -> Just <| Ctrl.CloseAll

view controls anigram =
  div
  [ style
    [ ("height", "100vh")
    ]
  ]
  [ controls
  , anigram
  ]


anigram =
  combine anigramView
    objects2Frames frames2Objects
    objects frames

objects2Frames msg =
  case msg of
    Obj.Create object -> Just <| Frame.AddObject object
    Obj.Drop pos -> Just <| Frame.AddChange <| move pos
    Obj.Fill color -> Just <| Frame.AddChange <| fill color
    Obj.Stroke color -> Just <| Frame.AddChange <| stroke color
    _ ->
      Nothing

frames2Objects msg =
  case msg of
    Frame.ChangeObjects objects -> Just <| Obj.Set objects
    _ ->
      Nothing

anigramView objects frames =
  div
    [ style
      [ ("display", "flex")
      ]
    ]
    [ objects
    , frames
    ]
