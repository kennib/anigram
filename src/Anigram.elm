module Anigram exposing (..)

import Html exposing (program, div)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attrs exposing (..)

import Component exposing (..)

import Anigram.Object as Obj exposing (..)
import Anigram.Controls as Ctrl exposing (..)


main =
  program
    <| combine view
       controls2Anigram anigram2Controls
       controls anigram

controls2Anigram msg =
  case msg of
    Ctrl.Fill color -> Just <| Obj.Fill color
    Ctrl.Stroke color -> Just <| Obj.Stroke color
    Ctrl.NewObject createObject -> Just <| Obj.Create createObject
    _ -> Nothing

anigram2Controls msg =
  case msg of
    _ -> Just <| CloseAll

view controls anigram =
  div [Attrs.style "height: 100vh"]
  [ controls
  , anigram
  ]

anigram : Component (List Object) ObjectMsg
anigram =
  { init = (objectsComponent []).init
  , update = update
  , subscriptions = \model -> (objectsComponent <| List.map object model).subscriptions  model
  , view = \model -> (objectsComponent <| List.map object model).view model |> anigramView model
  }

update msg model =
  let
    (newObjects, cmd) = (objectsComponent <| List.map object model).update msg model
    unselectObjects = List.map (\object -> { object | selected = False }) newObjects
    newId = List.length newObjects
  in
    case msg of
      Create createObject ->
        (unselectObjects ++ [createObject newId], cmd)
      _ ->
        (newObjects, cmd)

anigramView objects objectsHtml =
  div
    [ Attrs.style "height: 100vh" ] <|
    List.map textEditView objects ++
    [ svg
      [ width "100%"
      , height "100%"
      ]
      [ objectsHtml
      ]
    ]
