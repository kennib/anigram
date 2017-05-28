module Anigram.Frames exposing (..)

import Dict exposing (..)
import List.Extra as List

import Html exposing (..)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects

empty : Frame
empty = Dict.empty

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Selection action ->
      updateChange action model
    AddFrame ->
      ( { model | frames = model.frames ++ [ Dict.empty ], frameIndex = List.length model.frames }
      , Cmd.none)
    SelectFrame index ->
      ( { model | frameIndex = index }
      , Cmd.none )
    AddObject object ->
      ( { model | objects = model.objects ++ [object] }
      , Cmd.none)
    _ ->
      (model, Cmd.none)

selection objects =
  Objects.selection objects
    |> List.map .id

updateChange : Change -> Model -> (Model, Cmd Msg)
updateChange change model =
  (addChangeToModel change model, Cmd.none)

addChangeToModel : Change -> Model -> Model
addChangeToModel change model =
  { model | frames =
    List.updateAt model.frameIndex (addChanges (selection model.objects) change) model.frames
      |> Maybe.withDefault model.frames
  }

addChanges : List ObjectId -> Change -> Frame -> Frame
addChanges ids change frame =
  List.foldl (\id frame -> addChange id change frame) frame ids

addChange : ObjectId -> Change -> Frame -> Frame
addChange id change frame =
  let
    update changes =
      case changes of
        Just changes -> Just <| changes ++ [change]
        Nothing -> Just [ change ]
  in
    Dict.update id update frame 

view : Model -> Html Msg
view model =
  div
    [ Attr.style
      <| "height: 100vh; width: 200px; background-color: #eee; padding: 20px; overflow-y: scroll;"
      ++ "-webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none;"
    ] <|
    [ button
      [ Svg.Events.onClick AddFrame
      ]
      [ Svg.text "Add Frame"
      ]
    , div
      []
      <| List.indexedMap (viewFrame model)
      <| applyFrames model.frames model.objects
    ]

viewFrame : Model -> Int -> List Object -> Html Msg
viewFrame model index objects =
  svg
    [ width "100%"
    , height "150px"
    , viewBox "0 0 1000 1000"
    , Attr.style <| if model.frameIndex == index then "border: 1px solid red" else "border: 1px solid black"
    , onClick <| SelectFrame index
    ]
    <| List.map Objects.objectView
    <| List.map Objects.noInteraction
    <| objects

getFrameObjects : Int -> List Frame -> List Object -> Maybe (List Object)
getFrameObjects index frames objects =
  List.scanl applyFrame objects frames
  |> List.drop 1
  |> List.getAt index

applyFrames : List Frame -> List Object -> List (List Object)
applyFrames frames objects =
  List.scanl applyFrame objects frames
  |> List.drop 1

applyFrame : Frame  -> List Object -> List Object
applyFrame frame objects =
  let
    objectChange object =
      Dict.get object.id frame
        |> Maybe.map (\changes -> applyChanges changes object)
        |> Maybe.withDefault object
  in
    List.map objectChange objects

applyChanges : List Change -> Object -> Object
applyChanges changes object =
  List.foldl applyChange object changes

applyChange : Change -> Object -> Object
applyChange change object =
  case change of
    SetText string -> { object | objectType = Text string }
    Move delta -> Objects.move object delta
    Fill color -> { object | fill = color }
    Stroke color -> { object | stroke = color }
