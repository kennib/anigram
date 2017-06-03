module Anigram.Frames exposing (..)

import Dict
import List.Extra as List

import Html exposing (..)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects
import Anigram.Change as Change

empty : Frame
empty = Dict.empty

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Selection action ->
      updateChange action model
    AddFrame ->
      ( { model
        | frames =
             List.take (model.frameIndex+1) model.frames
          ++ [ Dict.empty ]
          ++ List.drop (model.frameIndex+1) model.frames
        , frameIndex = model.frameIndex+1
        }
      , Cmd.none)
    SelectFrame index ->
      ( { model | frameIndex = index }
      , Cmd.none )
    AddObject object ->
      ( { model | objects = model.objects ++ [object] }
        |> addChangeToModelAt 0 (Hide True) 
        |> addChangeToModelAt model.frameIndex (Hide False)
      , Cmd.none)
    _ ->
      (model, Cmd.none)

selection objects =
  Objects.selection objects
    |> List.map .id

updateChange : Change -> Model -> (Model, Cmd Msg)
updateChange change model =
  (addChangeToModel change model, Cmd.none)

addChangeToModelAt : Int -> Change -> Model -> Model
addChangeToModelAt index change model =
  { model | frames =
    List.updateAt index (addChanges (selection model.objects) change) model.frames
      |> Maybe.withDefault model.frames
      |> reduceChanges model.objects
  }

addChangeToModel : Change -> Model -> Model
addChangeToModel change model =
  addChangeToModelAt model.frameIndex change model

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

reduceChanges : List Object -> List Frame -> List Frame
reduceChanges objects frames =
  let
    prevFrameObjects = List.scanl applyFrame objects frames
  in
    frames
    |> List.map reduceFrameChanges
    |> List.map2 removeNonChanges prevFrameObjects

reduceFrameChanges : Frame -> Frame
reduceFrameChanges frame =
  let
    reduceObjectChanges objectId changes =
      (changes |> List.filterNot Change.isHide |> List.filterNot Change.isSetText |> List.filterNot Change.isFill |> List.filterNot Change.isStroke)
      ++ getLastChange Change.isHide changes
      ++ getLastChange Change.isSetText changes
      ++ getLastChange Change.isFill changes
      ++ getLastChange Change.isStroke changes

    getLastChange predicate changes =
      List.reverse changes
      |> List.filter predicate
      |> List.take 1
  in
    frame
    |> Dict.map reduceObjectChanges

removeNonChanges : List Object -> Frame -> Frame
removeNonChanges prevObjects frame =
  let
    nonChange prevObject change =
      applyChange change prevObject == prevObject

    filterNonChanges prevObject changes =
      List.filter (not << nonChange prevObject) changes

    filterObjectNonChanges prevObject objectId changes =
      if objectId == prevObject.id then
        filterNonChanges prevObject changes
      else
        changes

    filterFrameNonChanges prevObject frame =
      frame
      |> Dict.map (filterObjectNonChanges prevObject)
  in
    List.foldl filterFrameNonChanges frame prevObjects

view : Model -> Html Msg
view model =
  div
    [ Attr.style
      <| "height: 100vh; width: 200px; background-color: #eee; padding: 20px; overflow-y: scroll;"
      ++ "-webkit-touch-callout: none; -webkit-user-select: none; -khtml-user-select: none; -moz-user-select: none; -ms-user-select: none; user-select: none;"
    ] <|
    [ div
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
    Hide state -> { object | hidden = state }
    SetText string -> { object | objectType = Text string }
    Move delta -> Objects.move object delta
    Resize corner delta -> Objects.resize object corner delta
    Fill color -> { object | fill = color }
    Stroke color -> { object | stroke = color }
