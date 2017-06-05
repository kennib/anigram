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
import Anigram.Snapping as Snap

empty : Frame
empty = Dict.empty

objectIds : List Frame -> List ObjectId
objectIds frames =
  frames
    |> List.map Dict.keys
    |> List.concat
    |> List.unique

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
    AddObject objectType ->
      ( { model | objects = model.objects ++ [Objects.newState <| List.length model.objects] }
          |> addChangeToModelAt 0 (ChangeType objectType)
          |> addChangeToModelAt 0 (Hide True) 
          |> addChangeToModelAt model.frameIndex (Hide False)
      , Cmd.none)
    _ ->
      (model, Cmd.none)

updateChange : Change -> Model -> (Model, Cmd Msg)
updateChange change model =
  (addChangeToModel change model, Cmd.none)

addChangeToModelAt : Int -> Change -> Model -> Model
addChangeToModelAt index change model =
  { model | frames =
    List.updateAt index (addChanges (Objects.selectedIds model.objects) change) model.frames
      |> Maybe.withDefault model.frames
      |> reduceChanges (Objects.currentObjectStyles model.objects)
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

reduceChanges : List ObjectStyle -> List Frame -> List Frame
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
      List.filter Change.isMove changes
      ++ List.filter Change.isResize changes
      ++ getLastChange Change.isChangeType changes
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

removeNonChanges : List ObjectStyle -> Frame -> Frame
removeNonChanges prevObjects frame =
  let
    nonChange prevStyle change =
      applyChange change prevStyle == prevStyle

    filterNonChanges prevStyle changes =
      List.filter (not << nonChange prevStyle) changes

    filterObjectNonChanges prevObject objectId changes =
      if objectId == prevObject.id then
        filterNonChanges prevObject.style changes
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
      <| applyFrames model.frames
      <| List.map (flip ObjectStyle <| Objects.defaultStyle)
      <| List.map .id model.objects
    ]

viewFrame : Model -> Int -> List ObjectStyle -> Html Msg
viewFrame model index objects =
  svg
    [ width "100%"
    , height "150px"
    , viewBox "0 0 1000 1000"
    , Attr.style <| if model.frameIndex == index then "border: 1px solid red" else "border: 1px solid black"
    , onClick <| SelectFrame index
    ]
    <| List.map (\object -> Objects.unselectedView object.id object.style)
    <| objects

getFrameObjects : Int -> List Frame -> List ObjectState -> Maybe (List Object)
getFrameObjects index frames objectStates =
  let
    frameObjects = getFrameObjectsWithoutState index frames objectStates
    snap =
      Snap.snapDragDrop
        (frameObjects |> Maybe.withDefault [])
        (Objects.selectedIds objectStates)
    snapState state =
      { state | dragDrop = snap state.dragDrop }
    applyState : State -> Style -> Style
    applyState state style =
      Objects.applyState (snapState state) style
    applyObjectState : Object -> Object
    applyObjectState object =
      Objects.setStyle (applyState object.state) object
  in
    frameObjects
      |> Maybe.map (List.map applyObjectState)

getFrameObjectsWithoutState : Int -> List Frame -> List ObjectState -> Maybe (List Object)
getFrameObjectsWithoutState index frames objectStates =
  let
    objects = List.map style objectStates
    style object =
      { id = object.id
      , state = object.state
      , style = Objects.defaultStyle
      }
    objectChange frame object = 
      Dict.get object.id frame
        |> Maybe.map (\changes -> Objects.setStyle (applyChanges changes) object)
        |> Maybe.withDefault object
    applyFrame frame objects =
      List.map (objectChange frame) objects
  in
    List.scanl applyFrame objects frames
    |> List.drop 1
    |> List.getAt index

applyFrames : List Frame -> List ObjectStyle -> List (List ObjectStyle)
applyFrames frames objects =
  List.scanl applyFrame objects frames
  |> List.drop 1

applyFrame : Frame -> List ObjectStyle -> List ObjectStyle
applyFrame frame objects =
  let
    objectChange object =
      Dict.get object.id frame
        |> Maybe.map (\changes -> Objects.setStyle (applyChanges changes) object)
        |> Maybe.withDefault object
  in
    List.map objectChange objects

applyChanges : List Change -> Style -> Style
applyChanges changes style =
  List.foldl applyChange style changes

applyChange : Change -> Style -> Style
applyChange change style =
  case change of
    ChangeType objectType -> { style | objectType = objectType }
    Hide state -> { style | hidden = state }
    SetText string -> { style | objectType = Text string }
    Move delta -> Objects.move delta style
    Resize corner delta -> Objects.resize corner delta style
    Fill color -> { style | fill = color }
    Stroke color -> { style | stroke = color }
