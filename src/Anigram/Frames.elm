module Anigram.Frames exposing (..)

import Dict
import Dict.Extra as Dict
import List.Extra as List

import DragDrop

import Html exposing (..)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects exposing (defaultTextStyle)
import Anigram.Change as Change
import Anigram.Snapping as Snap
import Anigram.Selection as Selection

empty : Frame
empty = Dict.empty

objectIds : List Frame -> List ObjectId
objectIds frames =
  frames
    |> List.map Dict.keys
    |> List.concat
    |> List.unique

setCursorMode mode model = { model | cursorMode = mode }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragSelect start end ->
        let
          objects = getFrameObjects model.frameIndex model.frames model.cursorMode model.objects |> Maybe.withDefault []
          inBounds start end object =
            (  start.x < end.x && object.x > start.x && object.x+object.width < end.x
            || start.x > end.x && object.x < start.x && object.x+object.width > end.x
            ) &&
            (  start.y < end.y && object.y > start.y && object.y+object.height < end.y
            || start.y > end.y && object.y < start.y && object.y+object.height > end.y
            )
          selectInBounds start end objectState object =
            Objects.select (inBounds start end object.style) objectState
        in
          ({ model | objects = List.map2 (selectInBounds start end) model.objects objects, cursorMode = SelectMode }, Cmd.none)
    DragDrop dragDrop ->
      let
        objects = getFrameObjectsWithoutState model.frameIndex model.frames model.objects |> Maybe.withDefault []
        snap = Snap.snapDragDrop objects (Objects.selectedIds model.objects)
      in
        update (Selection <| Move <| DragDrop.delta <| snap dragDrop) model
          |> Tuple.mapFirst (setCursorMode SelectMode)
    DragResize corner dragResize ->
      let
        objects = getFrameObjectsWithoutState model.frameIndex model.frames model.objects |> Maybe.withDefault []
        snap = Snap.snapResize objects (Objects.selectedIds model.objects)
      in
        update (Selection <| uncurry Resize <| Tuple.mapSecond DragDrop.delta <| snap (corner, dragResize)) model
          |> Tuple.mapFirst (setCursorMode SelectMode)
    Selection action ->
      updateChange action model
    Duplicate ->
      mergeAnigram model (Selection.toAnigram model)
          |> update (Selection <| Move { x = 10, y = 10 })
    AddFrame ->
      ( { model
        | frames =
             List.take (model.frameIndex+1) model.frames
          ++ [ Dict.empty ]
          ++ List.drop (model.frameIndex+1) model.frames
        , frameIndex = model.frameIndex+1
        , focus = FrameArea
        }
      , Cmd.none)
    SelectFrame index ->
      ( { model | frameIndex = index, focus = FrameArea }
      , Cmd.none )
    PreviousFrame ->
      ( { model | frameIndex = Basics.max 0 <| model.frameIndex-1 }
      , Cmd.none )
    NextFrame ->
      ( { model | frameIndex = Basics.min (List.length model.frames - 1) <| model.frameIndex+1 }
      , Cmd.none )
    DeleteFrame ->
      ( { model | frames = removeFrame model.frameIndex model.frames }
      , Cmd.none)
    AddObject objectType ->
      ( { model | cursorMode = PlaceObjectMode objectType }
      , Cmd.none)
    PlaceObject objectType position ->
      ( { model
        | objects = model.objects ++
            [ (Objects.newState <| List.length model.objects)
            ]
        , cursorMode = DragResizeMode (Right, Bottom) <| DragDrop.StartDrag position
        }
          |> addChangeToModelAt 0 (ChangeType objectType)
          |> addChangeToModelAt 0 (MoveTo position)
          |> addChangeToModelAt 0 (SizeTo { width = 0, height = 0 })
          |> addChangeToModelAt 0 (Hide True) 
          |> addChangeToModelAt model.frameIndex (Hide False)
      , Cmd.none)
    _ ->
      (model, Cmd.none)

mergeAnigram : Model -> Anigram -> Model
mergeAnigram model anigram =
  let
    oldIds = objectIds anigram.frames
    newIds =
      List.range 0 (List.length oldIds - 1)
      |> List.map (\id -> id + List.length model.objects)
    getNewId oldId =
      List.elemIndex oldId oldIds
        |> Maybe.andThen (\index -> List.getAt index newIds)
        |> Maybe.withDefault -1
    newObjects = List.map Objects.newState newIds
    newFrames = List.map (Dict.mapKeys getNewId) anigram.frames
    unselected = List.map (Objects.select False)
  in
    { model
    | frames = mergeFrames model.frames newFrames
    , objects = unselected model.objects ++ newObjects
    }

mergeFrames : List Frame -> List Frame -> List Frame
mergeFrames baseFrames mergingFrames =
  List.map2
    Dict.union
    baseFrames
    mergingFrames

removeFrame : Int -> List Frame -> List Frame
removeFrame frameIndex frames =
  let
    (start, end) = List.splitAt frameIndex frames
    newEnd =
      case end of
        [] -> []
        [removee] -> []
        removee::nextFrame::end -> combineFrames removee nextFrame :: end
  in
    start ++ newEnd

combineFrames : Frame -> Frame -> Frame
combineFrames frame nextFrame =
  let
    mergeFrame = Dict.insert
    mergeFrames objectId changes nextChanges =
      Dict.insert objectId
        <| reduceObjectChanges
        <| changes ++ nextChanges
  in
    Dict.merge
      mergeFrame
      mergeFrames
      mergeFrame
      frame
      nextFrame
      Dict.empty

updateChange : Change -> Model -> (Model, Cmd Msg)
updateChange change model =
  (addChangeToModel change model, Cmd.none)

addChangeToModelAt : Int -> Change -> Model -> Model
addChangeToModelAt index change model =
  { model | frames =
    List.updateAt index (addChanges (Objects.selectedIds model.objects) change) model.frames
      |> Maybe.withDefault model.frames
      |> reduceChanges (List.map Objects.newStyle model.objects)
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
  frame
  |> Dict.map (\objectId -> reduceObjectChanges)

reduceObjectChanges : List Change -> List Change
reduceObjectChanges changes =
  let
    getLastChange predicate changes =
      List.reverse changes
      |> List.filter predicate
      |> List.take 1
  in
    []
    ++ getLastChange Change.isChangeType changes
    ++ getLastChange Change.isHide changes
    ++ getLastChange Change.isSetText changes
    ++ getLastChange Change.isMoveTo changes
    ++ getLastChange Change.isSizeTo changes
    ++ getLastChange Change.isFill changes
    ++ getLastChange Change.isStroke changes
    ++ getLastChange Change.isTextColor changes
    ++ getLastChange Change.isTextSizeTo changes
    ++ List.filter Change.isMove changes
    ++ List.filter Change.isResize changes

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
    <| List.map (\object -> Objects.unselectedView SelectMode object.id object.style)
    <| objects

getFrameObjects : Int -> List Frame -> CursorMode -> List ObjectState -> Maybe (List Object)
getFrameObjects index frames cursorMode objectStates =
  let
    snap objects = Snap.snapCursor objects (Objects.selectedIds objectStates) cursorMode
    applyCursor objects = Objects.applyCursor (snap objects) objects
  in
    getFrameObjectsWithoutState index frames objectStates
      |> Maybe.map applyCursor

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
    Move delta -> Objects.move delta style
    MoveTo position -> { style | x = position.x, y = position.y }
    Resize corner delta -> Objects.resize corner delta style
    SizeTo size -> { style | width = size.width, height = size.height }
    Fill color -> { style | fill = color }
    Stroke color -> { style | stroke = color }
    TextColor color ->
      case style.objectType of
        Text string textStyle -> { style | objectType = Text string { textStyle | color = color } }
        _ -> { style | objectType = Text "Add text here" { defaultTextStyle | color = color } }
    TextSizeTo size ->
      case style.objectType of
        Text string textStyle -> { style | objectType = Text string { textStyle | size = size } }
        _ -> { style | objectType = Text "Add text here" { defaultTextStyle | size = size } }
    SetText string ->
      case style.objectType of
        Text _ textStyle -> { style | objectType = Text string textStyle }
        _ -> { style | objectType = Text string defaultTextStyle }
