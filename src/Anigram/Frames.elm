module Anigram.Frames exposing (..)

import Dict exposing (..)
import List.Extra as List

import Mouse

import Html exposing (..)
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes as Attr exposing (..)

import Cmd exposing (..)
import Component exposing (..)

import Color

import Anigram.Object as Obj exposing (..)

type alias Frame =
  Dict ObjectId (List Change)

type alias Change =
  { value : Value
  , change : Value -> Object -> Object
  }

type Value
  = Text String
  | Number Int
  | Coords Mouse.Position
  | Color Color.Color

type alias FrameModel =
  { frames : List Frame
  , currentFrame : Int
  , objects : List Object
  }

type FrameMsg
  = AddChange Change
  | AddFrame
  | DeleteFrame Int
  | ChangeFrame Int
  | AddObject Object
  | ChangeObjects (List Object)
  | NoOp


frames : Component FrameModel FrameMsg
frames =
  { init = (model, Cmd.none)
  , update = updateFrames
  , subscriptions = \_ -> Sub.none
  , view = view
  }

model : FrameModel
model =
  { frames = [ Dict.empty ]
  , currentFrame = 0
  , objects = []
  }

updateFrames : FrameMsg -> FrameModel -> (FrameModel, Cmd FrameMsg)
updateFrames msg model =
  case msg of
    AddChange change ->
      let
        selection =
          model.objects
            |> List.filter .selected
            |> List.map .id
      in
        ( { model | frames =
            List.updateAt model.currentFrame (addChanges selection change) model.frames
              |> Maybe.withDefault model.frames
          }
        , Cmd.none)
    AddFrame ->
      ( { model | frames = model.frames ++ [ Dict.empty ], currentFrame = List.length model.frames }
      , Cmd.none)
    DeleteFrame index ->
      ( { model | frames = List.removeAt index model.frames
        , currentFrame = if model.currentFrame < index then model.currentFrame else model.currentFrame - 1 }
      , Cmd.none)
    ChangeFrame index ->
      let
        newObjects = getFrameObjects index model.frames model.objects
        cmd =
          case newObjects of
            Just objects -> Cmd.message <| ChangeObjects objects
            Nothing -> Cmd.none
      in
        ( { model | currentFrame = index }
        , cmd )
    AddObject object ->
      ( { model | objects = model.objects ++ [object] }
      , Cmd.none)
    _ ->
      (model, Cmd.none)

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


view : FrameModel -> Html FrameMsg
view model =
  div
    [ Attr.style "height: 100vh; width: 200px; background-color: #eee; padding: 20px;"
    ] <|
    [ button
      [ Svg.Events.onClick AddFrame
      ]
      [ Svg.text "Add Frame"
      ]
    ] ++
    ( List.indexedMap viewFrame
    <| applyFrames model.frames model.objects
    )

viewFrame : Int -> List Object -> Html FrameMsg
viewFrame index objects =
  svg
    [ width "100%"
    , height "150px"
    , viewBox "0 0 1000 1000"
    , Attr.style "border: 1px solid black"
    , onClick <| ChangeFrame index
    ]
    <| List.map (Html.map (\_ -> NoOp))
    <| List.map Obj.view
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
  change.change change.value object

move : Mouse.Position -> Change
move delta =
  { value = Coords delta
  , change = \value object ->
    case value of
      Coords delta -> moveObject { object | x = 0, y = 0 } delta
      _ -> object
  }

fill : Color.Color -> Change
fill color =
  { value = Color color
  , change = \value object ->
    case value of
      Color color -> { object | fill = color }
      _ -> object
  }

stroke : Color.Color -> Change
stroke color =
  { value = Color color
  , change = \value object ->
    case value of
      Color color -> { object | stroke = color }
      _ -> object
  }
