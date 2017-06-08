module Anigram.Snapping exposing (..)

import List
import List.Extra as List

import DragDrop exposing (DragDrop)

import Anigram.Common exposing (..)
import Anigram.Object as Object

snapResize  : List Object -> List ObjectId -> (Corner, DragDrop) -> (Corner, DragDrop)
snapResize objects snappingObjects (corner, dragDrop) =
  let
    otherObjects = List.filterNot (\obj -> List.member obj.id snappingObjects) objects
    selected = List.filter (\obj -> List.member obj.id snappingObjects) objects
      |> List.map (\object -> Object.setStyle (Object.applyState object.state) object)
    snapped = snap (snapLines <| List.map .style otherObjects) (List.andThen (cornerSnapLines corner) <| List.map .style selected)
  in
    (corner, DragDrop.mapEnd snapped dragDrop)

snapDragDrop : List Object -> List ObjectId -> DragDrop -> DragDrop
snapDragDrop objects snappingObjects =
  let
    otherObjects = List.filterNot (\obj -> List.member obj.id snappingObjects) objects
    selected = List.filter (\obj -> List.member obj.id snappingObjects) objects
      |> List.map (\object -> Object.setStyle (Object.applyState object.state) object)
    snapped = snap (snapLines <| List.map .style otherObjects) (snapLines <| List.map .style selected)
  in
    DragDrop.mapEnd snapped

snap : List SnapLine -> List SnapLine -> Position -> Position
snap snapLines snappedLines pos =
  let
    dist = 10
    snaps =
      List.lift2 (,)
        snappedLines 
        snapLines
      |> List.filter (uncurry <| lineSnaps dist)
    distanceBetween snap =
        snap
        |> Maybe.map (\(snapped, snap) -> snapPosition snapped - snapPosition snap)
        |> Maybe.withDefault 0
    horizontal =
       snaps
        |> List.find (Tuple.first >> isHorizontal)
        |> distanceBetween
    vertical =
      snaps
        |> List.find (Tuple.first >> isVertical)
        |> distanceBetween
  in
    { pos | x = pos.x - horizontal, y = pos.y - vertical }
  
snapLines : List Style -> List SnapLine
snapLines objects =
  List.concat
    <| List.map objectSnapLines objects

cornerSnapLines : Corner -> Style -> List SnapLine
cornerSnapLines (xSide, ySide) object =
  let
    horizontal = case xSide of
      Left -> [ HorizontalSnap object.x ]
      Right -> [ HorizontalSnap <| object.x + object.width ]
    vertical = case ySide of
      Top -> [ VerticalSnap object.y ]
      Bottom -> [ VerticalSnap <| object.y + object.height ]
   in
    horizontal ++ vertical

objectSnapLines : Style -> List SnapLine
objectSnapLines object =
  [ HorizontalSnap object.x
  , VerticalSnap object.y
  , HorizontalSnap <| object.x + object.width//2
  , VerticalSnap <| object.y + object.height//2
  , HorizontalSnap <| object.x + object.width
  , VerticalSnap <| object.y + object.height
  ]

horizontalSnaps : List SnapLine -> List Int
horizontalSnaps snapLines =
  snapLines
  |> List.filter isHorizontal
  |> List.map snapPosition

verticalSnaps : List SnapLine -> List Int
verticalSnaps snapLines =
  snapLines
  |> List.filter isVertical
  |> List.map snapPosition

snapPosition : SnapLine -> Int
snapPosition snapLine =
  case snapLine of
    VerticalSnap pos -> pos
    HorizontalSnap pos -> pos

isHorizontal : SnapLine -> Bool
isHorizontal snapLine =
  case snapLine of
    VerticalSnap _ -> False
    HorizontalSnap _ -> True

isVertical : SnapLine -> Bool
isVertical snapLine =
  case snapLine of
    VerticalSnap _ -> True
    HorizontalSnap _ -> False

lineSnaps : Int -> SnapLine -> SnapLine -> Bool
lineSnaps distance snapLine snappedLine =
  case (snapLine, snappedLine) of
    (HorizontalSnap snapPoint, HorizontalSnap snappedPoint) ->
      snaps distance snapPoint snappedPoint
    (VerticalSnap snapPoint, VerticalSnap snappedPoint) ->
      snaps distance snapPoint snappedPoint
    _ ->
      False

snaps : Int -> Int -> Int -> Bool
snaps distance snapPoint snappedPoint =
  (abs <| snapPoint - snappedPoint) < distance
