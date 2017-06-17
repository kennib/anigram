module Anigram.Shapes exposing
  ( toPath
  , square
  , star
  , polarPoint
  )

import Svg.Path exposing (..)

import Anigram.Common exposing (..)

square x y w h = 
  [ ( x, y )
  , ( x + w, y )
  , ( x + w, y + h)
  , ( x, y + h)
  ]

star : Int -> Int -> Int -> Int -> Int -> Shape
star x y w h points = 
  let
    getLength count = if count % 2 == 0 then h//2 else h//5 
    getRadians count = (toFloat count / toFloat points * pi) - pi/2
    getCoords count = polarPoint (x + w//2) (y + h//2) (getLength count) (getRadians count)

  in
    List.map getCoords (List.range 1 (2 * points))

polarPoint : Int -> Int -> Int -> Float -> (Int, Int)
polarPoint x y length angle =
  ( x + (round <| toFloat length * cos angle)
  , y + (round <| toFloat length * sin angle)
  )

toPath : Shape -> String
toPath shape =
  pathToString [ polygon <| floatPath <| shape ]

floatPath : List (Int, Int) -> List (Float, Float)
floatPath intPath = 
  let
    doubleFloat (a, b) = (toFloat a, toFloat b)
  in
    List.map doubleFloat intPath

polygon ps =
  case ps of
      [] ->
          emptySubpath

      x :: xs ->
          subpath (startAt x) closed [ lineToMany xs ]