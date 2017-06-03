module Anigram.Change exposing (..)

import Anigram.Common exposing (..)

isHide : Change -> Bool
isHide change =
  case change of
    Hide _ -> True
    _ -> False

isSetText : Change -> Bool
isSetText change =
  case change of
    SetText _ -> True
    _ -> False

isMove : Change -> Bool
isMove change =
  case change of
    Move _ -> True
    _ -> False

isFill : Change -> Bool
isFill change =
  case change of
    Fill _ -> True
    _ -> False

isStroke : Change -> Bool
isStroke change =
  case change of
    Stroke _ -> True
    _ -> False
