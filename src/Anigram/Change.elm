module Anigram.Change exposing (..)

import Anigram.Common exposing (..)

isChangeType : Change -> Bool
isChangeType change =
  case change of
    ChangeType _ -> True
    _ -> False

isAddStyleSet : Change -> Bool
isAddStyleSet change =
  case change of
    AddStyleSet _ -> True
    _ -> False

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

isMoveTo : Change -> Bool
isMoveTo change =
  case change of
    MoveTo _ -> True
    _ -> False

isResize : Change -> Bool
isResize change =
  case change of
    Resize _ _ -> True
    _ -> False

isSizeTo : Change -> Bool
isSizeTo change =
  case change of
    SizeTo _ -> True
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

isStrokeWidth : Change -> Bool
isStrokeWidth change =
  case change of
    StrokeWidth _ -> True
    _ -> False

isTextColor : Change -> Bool
isTextColor change =
  case change of
    TextColor _ -> True
    _ -> False

isTextSizeTo : Change -> Bool
isTextSizeTo change =
  case change of
    TextSizeTo _ -> True
    _ -> False
