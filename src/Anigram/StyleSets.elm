module Anigram.StyleSets exposing (..)

import Dict exposing (Dict)

import Color

import Anigram.Common exposing (..)
import Anigram.Object exposing (defaultStyle)

sets : StyleSets
sets =
  Dict.fromList
  [ ("", none)
  , ("Default", default)
  , ("Highlighted", highlighted)
  ]

none : StyleSet
none = []

default : StyleSet
default =
  [ Fill defaultStyle.fill
  , Stroke defaultStyle.stroke
  , StrokeWidth defaultStyle.strokeWidth
  ]

highlighted : StyleSet
highlighted =
  [ Stroke Color.red
  ]
