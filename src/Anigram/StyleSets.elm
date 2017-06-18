module Anigram.StyleSets exposing (..)

import Dict exposing (Dict)

import Color

import Anigram.Common exposing (..)
import Anigram.Object exposing (defaultStyle)

sets : StyleSets
sets =
  Dict.fromList
  [ ("Default", default) 
  , ("Highlighted", highlighted)
  ]

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
