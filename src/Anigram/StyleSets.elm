module Anigram.StyleSets exposing (..)

import Dict exposing (Dict)

import Color

import Anigram.Common exposing (..)

sets : StyleSets
sets =
  Dict.fromList
  [ ("Default", default) 
  , ("Highlighted", highlighted)
  ]

default : StyleSet
default = []

highlighted : StyleSet
highlighted =
  [ Stroke Color.red
  ]
