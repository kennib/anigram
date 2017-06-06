module Anigram.Selection exposing (..)

import Dict

import CopyPaste

import Anigram.Common exposing (..)

toAnigram : Model ->  Anigram
toAnigram model =
  let
    selection = ids model.objects
    frames =
      model.frames
        |> List.map (Dict.filter (\id changes -> List.member id selection))
  in
    { frames = frames }

selection = List.filter (.state >> .selected)
ids = selection >> List.map .id
