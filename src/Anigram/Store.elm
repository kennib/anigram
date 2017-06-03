module Anigram.Store exposing (..)

import Result

import Kinto

import Anigram.Common exposing (..)
import Anigram.Frames
import Anigram.Encode exposing (encodeAnigram)
import Anigram.Decode exposing (decodeAnigram)

client : Kinto.Client
client =
  Kinto.client
    "https://kinto.dev.mozaws.net/v1/"
    (Kinto.Basic "test" "test")

anigramResource : Kinto.Resource Anigram
anigramResource =
    Kinto.recordResource "default" "anigram" decodeAnigram

saveAnigram : Anigram -> Cmd Msg
saveAnigram anigram =
  client
    |> Kinto.create anigramResource (encodeAnigram anigram)
    |> Kinto.send (\_ -> AnigramSaved)

loadAnigram : Cmd Msg
loadAnigram =
  client
    |> Kinto.getList anigramResource
    |> Kinto.limit 1
    |> Kinto.send (AnigramLoaded << anigramResult)

anigramResult : Result Kinto.Error (Kinto.Pager Anigram) -> Anigram
anigramResult result =
  let
    default = { frames = [Anigram.Frames.empty] }
  in
    result
      |> Result.map .objects
      |> Result.map (List.head >> Maybe.withDefault default)
      |> Result.withDefault default
