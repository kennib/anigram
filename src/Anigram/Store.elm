port module Anigram.Store exposing (..)

import Result

import Kinto

import Json.Encode
import Json.Decode

import Anigram.Common exposing (..)
import Anigram.Frames
import Anigram.StyleSets
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
  result
    |> Result.map .objects
    |> Result.map (List.head >> Maybe.withDefault defaultAnigram)
    |> Result.withDefault defaultAnigram

defaultAnigram : Anigram
defaultAnigram = { frames = [Anigram.Frames.empty, Anigram.Frames.empty], styleSets = Anigram.StyleSets.sets }

port storeLocal : Json.Encode.Value -> Cmd msg
storeLocalAnigram : Anigram -> Cmd Msg
storeLocalAnigram anigram =
  storeLocal <| encodeAnigram anigram

port loadLocal : (Json.Encode.Value -> msg) -> Sub msg
loadLocalAnigram : Sub Msg
loadLocalAnigram =
  loadLocal (AnigramLoaded << Result.withDefault defaultAnigram << Json.Decode.decodeValue decodeAnigram)
