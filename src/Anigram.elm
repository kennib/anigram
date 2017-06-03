module Anigram exposing (..)

import List.Extra as List

import Html exposing (Html, program, div)
import Html.Attributes exposing (style)

import Color exposing (Color)

import Anigram.Common exposing (..)
import Anigram.Object as Objects
import Anigram.Frames as Frames
import Anigram.Controls as Ctrls

main =
  program
    { init = (model, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

model : Model
model =
  { objects = []
  , frames = [ Frames.empty ]
  , frameIndex = 0
  , controls = Ctrls.model
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    mergeUpdate nextUpdate (model, cmds) =
      nextUpdate msg model
        |> Tuple.mapSecond (\newCmd -> newCmd :: cmds)

    batchUpdate updates =
      List.foldl mergeUpdate (model, []) updates
        |> Tuple.mapSecond Cmd.batch
  in
    batchUpdate
      [ Ctrls.update
      , Objects.update
      , Frames.update
      ]

subscriptions =
  Objects.subscriptions

view : Model -> Html Msg
view model =
  div
    [ style
      [ ("height", "100vh")
      ]
    ]
    [ Ctrls.view model
    , anigramView model
    ]

anigramView : Model -> Html Msg
anigramView model =
  div
    [ style
      [ ("display", "flex")
      ]
    ]
    [ Objects.view
      <| Maybe.withDefault []
      <| Frames.getFrameObjects model.frameIndex model.frames model.objects
    , Frames.view model
    ]
