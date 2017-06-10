module Anigram.History exposing (..)

import Anigram.Common exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Undo ->
      (undo model, Cmd.none)
    Redo ->
      (redo model, Cmd.none)
    AddObject _ ->
      (push model, Cmd.none)
    Selection change ->
      (push model, Cmd.none)
    DeleteFrame ->
      (push model, Cmd.none)
    _ ->
      (model, Cmd.none)

undo : Model -> Model
undo model =
  case List.head model.history.past of
    Just (frames, objects) ->
      { model
      | history =
        model.history
          |> mapPast (List.drop 1)
          |> mapFuture ((::) (model.frames, model.objects))
      , frames = frames
      , objects = objects
      }
    Nothing -> model

redo : Model -> Model
redo model =
  case List.head model.history.future of
    Just (frames, objects) ->
      { model
      | history =
        model.history
          |> mapPast ((::) (model.frames, model.objects))
          |> mapFuture (List.drop 1)
      , frames = frames
      , objects = objects
      }
    Nothing -> model

push : Model -> Model
push model =
  { model
  | history =
    model.history
      |> mapPast ((::) (model.frames, model.objects))
      |> mapFuture (\_ -> [])
  }

mapPast : (List a -> List a) -> History a -> History a
mapPast map history = { history | past = map history.past }

mapFuture : (List a -> List a) -> History a -> History a
mapFuture map history = { history | future = map history.future }
