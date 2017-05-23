module Component exposing (..)

import List.Extra as List

import Html exposing (..)

type alias Component model msg =
  { init : (model, Cmd msg)
  , update : msg -> model -> (model, Cmd msg)
  , subscriptions : model -> Sub msg
  , view : model -> Html msg
  }

type Combined a b
  = Combined a b

type Either a b
  = Left a
  | Right b

combine :
  (Html (Either msgA msgB) -> Html (Either msgA msgB) -> Html (Either msgA msgB)) ->
  (msgA -> Maybe msgB) ->
  (msgB -> Maybe msgA) ->
  Component modelA msgA ->
  Component modelB msgB ->
  Component (Combined modelA modelB) (Either msgA msgB)
combine view aToB bToA componentA componentB =
  { init = combineInit componentA.init componentB.init 
  , update = combineUpdate aToB bToA componentA.update componentB.update
  , subscriptions = combineSubscriptions componentA.subscriptions componentB.subscriptions
  , view = combineView view componentA.view componentB.view
  }

combineInit :
  (modelA, Cmd msgA) ->
  (modelB, Cmd msgB) ->
  (Combined modelA modelB, Cmd (Either msgA msgB))
combineInit initA initB = 
  let
    (modelA, cmdA) = initA
    (modelB, cmdB) = initB
  in
    (combineModel modelA modelB, combineCmd cmdA cmdB)

combineModel : modelA -> modelB -> Combined modelA modelB
combineModel =
  Combined

combineCmd : Cmd msgA -> Cmd msgB -> Cmd (Either msgA msgB)
combineCmd cmdA cmdB =
  Cmd.batch
    [ Cmd.map Left <| cmdA
    , Cmd.map Right <| cmdB
    ]

combineUpdate :
  (msgA -> Maybe msgB) ->
  (msgB -> Maybe msgA) ->
  (msgA -> modelA -> (modelA, Cmd msgA)) ->
  (msgB -> modelB -> (modelB, Cmd msgB)) ->
  Either msgA msgB -> Combined modelA modelB -> (Combined modelA modelB, Cmd (Either msgA msgB))
combineUpdate aToB bToA updateA updateB msg (Combined modelA modelB) =
  case msg of
    Left msgA ->
      let
        resultA = updateA msgA modelA
        resultB =
          case aToB msgA of
            Just msgB ->
              updateB msgB modelB
            Nothing ->
              (modelB, Cmd.none)
      in
        combineInit resultA resultB
    Right msgB ->
      let
        resultA =
          case bToA msgB of
            Just msgA ->
              updateA msgA modelA
            Nothing ->
              (modelA, Cmd.none)
        resultB = updateB msgB modelB
      in
        combineInit resultA resultB

combineSubscriptions :
  (modelA -> Sub msgA) ->
  (modelB -> Sub msgB) ->
  Combined modelA modelB -> Sub (Either msgA msgB)
combineSubscriptions subsA subsB (Combined modelA modelB) =
  Sub.batch
    [ Sub.map Left <| subsA modelA
    , Sub.map Right <| subsB modelB
    ]

combineView :
  (Html (Either msgA msgB) -> Html (Either msgA msgB) -> Html (Either msgA msgB)) ->
  (modelA -> Html msgA) ->
  (modelB -> Html msgB) ->
  Combined modelA modelB -> Html (Either msgA msgB)
combineView view viewA viewB (Combined modelA modelB) =
  let
    htmlA = viewA modelA
    htmlB = viewB modelB
  in
    view
      (Html.map Left htmlA)
      (Html.map Right htmlB)


merge : 
  (List (Html msg) -> Html msg) ->
  List (Component model msg) ->
  Component (List model) msg
merge view components =
  { init = components |> List.map .init |> mergeInit
  , update = components |> List.map .update |> mergeUpdate
  , subscriptions = components |> List.map .subscriptions |> mergeSubscriptions
  , view = components |> List.map .view |> mergeView view
  }

mergeInit :
  List (model, Cmd msg) ->
  (List model, Cmd msg)
mergeInit inits = 
  let
    (models, cmds) = List.unzip inits
  in
    (models, Cmd.batch cmds)

mergeUpdate :
  List (msg -> model -> (model, Cmd msg)) ->
  msg -> List model -> (List model, Cmd msg)
mergeUpdate updates msg models =
  let
    results =
      updates
        |> List.map (\update -> update msg)
        |> List.andMap models
  in
    mergeInit results

mergeSubscriptions :
  List (model -> Sub msg) ->
  List model -> Sub msg
mergeSubscriptions subs models =
  let
    subscriptions = subs |> List.andMap models
  in
    Sub.batch subscriptions

mergeView :
  (List (Html msg) -> Html msg) ->
  List (model -> Html msg) ->
  List model -> Html msg
mergeView view views models =
  let
    html = views |> List.andMap models
  in
    view html
