module Cmd exposing (..)

import Task exposing (perform, succeed)

message : msg -> Cmd msg
message msg =
  Task.perform identity <| succeed msg
