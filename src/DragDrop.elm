module DragDrop exposing (..)

import Mouse

type DragDrop
  = StartDrag Mouse.Position
  | Drag Mouse.Position Mouse.Position

drag dragDrop pos = 
  case dragDrop of
    StartDrag start -> Drag start pos
    Drag start end -> Drag start pos

start dragDrop =
  case dragDrop of
    StartDrag start -> start
    Drag start end -> start

startend dragDrop =
  case dragDrop of
    Drag start end -> Just (start, end)
    _ -> Nothing

delta dragDrop =
  case dragDrop of
    Drag start end -> { x = end.x - start.x, y = end.y - start.y }
    _ -> { x = 0, y = 0 }

mapEnd function dragDrop =
    case dragDrop of
      Drag start end -> Drag start <| function end
      _ -> dragDrop
