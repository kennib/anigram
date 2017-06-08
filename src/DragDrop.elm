module DragDrop exposing (..)

import Mouse

type DragDrop
  = Unselected
  | PickedUp
  | StartDrag Mouse.Position
  | Drag Mouse.Position Mouse.Position
  | Drop Mouse.Position Mouse.Position


empty = Unselected 

pickup = PickedUp

drag dragDrop pos = 
  case dragDrop of
    Unselected -> Unselected
    PickedUp -> StartDrag pos
    StartDrag start -> Drag start pos
    Drag start end -> Drag start pos
    Drop _ _ -> StartDrag pos

drop dragDrop =
  case dragDrop of
    Unselected -> Unselected
    PickedUp -> Unselected
    StartDrag start -> Unselected
    Drag start end -> Drop start end
    Drop start end -> Drop start end

start dragDrop =
  case dragDrop of
    StartDrag start -> start
    Drag start end -> start
    Drop start end -> start
    _ -> { x = 0, y = 0 }

startend dragDrop =
  case dragDrop of
    Drag start end -> Just (start, end)
    Drop start end -> Just (start, end)
    _ -> Nothing

delta dragDrop =
  case dragDrop of
    Drag start end -> { x = end.x - start.x, y = end.y - start.y }
    Drop start end -> { x = end.x - start.x, y = end.y - start.y }
    _ -> { x = 0, y = 0 }

mapDragged function dragDrop =
  if isDragged dragDrop then
    function dragDrop
  else
    dragDrop

mapDropped function dragDrop =
  if isDropped dragDrop then
    function dragDrop
  else
    dragDrop

mapEnd function dragDrop =
    case dragDrop of
      Drag start end -> Drag start <| function end
      Drop start end -> Drop start <| function end
      _ -> dragDrop

isDragged dragDrop =
  case dragDrop of
    Unselected -> False
    PickedUp -> True
    StartDrag _ -> True
    Drag _ _ -> True
    Drop _ _ -> False

isDropped dragDrop =
  case dragDrop of
    Unselected -> False 
    PickedUp -> False
    StartDrag _ -> False
    Drag _ _ -> False
    Drop _ _ -> True
