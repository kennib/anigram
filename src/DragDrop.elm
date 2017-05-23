module DragDrop exposing (..)

import Mouse

type DragDrop
  = Unselected
  | PickedUp
  | StartDrag Mouse.Position
  | Drag Mouse.Position Mouse.Position


empty = Unselected 

pickup = PickedUp

drag dragDrop pos = 
  case dragDrop of
    Unselected -> Unselected
    PickedUp -> StartDrag pos
    StartDrag start -> Drag start pos
    Drag start end -> Drag start pos

drop dragDrop =
  case dragDrop of
    Unselected -> Nothing
    PickedUp -> Just { x = 0, y = 0 }
    StartDrag start -> Just { x = 0, y = 0 }
    Drag start end -> Just { x = end.x - start.x, y = end.y - start.y }

isDragged dragDrop =
  case dragDrop of
    Unselected -> False
    PickedUp -> True
    StartDrag _ -> True
    Drag _ _ -> True
