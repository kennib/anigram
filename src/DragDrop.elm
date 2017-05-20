module DragDrop exposing (..)

import Mouse

type DragDrop a
  = Empty
  | Select a
  | StartDrag a Mouse.Position
  | Drag a Mouse.Position Mouse.Position


empty = Empty 

pickup = Select

drag dragDrop pos = 
  case dragDrop of
      Empty -> Empty
      Select item -> StartDrag item pos
      StartDrag item start -> Drag item start pos
      Drag item start end -> Drag item start pos

drop dragDrop =
  case dragDrop of
    Empty -> Nothing
    Select item -> Just (item, { x = 0, y = 0 })
    StartDrag item start -> Just (item, { x = 0, y = 0 })
    Drag item start end -> Just (item, { x = end.x - start.x, y = end.y - start.y })

draggedItem dragDrop =
  case dragDrop of
    Empty -> Nothing
    Select item -> Just item
    StartDrag item _ -> Just item
    Drag item _ _ -> Just item
