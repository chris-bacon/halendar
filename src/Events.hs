module Events where

import Brick
import Control.Lens
import Graphics.Vty

import Calendar

-- TODO: Refactor to use lenses!
-- TODO: Remove magic numbers!
-- TODO: Handle ends and beginning of month
handleEvent :: Calendar -> BrickEvent a b -> EventM c (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'q') [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'w') [])) = continue $ c & focusedDay -~ 7
handleEvent c (VtyEvent (EvKey (KChar 'a') [])) = continue $ c & focusedDay -~ 1
handleEvent c (VtyEvent (EvKey (KChar 's') [])) = continue $ c & focusedDay +~ 7
handleEvent c (VtyEvent (EvKey (KChar 'd') [])) = continue $ c & focusedDay +~ 1
handleEvent c (VtyEvent (EvKey (KChar 'r') [])) = continue $ c & focusedDay .~ currentDay c
handleEvent c (VtyEvent (EvKey KEnter [])) = continue $ c & dayView .~ True
handleEvent c _ = continue c

