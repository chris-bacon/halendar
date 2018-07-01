module Events where

import Brick
import Graphics.Vty

import Calendar

-- TODO: Refactor to use lenses!
-- TODO: Remove magic numbers!
handleEvent :: Calendar -> BrickEvent a b -> EventM c (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'q') [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'w') [])) = continue Calendar { currentYear = currentYear c , currentMonth = currentMonth c , currentDay = currentDay c , focusedDay = focusedDay c - 7 }
handleEvent c (VtyEvent (EvKey (KChar 'a') [])) = continue Calendar { currentYear = currentYear c , currentMonth = currentMonth c , currentDay = currentDay c , focusedDay = focusedDay c - 1 }
handleEvent c (VtyEvent (EvKey (KChar 's') [])) = continue Calendar { currentYear = currentYear c , currentMonth = currentMonth c , currentDay = currentDay c , focusedDay = focusedDay c + 7 }
handleEvent c (VtyEvent (EvKey (KChar 'd') [])) = continue Calendar { currentYear = currentYear c , currentMonth = currentMonth c , currentDay = currentDay c , focusedDay = focusedDay c + 1 }
handleEvent c _ = continue c

