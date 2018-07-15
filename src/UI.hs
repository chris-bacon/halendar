module UI where

import Brick
import Brick.Widgets.Center
import qualified Brick.Widgets.Edit as Edit
import qualified Brick.Focus as F
import Control.Lens
import qualified Data.Time as Time
import qualified Data.Time.Calendar.MonthDay as MonthDay
import Data.Monoid 
import Graphics.Vty

import Calendar

instance Monoid (Widget a) where
    mempty = emptyWidget
    mappend a b = a <+> b

getDaysInMonth :: Calendar -> [Int]
getDaysInMonth c = [1..(MonthDay.monthLength (Time.isLeapYear $ c ^. currentYear) (c ^. currentMonth))]

padRightWithSpaces :: Int -> Widget String -> Widget String
padRightWithSpaces n = padRight (Pad n)

-- TODO: Refactor this mess of code
dateToWidget :: Calendar -> String -> Widget String
dateToWidget c d
  | length d == 1 && (read d :: Int) == c ^. focusedDay = (padRightWithSpaces 3) . styleToday . str $ d
  | length d == 1 = (padRightWithSpaces 3) . str $ d
  | length d == 2 && (read d :: Int) == c ^. focusedDay = (padRightWithSpaces 2) . styleToday . str $ d
  | otherwise = (padRightWithSpaces 2) . str $ d
styleToday :: Widget String -> Widget String
styleToday = withAttr (attrName "focusedDay")

widgetsToRows :: [[Widget String]] -> [Widget String]
widgetsToRows w = foldr (<>) mempty <$> w

splitAtAll :: Int -> [Int] -> [[Int]]
splitAtAll _ [] = []
splitAtAll c xs = [(fst $ splitAt c xs)] ++ splitAtAll c (drop c xs)

displayMonthYear :: Calendar -> Widget String
displayMonthYear c = str (show $ c ^. currentMonth) <+> str "/" <+> str (show $ c ^. currentYear)

datesUI :: Calendar -> [[Int]] -> Widget String 
datesUI c dates = vBox $ widgetsToRows $ ((<$>) . (<$>)) ((dateToWidget c) . show) dates

drawHour :: Calendar -> Int -> Widget String
drawHour c n
  | c ^. day ^. focusedHour == n = styleToday . padBottom (Pad 1) $ (str $ show n) <> str "  " <> editorContents
  | otherwise = (str $ show n) <+> str "  " <+> editorContents
    where
        editorContents = (str . unlines) (Edit.getEditContents (c ^. editor))

dayUI :: Calendar -> Widget String
dayUI c = 
    str (show $ c ^. focusedDay) <+> str "/" <+> displayMonthYear c
    <=> (vBox $ fmap (drawHour c) [1..24])

editUI :: Calendar -> Widget String
editUI c = hLimit 30 (F.withFocusRing (F.focusRing ["Editor"]) (Edit.renderEditor (str . unlines)) (c ^. editor))

ui :: Calendar -> Widget String
ui c@(Calendar DayView _ _ _ _ _ _) = dayUI c
ui c@(Calendar EditView _ _ _ _ _ _) = editUI c
ui c =  
    displayMonthYear c
    <=> datesUI c (splitAtAll 7 (getDaysInMonth c))

drawUI :: Calendar -> [Widget String]
drawUI c = return $ ui c

