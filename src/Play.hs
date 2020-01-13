module Play where

import Brick
import qualified Data.Map as M
import Data.Maybe
import Types
import UIHelpers


drawPlay :: AppState e -> Widget Resource
drawPlay AppState {story = Nothing} = error "should not be able to get here"
drawPlay AppState { story = (Just s), curPassage, cursor} =
  let psg = fromMaybe (fromJust $ M.lookup (start s) (passages s)) curPassage
      cw  = drawChoices cursor (catMaybes $ flip M.lookup (passages s) <$> choices psg)
  in  genericBorder "choose a path" (vBox cw)
  <+> genericBorder "passage" (padAll 1 $ txtWrap (passage psg))

drawChoices :: CursorPos -> [Passage] -> [Widget Resource]
drawChoices _ [] = [txtWrap "Fin"]
drawChoices p ps = txtWrap <$> prefixCursor p (showTitle . passageTitle <$> ps)
