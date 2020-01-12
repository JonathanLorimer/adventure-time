module UIHelpers where

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text (Text)

import Brick
import Types

uiBase :: Mode -> [Widget Resource] -> [Widget Resource]
uiBase m w = [ genericBorder
  (T.pack ("Adventure Time - Mode: " <> show m))
  $ vBox w]

genericBorder :: Text -> Widget Resource -> Widget Resource
genericBorder label widget = C.center
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt label)
  $ C.center widget

prefixCursor :: CursorPos -> [Text] -> [Text]
prefixCursor p l = fst $ foldr
      (\e (xs, i) -> if i == p
                      then ("➤ " <> e : xs, i - 1)
                      else ("  " <> e : xs, i - 1))
      ([], L.length l - 1) l
