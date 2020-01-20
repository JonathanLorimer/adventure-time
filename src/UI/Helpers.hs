{-# LANGUAGE LambdaCase #-}
module UI.Helpers where

import           Brick.Forms
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Brick.Widgets.Core         (padLeftRight)
import qualified Data.List                  as L
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Brick
import           Types

uiBase :: Mode -> [Widget Resource] -> [Widget Resource]
uiBase m w = [ genericBorder
  (T.pack ("Adventure Time - Mode: " <> show m))
  $ vBox w]

genericBorder :: Text -> Widget Resource -> Widget Resource
genericBorder label widget = C.center
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (borderName label)
  $ C.center widget

prefixCursor :: CursorPos -> [Text] -> [Text]
prefixCursor p l = fst $ foldr
      (\e (xs, i) -> if i == p
                      then ("➤ " <> e : xs, i - 1)
                      else ("  " <> e : xs, i - 1))
      ([], L.length l - 1) l


-- Taken from Kowainik's Summoner Tui
-- https://github.com/kowainik/summoner/blob/master/summoner-tui/src/Summoner/Tui/GroupBorder.hs

borderLabel :: Text -> Widget n -> Widget n
borderLabel l = B.borderWithLabel (borderName l)

borderName :: Text -> Widget n
borderName l = txt "╼" <+> padLeftRight 1 (txt l) <+> txt "╾"
-- | Create a pair of elements.
infix 4 |>
(|>) :: Int -> a -> (Int, a)
(|>) = (,)
{-# INLINE (|>) #-}

{- **Note:** on an empty list it doesn't create any group or border.
-}
groupBorder :: Text -> [(Int, s -> FormFieldState s e n)] -> [s -> FormFieldState s e n]
groupBorder groupName  = \case
    []       -> []
    [x]      -> [groupAllBorders groupName x]
    (x:y:xs) -> let (mid, l) = (init $ y : xs, last $ y : xs) in
        groupBorderTop groupName x : map groupBorderMid mid ++ [groupBorderBottom l]

-- | Creates the top border with the group name.
groupBorderTop :: Text -> (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderTop groupName (i, f) =
    ( vLimit i
    . ((tl <=> B.vBorder) <+>)
    . (<+> (tr <=> B.vBorder))
    . (B.hBorderWithLabel (borderName groupName) <=>)
    ) @@= f

-- | Creates the bottom border of the group.
groupBorderBottom :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderBottom (i, f) =
    ( vLimit i
    . ((B.vBorder <=> bl) <+>)
    . (<+> (B.vBorder <=> br))
    . (<=> B.hBorder)
    . padRight Max
    ) @@= f

-- | Creates the left and right borders for the middle elements of the group.
groupBorderMid :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderMid (i, f) =
    ( vLimit i
    . (B.vBorder <+>)
    . (<+> B.vBorder)
    . padRight Max
    ) @@= f

-- | Creates the border around the only one element.
groupAllBorders :: Text -> (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupAllBorders groupName (i, f) =
    ( vLimit i
    . borderLabel groupName
    . padRight Max
    ) @@= f


-- | Helpers for the correct border lines.
tl, tr, bl, br :: Widget n
tl = B.joinableBorder (Edges False True False True)
tr = B.joinableBorder (Edges False True True False)
bl = B.joinableBorder (Edges True False False True)
br = B.joinableBorder (Edges True False True False)
