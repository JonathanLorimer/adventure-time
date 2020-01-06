module UI where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Edit (Action(..))
import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Types
import qualified Graphics.Vty as V

data Mode = PickStory
          | PickMode
          | Play
          | Edit (Maybe Action)
          deriving (Eq, Ord, Show)

data AppState = AppState { mode       :: Mode
                         , stories    :: Map Title Story
                         , story      :: Maybe Story
                         , curPassage :: Maybe Passage
                         , cursor     :: CursorPos }

type Resource = ()
type CursorPos = Int

ui :: App AppState e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = handleEvent
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: AppState -> [Widget Resource]
drawUI (AppState PickStory       s _ _ c) = uiBase PickStory $ drawStories c (M.elems s)
drawUI (AppState PickMode        _ _ _ c) = uiBase PickMode $ drawPickMode c
drawUI a@(AppState Play          _ _ _ _) = uiBase Play [drawPlay a]
drawUI (AppState (Edit Nothing)  _ _ _ c) = uiBase (Edit Nothing) [drawPickAction]
drawUI (AppState (Edit (Just a)) _ (Just s) _ c) = uiBase (Edit Nothing) [drawEdit a s]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap = attrMap V.defAttr []

handleEvent :: AppState -> BrickEvent Resource e -> EventM Resource (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KUp []))    =
  if cursor s == 0
    then continue s
    else continue $ s { cursor = cursor s - 1 }
handleEvent s (VtyEvent (V.EvKey V.KDown []))  =
  case mode s of
   PickStory -> safeMove (M.size (stories s) - 1)
   PickMode -> safeMove 1
   Play     -> safeMove $ ((+ (-1)) . length . choices) (fromJust $ curPassage s)
  where
    safeMove i = if cursor s == i
                  then continue s
                  else continue $ s { cursor = cursor s + 1 }

handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = continue $ transitionState s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))   = halt s
handleEvent s _                                = continue s

transitionState :: AppState -> AppState
transitionState s =
  case mode s of
    PickStory -> s { mode = PickMode
                   , story = Just $ M.elems (stories s) !! cursor s
                   , cursor = 0 }
    PickMode  -> s { mode = if cursor s == 0 then Edit Nothing else Play
                   , curPassage = story s >>= \st -> M.lookup (start st) (passages st) }
    Play      -> case choices $ fromJust (curPassage s) of
                     [] -> s { mode = PickStory
                                  , story = Nothing
                                  , curPassage = Nothing
                                  , cursor = 0 }
                     xs -> s { curPassage = M.lookup
                                              (xs !! cursor s)
                                              (fromJust $ passages <$> story s) }
    _ -> s
{-
    Edit _    ->
-}

uiBase :: Mode -> [Widget Resource] -> [Widget Resource]
uiBase m w = [ genericBorder
  (T.pack ("Adventure Time - Mode: " <> show m))
  $ vBox w]

genericBorder :: Text -> Widget Resource -> Widget Resource
genericBorder label widget = C.center
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt label)
  $ C.center widget


drawStories :: CursorPos -> [Story] -> [Widget Resource]
drawStories _ [] = [txtWrap . ("➤ " <>) $ "create story"]
drawStories p ss = txtWrap <$> prefixCursor p (showTitle . storyTitle <$> ss)

drawPickMode :: CursorPos -> [Widget Resource]
drawPickMode 0 = [txtWrap "➤ Edit Mode", txtWrap "  Play Mode"]
drawPickMode 1 = [txtWrap "  Edit Mode", txtWrap "➤ Play Mode"]
drawPickMode _ = error "should not be able to get here"


drawPickAction :: Widget Resource
drawPickAction = undefined

drawEdit :: Action -> Story -> Widget Resource
drawEdit a = undefined

drawPlay :: AppState -> Widget Resource
drawPlay (AppState _ _ Nothing _ _) = error "should not be able to get here"
drawPlay (AppState m ss (Just s) p c) =
  let psg = fromMaybe (fromJust $ M.lookup (start s) (passages s)) p
      cw  = drawChoices c (catMaybes $ flip M.lookup (passages s) <$> choices psg)
  in  genericBorder "choose a path" (vBox cw)
  <+> genericBorder "passage" (padAll 1 $ txtWrap (passage psg))

drawChoices :: CursorPos -> [Passage] -> [Widget Resource]
drawChoices _ [] = [txtWrap "Fin"]
drawChoices p ps = txtWrap <$> prefixCursor p (showTitle . passageTitle <$> ps)


prefixCursor :: CursorPos -> [Text] -> [Text]
prefixCursor p l = fst $ foldr
      (\e (xs, i) -> if i == p
                      then ("➤ " <> e : xs, i - 1)
                      else ("  " <> e : xs, i - 1))
      ([], L.length l - 1) l
