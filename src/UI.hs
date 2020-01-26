module UI where

-- External Imports
import           Brick
import           Brick.Focus  (focusGetCurrent)
import           Brick.Forms
import qualified Data.List    as L
import qualified Data.Map     as M
import           Data.Maybe
-- import           Data.UUID    (UUID)
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Vty as V
import           Lens.Micro   ((^.))

-- Internal Imports
import           Menu
import           Mode.Edit
import           Mode.Play    (drawPlay)
import           Types
import           UI.Helpers

ui :: App (AppState e) e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = handleEvent
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: AppState e -> [Widget Resource]
drawUI AppState { mode = PickStory, stories, cursor} = uiBase PickStory
  $ drawStories cursor (M.elems stories)
drawUI AppState { mode = PickMode, cursor}           = uiBase PickMode
  $ drawPickMode cursor
drawUI a@AppState { mode = Play}                     = uiBase Play [drawPlay a]
drawUI AppState { mode = (Edit Nothing), cursor} =
  uiBase (Edit Nothing) (drawPickAction cursor)
drawUI AppState { mode = (Edit (Just a))
                , story = (Just s)
                , passageForm = Just pf } =
  uiBase (Edit Nothing) [drawEdit a s pf]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap =
  attrMap V.defAttr [ (focusedFormInputAttr, V.black `on` V.yellow)
                    ]

handleEvent :: AppState e -> BrickEvent Resource e -> EventM Resource (Next (AppState e))

-- Escape Events
handleEvent s (VtyEvent (V.EvKey V.KEsc []))   = halt s

-- Form Events
handleEvent s@AppState { mode = Edit (Just AddPassage)
                       , passageForm = Just pasform } e = do
      form <- handleFormEvent e pasform
      case e of
        (VtyEvent (V.EvKey V.KEnter [])) ->
          if (fromJust . focusGetCurrent . formFocus $ form) == SubmitField
            then ioAction (persistence s) s (formState form)
            else continue s
        _                                ->
          if formState form ^. formSubmit
            then ioAction (persistence s) s (formState form)
            else continue $ s { passageForm = Just form }

  where
    ioAction :: IORefPersistence -> AppState e -> PassageForm -> EventM Resource (Next (AppState e))
    ioAction Persistence { get, put} as pf = suspendAndResume $ do
                      uid <- nextRandom
                      let pid = ID uid
                      let passage = Passage { uuid = pid
                                            , passageTitle = pf ^. formPassageTitle
                                            , passage = pf ^. formPassage
                                            , choices = fmap fst
                                                      . filter snd
                                                      . M.toList
                                                      $ pf ^. formChoices
                                            }
                      let oldStory = fromJust $ story as
                      let newStory = oldStory { passages = M.insert pid passage (passages oldStory) }
                      stories <- get
                      let newState =  M.adjust (const newStory) (storyTitle newStory) stories
                      put newState
                      pure s { mode        = PickStory
                             , passageForm = Nothing
                             , story       = Nothing
                             , curPassage  = Nothing
                             , cursor      = 0
                             }

-- Cursor Events
handleEvent s (VtyEvent (V.EvKey V.KUp []))    =
  if cursor s == 0
    then continue s
    else continue $ s { cursor = cursor s - 1 }
handleEvent s (VtyEvent (V.EvKey V.KDown []))  =
  case mode s of
   PickStory    -> safeMove (M.size (stories s))
   PickMode     -> safeMove 2
   Play         -> safeMove $ (length . choices) (fromJust $ curPassage s)
   Edit Nothing -> safeMove 3
   _            -> continue s
  where
    safeMove i = if cursor s == (i - 1)
                  then continue s
                  else continue $ s { cursor = cursor s + 1 }

-- Transition Event
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = continue $ transitionState s
handleEvent s _                                = continue s

transitionState :: AppState e -> AppState e
transitionState s =
  case mode s of
    -- Menu Transitions
    PickStory -> s { mode = PickMode
                   , story = Just $ M.elems (stories s) !! cursor s
                   , cursor = 0
                   }
    PickMode  -> s { mode = if cursor s == 0
                               then Edit Nothing
                               else Play
                   , curPassage = story s >>=
                      \st -> M.lookup (start st) (passages st)
                   }

    -- Play Transitions
    Play  -> case choices $ fromJust (curPassage s) of
                [] -> s { mode       = PickStory
                        , story      = Nothing
                        , curPassage = Nothing
                        , cursor     = 0
                        }
                xs -> s { curPassage =
                       M.lookup (xs !! cursor s)
                                (fromJust $ passages <$> story s)
                        }

    -- Edit Transitions
    Edit Nothing                       ->
      let action = actionTable L.!! cursor s
      in  s { mode = Edit $ Just action
            , passageForm = case action of
                              AddPassage -> mkAddPassageForm . passages <$> story s
                              _ -> Nothing
            }
    Edit (Just AddPassage)             -> s
    Edit (Just RemovePassage)          -> s
    Edit (Just (EditPassage Nothing))  -> s
    -- Edit (Just (EditPassage (Just p))) -> s
    _ -> s
