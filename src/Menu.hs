module Menu where

import Brick
import Types
import UI.Helpers

drawStories :: CursorPos -> [Story] -> [Widget Resource]
drawStories _ [] = [txtWrap . ("➤ " <>) $ "create story"]
drawStories p ss = txtWrap <$> prefixCursor p (showTitle . storyTitle <$> ss)

drawPickMode :: CursorPos -> [Widget Resource]
drawPickMode 0 = [txtWrap "➤ Edit Mode", txtWrap "  Play Mode"]
drawPickMode 1 = [txtWrap "  Edit Mode", txtWrap "➤ Play Mode"]
drawPickMode _ = error "should not be able to get here"
