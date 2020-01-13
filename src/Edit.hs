module Edit where

-- External Imports
import Brick
import Brick.Forms
import qualified Data.Map as M

-- Internal Imports
import Types
import UIHelpers



drawPickAction :: CursorPos -> [Widget Resource]
drawPickAction c = txtWrap <$> prefixCursor c (showAction <$> actionTable)

drawEdit :: Action -> Story -> Form PassageForm e Resource -> Widget Resource
drawEdit AddPassage Story { passages } form = renderForm form
drawEdit RemovePassage s _ = txtWrap "Remove Passage"
drawEdit (EditPassage _) s _ = txtWrap "Edit Passage"

mkAddPassageForm :: PassageForm -> Form PassageForm e Resource
mkAddPassageForm = newForm
                 [ (str "Title: " <+>)   @@= editTextField formPassageTitle TitleField (Just 1)
                 , (str "Passage: " <+>) @@= editTextField formPassage PassageField Nothing
                 ]

                    -- ++ ((checkboxField formChoices) . passageTitle <$> p))


addPassage :: Story
           -> Passage
           -> Story
addPassage story passage =
  Story (storyTitle story)
        (start story)
        (M.insert (uuid passage) passage (passages story))

getTitles :: Story -> [Title]
getTitles = fmap passageTitle . getPassages

getPassages :: Story -> [Passage]
getPassages = M.elems . passages

editPassage :: Passage -> PassageBody -> Passage
editPassage p pbody = p { passage = pbody }

addChoice' :: Passage -> ID Passage -> Passage
addChoice' p choiceId = p { choices = choiceId : choices p }

addChoice :: Passage -> Passage -> Passage
addChoice p choice = addChoice' p $ uuid choice

removeChoice' :: Passage -> ID Passage -> Passage
removeChoice' p r = p { choices = filter (/= r) (choices p) }

removeChoice :: Passage -> Passage -> Passage
removeChoice p r = removeChoice' p $ uuid r



