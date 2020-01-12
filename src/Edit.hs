module Edit where

import Types
import qualified Data.Map as M

data Action = AddPassage
            | RemovePassage
            | EditPassage
            deriving (Eq, Ord, Show, Enum)
edit :: Passage -> IO ()
edit = undefined

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



