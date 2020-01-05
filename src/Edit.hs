module Edit where

import Types
import qualified Data.Map as M
import Data.UUID (UUID)

data Action = AddPassage
            | RemovePassage
            | EditPassage

edit :: Passage -> IO ()
edit p = undefined

addPassage :: Story
           -> UUID
           -> Title
           -> PassageBody
           -> [UUID]
           -> Story
addPassage story pid title pb ids =
  Story (storyTitle story)
        (start story)
        (M.insert pid newPassage (passages story))
    where
      newPassage = Passage pid title pb ids

getTitles :: Story -> [Title]
getTitles = fmap passageTitle . getPassages

getPassages :: Story -> [Passage]
getPassages = fmap snd . M.toList . passages

editPassage :: Passage -> PassageBody -> Passage
editPassage p pbody = p { passage = pbody }

addChoice' :: Passage -> UUID -> Passage
addChoice' p choiceId = p { choices = choiceId : choices p }

addChoice :: Passage -> Passage -> Passage
addChoice p choice = addChoice' p $ uuid choice

removeChoice' :: Passage -> UUID -> Passage
removeChoice' p r = p { choices = filter (/= r) (choices p) }

removeChoice :: Passage -> Passage -> Passage
removeChoice p r = removeChoice' p $ uuid r



