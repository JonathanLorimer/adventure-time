{-# LANGUAGE TupleSections #-}

module Mode.Edit where

-- External Imports
import           Brick
import           Brick.Forms
import qualified Brick.Widgets.Center as C
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Lens.Micro
import           Lens.Micro.GHC       ()

-- Internal Imports
import           Types
import           UI.Helpers


drawPickAction :: CursorPos -> [Widget Resource]
drawPickAction c = txtWrap <$> prefixCursor c (showAction <$> actionTable)

drawEdit :: Action -> Story -> Form PassageForm e Resource -> Widget Resource
drawEdit AddPassage Story { passages } form = renderForm form
                                           <=> C.center (txt "Submit")
drawEdit RemovePassage s _                  = txtWrap "Remove Passage"
drawEdit (EditPassage _) s _                = txtWrap "Edit Passage"

mkAddPassageForm :: Map (ID Passage) Passage ->  Form PassageForm e Resource
mkAddPassageForm ps = setFormConcat vBox $ newForm
   ( groupBorder "Passage Content"
      [ 1 |> (str "Title:   " <+>)   @@= editTextField formPassageTitle TitleField (Just 1)
      , 2 |> (str "Passage: " <+>) @@= editTextField formPassage PassageField Nothing
      ]
  ++ groupBorder "Add Passage Choices"
      ((1 |>) . (mkCheckboxPassage ps) <$> M.keys fc)
  ++ [checkboxCustomField ' ' ' ' ' ' formSubmit SubmitField "Submit"]
   ) pf
    where
      fc = fmap (const False) ps
      pf = PassageForm { _formPassageTitle = ""
                       , _formPassage = ""
                       , _formChoices = fc
                       , _formSubmit = False
                       }

mkCheckboxPassage :: Map (ID Passage) Passage
                  -> ID Passage
                  -> PassageForm -> FormFieldState PassageForm e Resource
mkCheckboxPassage m pid = checkboxField
   (formChoices . at pid . non True)
   (ChoicesField (show pid))
   (passageTitle (fromJust $ M.lookup pid m))

-- addPassage :: Story
--            -> Passage
--            -> Story
-- addPassage story passage =
--   Story (storyTitle story)
--         (start story)
--         (M.insert (uuid passage) passage (passages story))

-- getTitles :: Story -> [Title]
-- getTitles = fmap passageTitle . getPassages

-- getPassages :: Story -> [Passage]
-- getPassages = M.elems . passages

-- editPassage :: Passage -> PassageBody -> Passage
-- editPassage p pbody = p { passage = pbody }
-- addChoice' :: Passage -> ID Passage -> Passage
-- addChoice' p choiceId = p { choices = choiceId : choices p }

-- addChoice :: Passage -> Passage -> Passage
-- addChoice p choice = addChoice' p $ uuid choice

-- removeChoice' :: Passage -> ID Passage -> Passage
-- removeChoice' p r = p { choices = filter (/= r) (choices p) }

-- removeChoice :: Passage -> Passage -> Passage
-- removeChoice p r = removeChoice' p $ uuid r



