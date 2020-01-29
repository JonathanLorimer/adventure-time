{-# LANGUAGE TupleSections #-}

module Mode.Edit where

-- External Imports
import           Brick
import           Brick.Forms
import           Data.Map       (Map)
import qualified Data.Map       as M
import           Data.Maybe
import           Lens.Micro
import           Lens.Micro.GHC ()

-- Internal Imports
import           Types
import           UI.Helpers


drawPickAction :: CursorPos -> [Widget Resource]
drawPickAction c = txtWrap <$> prefixCursor c (showAction <$> actionTable)

drawEdit :: Action -> Story -> Form PassageForm e Resource -> Widget Resource
drawEdit AddPassage _ form   = renderForm form
drawEdit RemovePassage _ _   = txtWrap "Remove Passage"
drawEdit (EditPassage _) _ _ = txtWrap "Edit Passage"

mkAddPassageForm :: Map (ID Passage) Passage ->  Form PassageForm e Resource
mkAddPassageForm ps = setFormConcat vBox $ newForm
   ( groupBorder "Passage Content"
      [ 2 |> (str "Title:   " <+>) @@= editTextField formPassageTitle TitleField (Just 1)
      , 2 |> (str "Passage: " <+>) @@= editTextField formPassage PassageField Nothing
      ]
  ++ groupBorder "Add Passage Choices"
      ((2 |>) . mkCheckboxPassage ps <$> M.keys fc)
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



