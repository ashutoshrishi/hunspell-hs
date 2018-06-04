module Language.HunspellSpec where

import qualified Data.List         as List
import           Language.Hunspell
import           Test.Hspec

spec :: Spec
spec = do
  beforeAll
    (createSpellChecker "dictionaries/en_GB.aff" "dictionaries/en_GB.dic") $ do
    describe "Hunspell" $ do
      it "suggests corrections" $ \checker -> do
        correct checker "speling" "spelling" `shouldReturn` True
      it "checks for correct words" $ \checker -> do
        spell checker "speling" `shouldReturn` False
        spell checker "color" `shouldReturn` False
      it "provides a stemming utility" $ \checker -> do
        stem checker "spelling" `shouldReturn` ["spelling", "spell"]

correct :: SpellChecker -> String -> String -> IO Bool
correct checker wrong right  = List.elem right <$> suggest checker wrong
