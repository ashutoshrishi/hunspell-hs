module Language.HunspellSpec where

import           Control.Concurrent      (forkIO, myThreadId)
import           Control.Concurrent.MVar
import           Control.Monad           (forM_, replicateM)
import qualified Data.List               as List
import           Language.Hunspell
import           Test.Hspec

spec :: Spec
spec =
  beforeAll
    (createSpellChecker "dictionaries/en_GB.aff" "dictionaries/en_GB.dic") $
    describe "Hunspell" $ do
      it "suggests corrections" $ \checker ->
        correct checker "speling" "spelling" `shouldReturn` True
      it "checks for correct words" $ \checker -> do
        spell checker "speling" `shouldReturn` False
        spell checker "color" `shouldReturn` False
      it "provides a stemming utility" $ \checker ->
        stem checker "spelling" `shouldReturn` ["spelling", "spell"]

      it "can add and remove a word" $ \checker -> do
        spell checker "blahblah" `shouldReturn` False
        add checker "blahblah"
        spell checker "blahblah" `shouldReturn` True
        remove checker "blahblah"
        spell checker "blahblah" `shouldReturn` False

      it "multi word suggestions" $ \checker ->
        suggestions checker (fst <$> checks) `shouldReturn` checkResults

      it "suggestions thread safety" $ \checker -> do
        let jobs = List.replicate 10 (fst <$> checks)
        let n = length jobs
        vars <- replicateM n newEmptyMVar
        forM_ (zip vars jobs) $ \(var, ws) ->
          forkIO $ suggestions checker ws >>= putMVar var
        mapM readMVar vars `shouldReturn` List.replicate 10 checkResults

      it "suggest thread safety" $ \checker -> do
        let jobs = take 10 $ List.cycle checks
        let n = length jobs
        vars <- replicateM n newEmptyMVar
        forM_ (zip vars jobs) $ \(var, (wrong, right)) ->
          forkIO $ correct checker wrong right >>= putMVar var
        rs <- mapM readMVar vars
        List.all (== True) rs `shouldBe` True


checks :: [(String, String)]
checks = [ ("speling", "spelling"),
           ("helllo", "hello"),
           ("badd", "bad"),
           ("realize", "realise")
         ]

checkResults :: [(String, [String])]
checkResults =
  [ ("badd", ["bad", "add", "baddy", "bade", "band"])
  , ("helllo", ["hello", "hell lo", "hell-lo", "hellhole", "hell"])
  , ("speling", ["spieling", "spelling", "spewing", "peeling", "splining"])
  ]


correct :: SpellChecker -> String -> String -> IO Bool
correct checker wrong right  = List.elem right . take 5 <$> suggest checker wrong
