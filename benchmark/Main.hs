module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Data.Maybe        (fromMaybe)
import           Language.Hunspell

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(~(Checker checker), _para) ->
      bench "single suggestion" $ nfIO (runSuggestion checker)
  , env setupEnv $ \ ~(~(Checker checker), para) ->
      bench "paragraph suggestion" $ nfIO (runSuggestions checker para)
  ]

runSuggestion :: SpellChecker -> IO [String]
runSuggestion checker = suggest checker "speiling"

runSuggestions :: SpellChecker -> String -> IO String
runSuggestions checker p =
  unwords <$> mapM (autoCorrect checker) (words p)

-- | Auto correct a single word from the top suggestion.
autoCorrect :: SpellChecker -> String -> IO String
autoCorrect checker word = fromMaybe word . head' <$> suggest checker word

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

newtype Checker = Checker SpellChecker

instance NFData Checker where
  rnf (Checker c) = seq c ()

setupEnv :: IO (Checker, String)
setupEnv = (,) . Checker <$> createSpellChecker "dictionaries/en_GB.aff" "dictionaries/en_GB.dic"
                         <*> readFile "benchmark/spell-testset1.txt"
