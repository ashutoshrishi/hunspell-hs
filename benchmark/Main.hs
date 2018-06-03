module Main where

import Language.Hunspell
import Criterion.Main

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(checker) ->
      bench "suggestions" $ nfIO (runSuggestions checker) ]

runSuggestions :: SpellChecker -> IO [String]
runSuggestions checker = suggest checker "speiling"

setupEnv :: IO SpellChecker
setupEnv = createSpellChecker "dictionaries/en_GB.aff" "dictionaries/en_GB.dic"
