module Main where

import           Control.DeepSeq
import           Criterion.Main
import           Language.Hunspell

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(Env checker) ->
      bench "single suggestion" $ nfIO (runSuggestions checker) ]

runSuggestions :: SpellChecker -> IO [String]
runSuggestions checker = suggest checker "speiling"

newtype Env = Env SpellChecker

instance NFData Env where
  rnf (Env checker) = seq checker ()

setupEnv :: IO Env
setupEnv = Env <$> createSpellChecker "dictionaries/en_GB.aff" "dictionaries/en_GB.dic"
