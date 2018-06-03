import Language.Hunspell

main :: IO ()
main = do
  h <-
    createSpellChecker
      "dictionaries/en_GB.aff"
      "dictionaries/en_GB.dic"
  rs <- suggest h "colour"
  print rs
