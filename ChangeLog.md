# Changelog for hunspell-hs

## [0.2.0.0] - 2018-08-27

- Added a new api function `suggestions` to generate spelling
  corrections on a list of words.
- Fixed linux hunspell library name being bound to a version #3 @typetetris
- Including the `/dictionaries` folder in the release #3 @typetetris


## [0.1.0.0] - 2018-05-04

- Added Hunspell bindings for spell, suggest, stem, add, remove functions.
- Added `TMVar` based thread locking for access to the hunspell pointer.
