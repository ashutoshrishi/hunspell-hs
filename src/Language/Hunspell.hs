{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      : Language.Hunspell
-- Description : Thread-safe FFI bindings to the Hunspell library
-- Copyright   : (c) Ashutosh Rishi Ranjan, 2023
-- Maintainer  : arishiranjan at gmail
module Language.Hunspell
  ( -- * Usage
    -- $usage

    -- * Usage with threads
    -- $threads

    -- * Hunspell FFI requirements
    -- $ffi

    -- * Creation
    createSpellChecker,
    SpellChecker,
    withHandle,

    -- * Hunspell API mappings
    spell,
    suggest,
    stem,
    add,
    remove,

    -- * More compound functionality
    suggestions,
  )
where

import Control.Concurrent.STM
import Control.Monad (foldM)
import qualified Data.ByteString as B
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- $threads
--
-- Initialise a 'SpellChecker' instance before you spawn your
-- threads. After which, a SpellChecker instance can be used
-- safely across the threads.

-- $usage
--
-- The functions exported try to match the Hunspell API one-to-one.
--
-- >>> checker <- createSpellChecker "en_GB.aff" "en_GB.buf"
-- >>> suggest checker "splling"
-- ["spelling", ...]

-- $ffi
--
-- This library expects that GHC can find the Hunspell shared library.
--
-- On Linux you need to install the @libhunspell-dev@ package from
-- your package manager:
--
-- > sudo apt-get install libhunspell-dev
--
-- On Macos you can install @hunspell@ from brew since the default
-- package config looks into the Homebrew include and lib dirs:
--
-- > brew install hunspell

-- | Initialise a new 'SpellChecker' with the '.aff' and '.dic'
--  dictionary files.
createSpellChecker ::
  -- | .aff file path
  FilePath ->
  -- | .dic file path
  FilePath ->
  IO SpellChecker
createSpellChecker affpath dicpath =
  withCString affpath $ \aff ->
    withCString dicpath $ \dic -> do
      ptr <- newForeignPtr hunspellDestroy (hunspellCreate aff dic)
      SpellChecker ptr <$> newTMVarIO ptr

-- | Check for correctness of a word.
spell :: SpellChecker -> String -> IO Bool
spell checker word =
  withCString word $ \cword ->
    (/= 0) <$> withHandle checker (`hunspellSpell` cword)

-- | Check for correctness of a word (ByteString variant)
spellBS :: SpellChecker -> B.ByteString -> IO Bool
spellBS checker word =
  B.useAsCString word $ \cword ->
    (/= 0) <$> withHandle checker (`hunspellSpell` cword)

-- | Return spelling suggestions for a word.
suggest :: SpellChecker -> String -> IO [String]
suggest checker word =
  withCString word $ \word' ->
    alloca $ \resultsPtr -> do
      len <-
        withHandle checker $ \handle -> hunspellSuggest handle resultsPtr word'
      peekResults checker len resultsPtr

-- | Return spelling suggestions for a word.
suggestBS :: SpellChecker -> B.ByteString -> IO [B.ByteString]
suggestBS checker word =
  B.useAsCString word $ \word' ->
    alloca $ \resultsPtr -> do
      len <-
        withHandle checker $ \handle -> hunspellSuggest handle resultsPtr word'
      peekResultsBS checker len resultsPtr

-- | Hunspell stemmer function
stem :: SpellChecker -> String -> IO [String]
stem checker word =
  withCString word $ \word' ->
    alloca $ \resultsPtr -> do
      len <-
        withHandle checker $ \handle -> hunspellStem handle resultsPtr word'
      peekResults checker len resultsPtr

-- | Add a word to the runtime dictionary.
add :: SpellChecker -> String -> IO ()
add checker word =
  withCString word $ \word' -> withHandle checker (`hunspellAdd` word')

-- | Remove a word from the runtime dictionary.
remove :: SpellChecker -> String -> IO ()
remove checker word =
  withCString word $ \word' -> withHandle checker (`hunspellRemove` word')

-- | Multi word spelling suggestions. Takes a list of possibly
-- incorrect spellings, and returns the spelling corrections for all
-- the incorrect spellings. Internally uses both `spell` and
-- `suggest`.
suggestions :: SpellChecker -> [String] -> IO [(String, [String])]
suggestions checker = foldM collect []
  where
    collect acc word =
      withCString word $ \cword -> do
        correct <- (/= 0) <$> withHandle checker (`hunspellSpell` cword)
        if correct
          then return acc
          else alloca $ \resultsPtr -> do
            len <-
              withHandle checker $ \handle -> hunspellSuggest handle resultsPtr cword
            results <- peekResults checker len resultsPtr
            return ((word, results) : acc)

--------------------------------------------------------------------
-- Internal                                                       --
--------------------------------------------------------------------

-- | Atomically perform an action with the foreign Hunspell
-- instance. Uses a mutex type lock for synchronisation and safety.
withHandle :: SpellChecker -> (Hunhandle -> IO a) -> IO a
withHandle SpellChecker {hunPtrVar = tmvar} action = do
  handlePtr <- atomically $ takeTMVar tmvar
  result <- withForeignPtr handlePtr action
  atomically $ putTMVar tmvar handlePtr
  return result

-- | Read the suggestion results (encoded as a array of length
-- 'len' containing `CString` words) from a 'resultsPtr'. The pointer
-- is cleaned up after reading.
peekResults :: SpellChecker -> CInt -> Ptr (Ptr CString) -> IO [String]
peekResults SpellChecker {hunPtr = handlePtr} len resultsPtr =
  withForeignPtr handlePtr $ \handle -> do
    results <- peekWords len resultsPtr
    hunspellFreeList handle resultsPtr len
    return results

-- | Read the suggestion results (encoded as a array of length
-- 'len' containing `CString` words) from a 'resultsPtr'. The pointer
-- is cleaned up after reading.
peekResultsBS :: SpellChecker -> CInt -> Ptr (Ptr CString) -> IO [B.ByteString]
peekResultsBS SpellChecker {hunPtr = handlePtr} len resultsPtr =
  withForeignPtr handlePtr $ \handle -> do
    results <- peekWordsBS len resultsPtr
    hunspellFreeList handle resultsPtr len
    return results

peekWords :: CInt -> Ptr (Ptr CString) -> IO [String]
peekWords 0 _ = return []
peekWords len ptr = do
  arrayPtr <- peek ptr
  cstrings <- peekArray (fromIntegral len) arrayPtr
  mapM peekCString cstrings

peekWordsBS :: CInt -> Ptr (Ptr CString) -> IO [B.ByteString]
peekWordsBS 0 _ = return []
peekWordsBS len ptr = do
  arrayPtr <- peek ptr
  cstrings <- peekArray (fromIntegral len) arrayPtr
  mapM B.packCString cstrings

--------------------------------------------------------------------
-- Types                                                          --
--------------------------------------------------------------------

-- | Opaque Hunspell struct
data Hunspell

-- | Ptr to the Hunspell struct
type Hunhandle = Ptr Hunspell

-- | Main type to hold a 'TMVar' wrapped reference to the Hunspell
-- handle pointer.
data SpellChecker = SpellChecker
  { hunPtr :: ForeignPtr Hunspell,
    hunPtrVar :: TMVar (ForeignPtr Hunspell)
  }

--------------------------------------------------------------------
-- FFI                                                            --
--------------------------------------------------------------------

foreign import ccall "Hunspell_create"
  hunspellCreate ::
    CString -> CString -> Hunhandle

foreign import ccall "&Hunspell_destroy"
  hunspellDestroy ::
    FunPtr (Hunhandle -> IO ())

foreign import ccall "Hunspell_free_list"
  hunspellFreeList ::
    Hunhandle -> Ptr (Ptr CString) -> CInt -> IO ()

foreign import ccall "Hunspell_spell"
  hunspellSpell ::
    Hunhandle -> CString -> IO CInt

foreign import ccall "Hunspell_suggest"
  hunspellSuggest ::
    Hunhandle -> Ptr (Ptr CString) -> CString -> IO CInt

foreign import ccall "Hunspell_stem"
  hunspellStem ::
    Hunhandle -> Ptr (Ptr CString) -> CString -> IO CInt

foreign import ccall "Hunspell_add" hunspellAdd :: Hunhandle -> CString -> IO ()

foreign import ccall "Hunspell_remove"
  hunspellRemove ::
    Hunhandle -> CString -> IO ()
