{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Language.Hunspell
Description : Thread-safe bindings to the Hunspell library
Copyright   : (c) Ashutosh Rishi Ranjan, 2018
Maintainer  : ashutoshrishi92 at gmail
-}
module Language.Hunspell
  ( -- * Initialisation
      createSpellChecker
    -- * Hunspell API mappings
    , spell, suggest
    -- * Types
    , SpellChecker
  ) where

import           Control.Concurrent.STM
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

-- |Initialise a new `SpellChecker` with the `.aff` and `.dic`
-- dictionary files.
createSpellChecker :: String -- ^ .aff file path
                   -> String -- ^ .dic file path
                   -> IO SpellChecker
createSpellChecker affpath dicpath = do
  withCString affpath $ \aff ->
    withCString dicpath $ \dic -> do
      ptr <- newForeignPtr hunspellDestroy (hunspellCreate aff dic)
      SpellChecker <$> atomically (newTMVar ptr)

-- |Check for correctness of a word.
spell :: SpellChecker -> String -> IO Bool
spell SpellChecker{hunPtr=tmvar} word = do
  withCString word $ \word' -> do
    handlePtr <- atomically $ takeTMVar tmvar
    result <- withForeignPtr handlePtr (flip hunspellSpell word')
    atomically $ putTMVar tmvar handlePtr
    return (if result == 0 then False else True)

-- |Return spelling suggestions for a word.
suggest :: SpellChecker -> String -> IO [String]
suggest SpellChecker {hunPtr = tmvar} word = do
  withCString word $ \word' ->
    alloca $ \ptr -> do
    handlePtr <- atomically $ takeTMVar tmvar
    len <-
      withForeignPtr handlePtr $ \handle -> hunspellSuggest handle ptr word'
    atomically $ putTMVar tmvar handlePtr
    if len == 0
      then return []
      else do
          arrayPtr <- peek ptr
          cstrings <- peekArray (fromIntegral len) arrayPtr
          results <- mapM peekCString cstrings
          withForeignPtr handlePtr $ \handle -> hunspellFreeList handle ptr len
          return results


-- | Opaque Hunspell struct
data Hunspell

-- | Ptr to the Hunspell struct
type Hunhandle = Ptr Hunspell

-- | Main type to hold a `MVar` wrapped reference to the Hunspell
-- handle pointer.
data SpellChecker = SpellChecker
  { hunPtr :: TMVar (ForeignPtr Hunspell)
  }

foreign import ccall "Hunspell_create" hunspellCreate
  :: CString -> CString -> Hunhandle

foreign import ccall "&Hunspell_destroy" hunspellDestroy
  :: FunPtr (Hunhandle -> IO ())

foreign import ccall "Hunspell_free_list" hunspellFreeList
  :: Hunhandle -> Ptr (Ptr CString) -> CInt -> IO ()

foreign import ccall "Hunspell_spell" hunspellSpell
  :: Hunhandle -> CString -> IO CInt

foreign import ccall "Hunspell_suggest" hunspellSuggest
  :: Hunhandle -> Ptr (Ptr CString) -> CString -> IO CInt
