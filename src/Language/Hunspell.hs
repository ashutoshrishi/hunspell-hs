{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
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

import           Control.Concurrent.MVar
import           Control.DeepSeq         (NFData)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Generics            (Generic)

-- |Initialise a new `SpellChecker` with the `.aff` and `.dic`
-- dictionary files.
createSpellChecker :: String -- ^ .aff file path
                   -> String -- ^ .dic file path
                   -> IO SpellChecker
createSpellChecker affpath dicpath = do
  withCString affpath $ \aff ->
    withCString dicpath $ \dic -> do
      ptr <- newForeignPtr hunspellDestroy (hunspellCreate aff dic)
      SpellChecker <$> newMVar ptr

-- |Check for correctness of a word.
spell :: SpellChecker -> String -> IO Bool
spell SpellChecker{hunPtr=mvar} word = do
  withCString word $ \word' -> do
    handlePtr <- takeMVar mvar
    result <- withForeignPtr handlePtr (flip hunspellSpell word')
    putMVar mvar handlePtr
    return (if result == 0 then False else True)

-- |Return spelling suggestions for a word.
suggest :: SpellChecker -> String -> IO [String]
suggest SpellChecker {hunPtr = mvar} word = do
  withCString word $ \word' ->
    alloca $ \ptr -> do
      handlePtr <- takeMVar mvar
      len <-
        withForeignPtr handlePtr $ \handle -> hunspellSuggest handle ptr word'
      putMVar mvar handlePtr
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

data SpellChecker = SpellChecker
  { hunPtr :: MVar (ForeignPtr Hunspell)
  } deriving (Generic, NFData)

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
