-- C.hs
{- |
Module      :  $Header$
Description :  Roman Numerals
Copyright   :  (c) Alexander Hakki
License     :  BSD3

Maintainer  :  ahk@ahakki.xyz
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Roman.Interop.C where

import Data.Roman

import Foreign.C ( newCString, CUInt(..), CString )
import Foreign.C.String ( peekCString )
import GHC.IO ( unsafePerformIO )

foreign export ccall cToRoman :: CUInt -> CString
foreign export ccall cFromRoman :: CString -> CUInt

cToRoman :: CUInt -> CString
cToRoman i = (unsafePerformIO . newCString) $ 
        show (fromIntegral (fromIntegral i ::Int) ::RomanNumeral) ::CString

cFromRoman :: CString -> CUInt
cFromRoman input  =  unsafePerformIO $
    peekCString input 
    >>= \y -> return (fromIntegral (read y :: RomanNumeral) ::CUInt)

    -- x <- peekCString input
    -- let y =(read x ::RomanNumeral)
    -- return (fromIntegral y ::CUInt)

--fromIntegral ( read (unsafePerformIO peekCString input ::String)::Romokoijkj    * Defaulting the type variable `a0' to type `Integer' in the following constraints     * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints    * Defaulting the type variable `a0' to type `Integer' in the following constraints