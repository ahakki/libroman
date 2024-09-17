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

import Foreign.C ( newCString, CInt(..), CString )
import GHC.IO (unsafePerformIO)

foreign export ccall cToRoman :: CInt -> CString
foreign export ccall cFromRoman :: CString -> CInt

cToRoman :: CInt -> CString
cToRoman i = unsafePerformIO $ 
    newCString $ show (fromIntegral (fromIntegral i ::Int) ::RomanNumeral) ::CString

cFromRoman :: CString -> CInt
cFromRoman _ = 12

