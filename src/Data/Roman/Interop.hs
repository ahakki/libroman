{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Roman.Interop 
    ( readCToRoman
    , readCFromRoman
    , hsToCRoman
    , hsFromCRoman
    ) where

import Foreign.C (CUInt(..), CString )
import Foreign.C.String ( peekCString )
import GHC.IO ( unsafePerformIO )

import Data.Roman
import Data.Roman.Interop.C (cFromRoman, cToRoman)



readCToRoman 
    :: Roman a 
    => CString 
    -> a
readCToRoman cStr = unsafePerformIO $ 
    peekCString cStr >>=
    (\y -> return (toRoman (read y::RomanNumeral))) 

readCFromRoman :: Integral a => CUInt -> a
readCFromRoman = 
    fromIntegral 

hsToCRoman :: Roman a => a -> CString
hsToCRoman a = 
    cToRoman (fromRoman a)
    
hsFromCRoman :: Roman b =>CString -> b
hsFromCRoman a =
    (toRoman . cFromRoman) a  