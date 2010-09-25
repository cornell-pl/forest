{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module Language.Pads.BaseTypes where

import Language.Pads.Source
import Language.Pads.Errors 
import Language.Pads.Generic
import Language.Pads.MetaData
import Language.Pads.CoreBaseTypes
import Language.Pads.Quote

import qualified Data.Char as C
import qualified Data.List as L

[pads| type  Pstringln = constrain x :: PstringSE(: RE "$":) where <| True |> |]

hexStr2Int :: Pos -> (PstringFW, Base_md) -> (Pint, Base_md)
hexStr2Int src_pos (PstringFW s,md) = if good then (Pint (intList2Int ints 0), md)
                                      else (0, mkErrBasePD  (TransformToDstFail "StrHex" s " (non-hex digit)") (Just src_pos))
  where
    hc2int c = if C.isHexDigit c then (C.digitToInt c,True) else (0,False)
    (ints,bools) = unzip (map hc2int s)
    good = (L.and bools) && (length ints > 0)
    intList2Int digits a = case digits of
        []     -> a
        (d:ds) -> intList2Int ds ((16 * a) + d)

int2HexStr :: Int -> (Pint, Base_md) -> (PstringFW, Base_md)
int2HexStr size (Pint x,md) = if (length result == size) && wasPos  then (PstringFW result, md)       
                              else if not wasPos then 
                                   (PstringFW (Prelude.take size result),    
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (Expected positive number)")) Nothing)
                              else (PstringFW (Prelude.take size result),
                                    mkErrBasePD (TransformToSrcFail "StrHex" (show x) (" (too big to fit in "++ (show size) ++" characters)")) Nothing)
  where
   cvt rest a = if rest < 16 then reverse $ (C.intToDigit rest) : a
                else cvt (rest `div` 16) (C.intToDigit (rest `mod` 16) : a)
   (wasPos,x') = if x < 0 then (False, -x) else (True, x)
   temp = cvt x' []
   padding = size - (length temp)
   stutter c n = if n <= 0 then [] else c : (stutter c (n-1))
   result = (stutter '0' padding) ++ temp

[pads| type Phex32FW (size :: Int)  = Trans { PstringFW(: size :)  <=> Pint using (hexStr2Int,int2HexStr size) } |]  
