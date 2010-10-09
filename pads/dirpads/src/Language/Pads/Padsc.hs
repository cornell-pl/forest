{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, DeriveDataTypeable, 
             ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             UndecidableInstances, NamedFieldPuns, RecordWildCards, StandaloneDeriving   #-}
module Language.Pads.Padsc (
     module Language.Pads.Source,
     module Language.Pads.RegExp,
     module Language.Pads.Errors,
     module Language.Pads.PadsParser,
     module Language.Pads.MetaData,
     module Language.Pads.Generic,
     module Language.Pads.CoreBaseTypes,
     module Language.Pads.Quote,
     module Language.Pads.BaseTypes,
     module Text.PrettyPrint.Mainland
  ) 
  where


import Language.Pads.Source 
import Language.Pads.RegExp
import Language.Pads.Errors 
import Language.Pads.PadsParser
import Language.Pads.MetaData
import Language.Pads.Generic
import Language.Pads.CoreBaseTypes
import Language.Pads.Quote
import Language.Pads.BaseTypes

import Data.Data
import Text.PrettyPrint.Mainland hiding (line,  dot)



{- Fix these should be reexported -}
parseAllS = Language.Pads.PadsParser.parseAllS
numErrors = Language.Pads.MetaData.numErrors




  





