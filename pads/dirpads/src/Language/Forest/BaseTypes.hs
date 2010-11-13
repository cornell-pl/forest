{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances,FlexibleInstances #-}
module Language.Forest.BaseTypes where

import Language.Forest.Generic
import Language.Forest.MetaData
import Language.Forest.Quote
import Language.Pads.Padsc

[forest|
  type Text = File Ptext
  type Binary = File Pbinary
  type Any = File Pbinary
|]


