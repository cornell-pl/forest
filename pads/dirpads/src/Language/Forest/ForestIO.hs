module Language.Forest.ForestIO where

fileload path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile   path
   return (rep, (fmd, md))
