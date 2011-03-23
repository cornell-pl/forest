{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.ZipNTar where

import Language.Pads.Padsc
import Language.Forest.Forestc
import Language.Pads.GenPretty

import System.IO.Unsafe (unsafePerformIO)


[pads| type Score_t  = (Pint, ',', Pstringln)
       type Scores_t = [Line Score_t]                  |]


[forest| type Scores_f  = File Scores_t
         type NestedScores_d = Directory { nested is [ d :: Scores_f | d <- matches (RE "scores[0-9]") ] }
         type Collect_d (s::String) = Directory { collect is <|"collect" ++ s |> :: NestedScores_d }
         type Scores_d  = Directory 
                              {   scores  is "scores.gz"       :: Gzip Scores_f
                              ,  collect1  is "collect1.tar"    :: Tar (Collect_d "1")
                              ,  collect2  is "collect2.tar.gz" :: Gzip (Tar (Collect_d "2"))   
                              } 
         type Score2 = Directory { scores1 :: Scores_f
                                 , scores2 :: Scores_f
                                 }
         type Collect = Directory { collect1' is "collect1"  :: Score2 }
         type CollectTar = Tar Collect
|] 

 
mkPrettyInstance ''Scores_d
mkPrettyInstance ''Scores_d_md

collectTar_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/zipntarSimple/collect1.tar"

doZipSimpleTest' :: IO Manifest
doZipSimpleTest' = do 
 { pResult <- collectTar_load collectTar_dir
 ; emptyManifest <- newManifest
 ; rawManifest <- collectTar_updateManifest pResult emptyManifest
 ; validateManifest rawManifest
 }

doZipSimpleTest :: IO Manifest
doZipSimpleTest = do 
 { pResult <- collectTar_load collectTar_dir
 ; generateManifest pResult
 }

clip_path = "/Users/kfisher/pads/dirpads/src/Examples/data/"
scores_dir = "/Users/kfisher/pads/dirpads/src/Examples/data/zipntar"

(scores_rep, scores_fmd) = unsafePerformIO $ scores_d_load scores_dir
pretty_scores = putStrLn(pretty 80 (ppr scores_rep))

doZipTest :: IO Manifest
doZipTest = do 
 { pResult <- scores_d_load scores_dir
 ; generateManifest pResult
 }


doManifest' :: Manifest -> IO ()
doManifest' manifest = 
   storeManifestAt' "/Users/kfisher/temp" clip_path manifest


doManifest :: Manifest -> IO ()
doManifest manifest = 
   storeManifestAt "/Users/kfisher/temp" manifest
