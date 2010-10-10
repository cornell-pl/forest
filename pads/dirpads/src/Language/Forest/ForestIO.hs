module Language.Forest.ForestIO where

import Language.Pads.Padsc
import Language.Forest.MetaData
import Language.Forest.Generic
import System.FilePath.Posix
import System.Cmd

import Data.Data


fileload :: Pads pads md => FilePath -> IO (pads, (Forest_md, md))
fileload path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile path
   return (rep, (fmd, md))

fileload1 :: Pads1 arg pads md => arg -> FilePath -> IO (pads, (Forest_md, md))
fileload1 arg path = do
   fmd       <- getForestMD path
   (rep, md) <- parseFile1 arg path
   return (rep, (fmd, md))


--XXX need to handle possible failues; although we can assume that file exists.
gunzip :: FilePath -> IO(FilePath)  
gunzip path = do
  { let (root, ext) = splitExtension path
  ; let cmd = "gzip " ++ path ++ " -c > " ++ root 
  ; putStrLn cmd
  ; exitCode <- system cmd
  ; putStrLn root
  ; return root
  }

gzip :: FilePath -> IO()
gzip path = do
 { exitCode <- system ("gzip " ++ path)
 ; return ()
 }

gzipload'  :: (ForestMD md, Data rep) =>  (FilePath -> IO (rep, md)) -> FilePath -> IO (rep, md)
gzipload' load path = checkPath path (do 
  { md <- getForestMD path
  ; newpath <- gunzip path
  ; (rep, md_zip) <- load newpath
  ; let md' = replace_fmd_header md_zip md
--  ; gzip newpath
  ; return (rep, md')
  })


gzipload :: Forest rep md =>  FilePath -> IO(rep, md)
gzipload path = checkPath path (do 
  { md <- getForestMD path
  ; newpath <- gunzip path
  ; (rep, md_zip) <- load newpath
  ; let md' = replace_fmd_header md_zip md
  ; gzip newpath
  ; return (rep, md')
  })
