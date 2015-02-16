{-# LANGUAGE ViewPatterns #-}

module Language.Forest.FS.NILFS where

import Language.Forest.FS.FSDelta
import Data.List
import Language.Forest.IO.Shell
import qualified Data.Map as Map
import System.FilePath
import Data.List.Split

diffNILFS :: FilePath -> FilePath -> Int -> Int -> IO FSTreeDelta
diffNILFS mountpoint device cp1 cp2 = do
	diffLog <- sudoShellCommand $ "nilfs-diff " ++ device ++ " " ++ show cp1 ++ ".." ++ show cp2
	let diffLines = lines diffLog
	let td = foldl' (parseLine mountpoint) Map.empty diffLines
	return $ unfocusFSTreeDelta (relativeFSTreeDelta td) mountpoint

parseLine :: FilePath -> FSTreeDelta -> String -> FSTreeDelta
parseLine mountpoint td (modified -> Just path) = appendToFSTreeDelta (ChgAttrs path $ mountpoint </> makeRelative "/" path) td
parseLine mountpoint td (added -> Just to) = appendToFSTreeDelta (Add to $ mountpoint </> makeRelative "/" to) td
parseLine mountpoint td (removed -> Just from) = appendToFSTreeDelta (Rem from) td
parseLine mountpoint td (moved -> Just (from,to)) = appendToFSTreeDelta (Move from to $ mountpoint </> makeRelative "/" to) td
parseLine mountpoint td line = error $ "parseLine: failed to parse nilfs-diff log entry " ++ show line

modified ('M':' ':path) = Just path
modified _ = Nothing
added ('+':' ':to) = Just to
added _ = Nothing
removed ('-':' ':from) = Just from
removed _ = Nothing
moved ('R':' ':e) = case splitOn "->" e of
	[from,to] -> Just (from,to)
	otherwise -> Nothing
moved _ = Nothing