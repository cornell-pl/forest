{-# LANGUAGE ViewPatterns #-}

-- Module for performing and parsing the result of a OS diff between two paths

module Language.Forest.FS.Diff where

import Control.Monad
import Language.Forest.IO.Utils
import Language.Forest.IO.Shell
import System.FilePath.Posix
import Language.Forest.FS.FSRep
import Language.Forest.IC.ICRep
import Language.Forest.FS.FSDelta
import Text.Regex
import Data.Map (Map(..))
import qualified Data.Map as Map
--import Control.Monad.IO.Class
import Safe
import Control.Monad.Incremental

-- * filesystem tree difference

-- | Returns a filesystem delta tree reporting the (recursive) difference between two paths
diffPath :: FilePath -> FilePath -> IO FSTreeDelta
diffPath dskpath1 dskpath2 = do
	difflines <- runShellCommand $ "diff -qr " ++ dskpath1 ++ " " ++ dskpath2
	foldM diffLine Map.empty $ lines difflines
  where
--	diffLine :: FSRep fs => FSTreeDelta -> String -> ForestO fs FSTreeDelta
	diffLine td (matchRegex onlyInRegex -> Just [path,file]) = if isParentPathOf dskpath1 path
		then do
			let d = Rem (path </> file)
			return $ appendToFSTreeDelta d td
		else if isParentPathOf dskpath2 path
			then do
				let d = Add (path </> file) (path </> file)
				return $ appendToFSTreeDelta d td
			else error $ "unsupported diff path " ++ show path
	diffLine td (matchRegex differRegex -> Just [file1,file2]) = do
		let d1 = Rem file1
		let d2 = Add file2 file2
		return $ appendToFSTreeDelta d2 $ appendToFSTreeDelta d1 td
	diffLine td str = error $ "unsupported diff result " ++ show str

onlyInRegex = mkRegex "Only in ([^\n]+): ([^\n]+)"
differRegex  = mkRegex "Files ([^\n]+) and ([^\n]+) differ"

focusDiffFSTree :: FSRep fs => FSTree fs -> FilePath -> FSTree fs -> FilePath -> ForestM fs FSTreeDeltaNodeMay
focusDiffFSTree tree path tree' path' = do
	old <- pathInTree path tree
	new <- pathInTree path' tree'
	df <- forestIO $ diffPath old new
	return $ focusFSTreeDeltaByRelativePathMay df new



