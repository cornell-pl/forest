{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.Forest.IO.Shell where

import Data.Typeable
import Data.Text (Text(..))
import qualified Data.Text as T
import Data.String
import Control.Monad
import System.Exit
import System.Process
import System.IO
import System.Directory
import Safe
import Data.Maybe
import System.Posix.Process
import Control.Monad.Incremental.Draw
import Control.Concurrent
import Data.UUID
import Data.UUID.V1

-- gets the physical device on which a path is mounted (linux-specific)
pathDevice :: String -> IO String
pathDevice path = liftM (head . words) $ runShellCommand $ "/usr/sbin/grub-probe --target=device "++ show path

-- gets the path where a given device is mounted
devicePath :: String -> IO String
devicePath = liftM (fromJustNote "devicePath") . devicePathMay

mergePDFsTo :: [String] -> String -> IO ExitCode
mergePDFsTo pdfs to = runShellCommand_ $ "pdftk " ++ unwords pdfs ++ " cat output " ++ to

devicePathMay :: String -> IO (Maybe String)
devicePathMay device = liftM (headMay . words) $ runShellCommand $ "mount | egrep "++show device++" | awk '{print $3}'"

-- mounts the avfs filesystem that allows to look inside compressed files via a virtual filesystem mount at root ~/.avfs without extracting them
mountAVFS :: IO ExitCode
mountAVFS = runShellCommand_ "mountavfs"

unmountAVFS :: IO ExitCode
unmountAVFS = runShellCommand_ "umountavfs"

-- runs an arbitrary command and returns the stdout
--runShellCommand :: String -> IO String
--runShellCommand cmd = do
--	tempDir <- getTemporaryDirectory
--	(fp,handle) <- openTempFile tempDir "ForestResultFile"
--	system $ cmd ++ " > ForestResultFile"
--	result <- hGetContents handle
--	hClose handle
--	removeFile fp
--	return result

sudoShellCommand :: String -> IO String
sudoShellCommand cmd = runShellCommand $ "sudo "++cmd

sudoShellCommand_ :: String -> IO ExitCode
sudoShellCommand_ cmd = runShellCommand_ $ "sudo "++cmd

runShellCommand_ :: String -> IO ExitCode
runShellCommand_ cmd = putStrLn ("Running command: "++show cmd) >> system cmd

runShellCommand :: String -> IO String
runShellCommand cmd = do
	putStrLn ("Running command: "++ cmd)
	let process = (shell cmd) { std_out = CreatePipe }
	(_,Just hout,_,ph) <- createProcess process
	result <- hGetContents hout
	waitForProcess ph
	return result

removePath :: FilePath -> IO ExitCode
removePath path = do
	test <- doesPathExistShell path
	if test
		then runShellCommand_ ("rm -rf " ++ path)
		else return ExitSuccess

movePath :: FilePath -> FilePath -> IO ExitCode
movePath from to = runShellCommand_ $ "mv " ++ from ++ " " ++ to

data ExistFlag = DirExists | FileExists | AnyExists deriving (Typeable,Show,Eq,Ord)

doesExistShellFlag :: ExistFlag -> FilePath -> IO Bool
doesExistShellFlag flag = case flag of
	DirExists -> doesDirectoryExistShell
	FileExists -> doesFileExistShell
	AnyExists -> doesPathExistShell

doesDirectoryExistShell :: FilePath -> IO Bool
doesDirectoryExistShell path = liftM (==ExitSuccess) $ runShellCommand_ ("test -d " ++ path)

doesFileExistShell :: FilePath -> IO Bool
doesFileExistShell path = liftM (==ExitSuccess) $ runShellCommand_ ("test -f " ++ path)

doesPathExistShell :: FilePath -> IO Bool
doesPathExistShell path = liftM (==ExitSuccess) $ runShellCommand_ ("test " ++ path)

getDirectoryContentsShell :: FilePath -> IO [FilePath]
getDirectoryContentsShell path = liftM lines $ runShellCommand ("ls -1 " ++ path)

--avfsPath :: FilePath -> IO FilePath
--avfsPath path = getHomeDirectory >>= \home -> return $ home </> ".avfs" </> path
 
--doShellCmd :: String -> IO String
--doShellCmd cmd = do 
--	(_, Just hout, _, ph) <- createProcess (shell cmd){ std_out = CreatePipe }
--	isEof <- hIsEOF hout
--	if isEof
--		then do
--			waitForProcess ph
--			return ""
--		else do
--			result <- hGetLine hout
--			hClose hout
--			waitForProcess ph
--			return result	

-- | Tests if the contents of two files differ
fileIsDifferent :: FilePath -> FilePath -> IO Bool
fileIsDifferent f1 f2 = do
	let cmd = "diff " ++ f1 ++ " " ++ f2
	result <- runShellCommand cmd
	return $ not $ Prelude.null result
	
allSymLinksUnder :: FilePath -> IO [(String,String)]
allSymLinksUnder path = do
	xs <- runShellCommand $ "find " ++ path ++ " -type l -exec ls -l {} \\; | awk '{print $9, $11}'"
	return $ map (\s -> let [src,tgt] = words s in (src,tgt)) $ lines xs
	
-- adds a cardinal to a file to look inside its content (for AVFS containers)
cardinalPath :: FilePath -> FilePath
cardinalPath path@(lastMay -> Just '#') = path
cardinalPath path = path ++ "#"

-- removes a trailing cardinal from an archive path (for AVFS containers)
uncardinalPath :: FilePath -> FilePath
uncardinalPath path@(lastMay -> Just '#') = init path
uncardinalPath path = path

-- generates a new unique filename by using the current procress id and a uuid
uniqueFileName :: IO FilePath
uniqueFileName = liftM toString $ waitNextUUID 5

waitNextUUID :: Int -> IO UUID
waitNextUUID i = do
	mb <- nextUUID
	case mb of
		Nothing -> threadDelay i >> nextUUIDSafe
		Just uuid -> return uuid
	



	