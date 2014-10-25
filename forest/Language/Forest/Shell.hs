{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Language.Forest.Shell where

import Data.Text (Text(..))
import qualified Data.Text as T
import Data.String
import Control.Monad
import System.Exit
import System.Process
import System.IO
import System.Directory
import Safe

--runShelly :: String -> [String] -> IO String
--runShelly (fromString -> cmd) (map fromString -> args) = liftM T.unpack $
--	shelly $ silently $ run cmd args
--
--runShelly__ :: String -> [String] -> IO Int
--runShelly__ (fromString -> cmd) (map fromString -> args) =
--	shelly $ silently $ run cmd args >> lastExitCode
--
--runShelly_ :: String -> [String] -> IO ()
--runShelly_ (fromString -> cmd) (map fromString -> args) =
--	shelly $ silently $ run_ cmd args
--
--pipe2Shelly :: String -> [String] -> String -> [String] -> IO String
--pipe2Shelly (fromString -> cmd1) (map fromString -> args1) (fromString -> cmd2) (map fromString -> args2) = liftM T.unpack $
--	shelly $ silently $ run cmd1 args1 -|- run cmd2 args2
--
--sudoShelly_ :: String -> [String] -> IO ()
--sudoShelly_ (fromString -> cmd) (map fromString -> args) = shelly $ silently $ run_ "sudo" (cmd:args)

-- gets the physical device on which a path is mounted (linux-specific)
pathDevice :: String -> IO String
pathDevice path = liftM (head . words) $ runShellCommand $ "/usr/sbin/grub-probe --target=device "++ show path

-- gets the path where a given device is mounted
devicePath :: String -> IO String
devicePath device = liftM (head . words) $ runShellCommand $ "mount | egrep "++show device++" | awk '{print $3}'"

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
	putStrLn ("Running command: "++show cmd)
	let process = (shell cmd) { std_out = CreatePipe }
	(_,Just hout,_,_) <- createProcess process
	result <- hGetContents hout
	return result

removePath :: FilePath -> IO ExitCode
removePath path = do
	test <- doesPathExistShell path
	if test
		then runShellCommand_ ("rm -rf " ++ path)
		else return ExitSuccess

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