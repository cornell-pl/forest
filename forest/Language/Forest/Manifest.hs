{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, StandaloneDeriving, NamedFieldPuns, RecordWildCards #-}

module Language.Forest.Manifest where

import System.Directory
import System.FilePath.Posix
import System.Cmd
import Language.Forest.IO.Shell
import System.IO

import Language.Forest.FS.FSRep as FSRep
import Language.Forest.Pure.MetaData
import Language.Pads.Padsc
import Language.Forest.Syntax

import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Monoid
import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Prelude hiding (const,read,writeFile)
import qualified Prelude
import Data.IORef
import Safe
import Language.Forest.IO.Utils
import Data.Foldable (foldlM)
import Control.Monad

-- ** Manifest data types

type Message = String

-- Note: should we be checking that permissions and owners are also consistent? For instance, store is not writing the fileinfos stored in memory

data Content = Local FilePath     -- contents are files in temp directory with given name (absolute paths on disk)
             | Link FilePath      -- contents are symbolic links, with given target (absolute paths on tree)
             | Dir				  
             | None               -- file path should not exist
   deriving (Show, Eq,Data,Typeable)

data Status = Invalid Message | Valid | NotValidated
   deriving (Show, Eq,Data,Typeable)

data ManifestEntry =  ManifestEntry
         { content       :: [Content]    -- list of content sources for canonical path (temporary files stored by Forest)
         , sources       :: [FilePath]   -- list of files that mapped to canonical path (for error reporting only) (paths relative to pathRoot)
         , status        :: Status       -- report on validity of manifest
         }  -- the two lists should have the same length
       deriving (Show,Eq,Data,Typeable)

type ManifestTable = Map FilePath ManifestEntry -- paths relative to pathRoot

--should we store the manifest in a top-level thunk?
--storing is an inherently strict operation, so there is no point in more precise laziness
-- unless we incrementally generate manifests, but for incremental storing the generated manifest should be proportional to the size of the modifications and that would suffice

data Manifest fs = MakeManifest
     {
       pathRoot    :: FilePath     -- root path of the manifest; if defined, all paths (even canonical ones) must be relative to the path root
     , manifestTree :: FSTree fs -- the FileStore version against which the manifest has been generated and to which it can be consistently applied; applying the manifest to any other filestore may yield inconsistent results
--     , tempDir :: FilePath        -- temp directory in which local files are written
     , entries :: ManifestTable   -- map from canonical paths (relative to pathRoot) to manifest entries
     , tests :: [ForestM fs Status] -- a *delayed* sequence of validity tests on the data (they should never return @NotValidated@)
     } deriving (Typeable)

deriving instance (Data (ForestM fs Status),Typeable fs,Data (FSTree fs)) => Data (Manifest fs)

-- showing a manifest does not evaluate the tests
instance Show (FSTree fs) => Show (Manifest fs) where
	show man = "(MakeManifest " ++ show (pathRoot man) ++ " " ++ show (manifestTree man) ++ " " ++ show (entries man) ++ " " ++ show (map (Prelude.const "<test>") $ tests man) ++ ")"

instance Monoid Status where
	mempty = Valid
	mappend Valid s2 = s2
	mappend s1 Valid = s1
	mappend (Invalid m1) (Invalid m2) = Invalid $ m1 ++ "; " ++ m2
	mappend NotValidated (Invalid m2) = Invalid m2
	mappend (Invalid m1) NotValidated = Invalid m1

instance Monoid ManifestEntry where
	mempty = ManifestEntry [] [] Valid
	mappend e1 e2 = ManifestEntry (content e1 ++ content e2) (sources e1 ++ sources e2) (status e1 `mappend` status e2)

-- ** manifest validation

-- validates a manifest and compiles a list of errors
manifestErrors :: FSRep fs => Manifest fs -> ForestM fs [Message]
manifestErrors man = do
	let all_tests = tests man ++ map (liftM (status . snd) . detectConflictInEntry (pathRoot man)) (Map.toList (entries man))
	let doTest res test = do
		status <- test
		case status of
			Invalid msg -> return $ msg:res
			otherwise -> return res
	foldlM doTest [] all_tests

-- | Validates a manifest against the current FSTree and returns a validated manifest
-- it expects that the current FSTree is the result of applying the manifest changes to the old tree
-- conflicts only arise from trying to write different content to the same file?
validateManifest :: FSRep fs => Manifest fs -> ForestM fs (Manifest fs)
validateManifest man = do
	entries' <- mapM (detectConflictInEntry (pathRoot man)) $ Map.toList (entries man)
	tests' <- mapM (\m -> m >>= return . return) (tests man) -- evaluates each test
	return $ man { entries = Map.fromList entries', tests = tests' }

detectConflictInEntry :: FSRep fs => FilePath -> (FilePath,ManifestEntry) -> ForestM fs (FilePath,ManifestEntry)
detectConflictInEntry pathRoot (dskpath,entry) = do
	status' <- detectContentConflict pathRoot (zip (content entry) (sources entry))
	return (dskpath,entry { status = status' })

detectContentConflict :: FSRep fs => FilePath -> [(Content,FilePath)] -> ForestM fs Status
detectContentConflict pathRoot content_fp = case content_fp of
	[] ->  return Valid
	[x] -> return Valid
	(x:y:rest) -> do
		cxy    <- detectPairConflict pathRoot x y
		cyrest <- detectContentConflict pathRoot (y:rest)
		return $ cxy `mappend` cyrest
		
detectPairConflict :: FSRep fs => FilePath -> (Content,FilePath) -> (Content,FilePath) -> ForestM fs Status
detectPairConflict pathRoot (c1,s1) (c2,s2) = case (c1,c2) of
	(Local fp1,Local fp2) -> do
		isDifferent <- forestIO $ fileIsDifferent fp1 fp2
		if isDifferent
			then return $ Invalid $ "File " ++ pathRoot </> s1 ++ " is in conflict with file " ++ pathRoot </> s2
			else return Valid
	(None,None) -> return Valid
	(Link target1,Link target2) -> if target1 == target2
		then return Valid
		else return $ Invalid $ "Link " ++ target1 ++ " clashed with target " ++ target2
	(Dir,Dir) -> return Valid   --- update this to manage conflicts with contained comprehensions
	(t1,t2) -> return $ Invalid $ show t1 ++ " is in conflict with "  ++ show t2

-- ** manifest storing

-- storing a manifest only commits the changes under the pathRoot, and returns a manifest with the remaining changes

storeManifest :: FSRep fs => Manifest fs -> ForestM fs ()
storeManifest man = storeManifest' man >> return ()

storeManifest' :: FSRep fs => Manifest fs -> ForestM fs (Manifest fs)
storeManifest' man = storeManifestAt (pathRoot man) man

-- this is kind of unsafe in general, since the validation of the spec may depend on the pathRoot
-- writes the manifest by blindly replacing its original root directory with a new root
-- returns a manifest of writes that would occur outside of the original root directory
-- this is mostly a sanity check, to avoid for instance user-provided absolute paths in a manifest
storeManifestAt :: FSRep fs => FilePath -> Manifest fs -> ForestM fs (Manifest fs)
storeManifestAt new_root man = do
	let ori_root = pathRoot man
	let tbl = Map.toList $ entries man
	let (rel_tbl,abs_tbl) = List.partition (\(n,_) -> isRelative n) tbl -- make sure that we only process relative paths
	mapM_ (storeManifestEntryAt new_root ori_root) rel_tbl
	return $ man { entries = Map.fromList abs_tbl }

storeManifestEntryAt :: FSRep fs => FilePath -> FilePath -> (FilePath,ManifestEntry) -> ForestM fs ()
storeManifestEntryAt new_root ori_root (rel,e) = do 
	let newpath = new_root </> rel
	case headMay (content e) of
		Just (Local tmpFile) -> FSRep.writeFile newpath tmpFile
		Just (Link linkDest) -> writeLink newpath linkDest
		Just Dir -> writeDir newpath
		Just None -> deletePath newpath
		Nothing -> return ()

-- ** manifest creation

-- | creates a fresh manifest with the latest tree
newManifest :: FSRep fs => ForestM fs (Manifest fs)
newManifest = latestTree >>= newManifestWith "/"

newManifestWith :: FSRep fs => FilePath -> FSTree fs -> ForestM fs (Manifest fs)
newManifestWith pathRoot tree = return $ MakeManifest pathRoot tree Map.empty []

-- ** manifest manipulation functions

-- right-biased merge that joins two manifests by finding the common ancestor pathRoot
mergeManifests :: Manifest fs -> Manifest fs -> Manifest fs
mergeManifests man1 man2 = MakeManifest newRoot (manifestTree man2) (newTable1 `mappend` newTable2) (tests man1 `mappend` tests man2)
	where newRoot = commonParentPath (pathRoot man1) (pathRoot man2)
	      newTable1 = changeRootOfManifestTable (pathRoot man1) newRoot (entries man1)
	      newTable2 = changeRootOfManifestTable (pathRoot man2) newRoot (entries man2)

changeRootOfManifestTable :: FilePath -> FilePath -> ManifestTable -> ManifestTable
changeRootOfManifestTable old_root new_root tbl = Map.fromList $ map (\(n,e) -> (makeRelative (old_root </> n) new_root,changeRootOfManifestEntry old_root new_root e)) $ Map.toList tbl

changeRootOfManifestEntry :: FilePath -> FilePath -> ManifestEntry -> ManifestEntry
changeRootOfManifestEntry old_root new_root e = e { sources = map (\n -> makeRelative (old_root </> n) new_root) (sources e) }

boolStatus :: String -> Bool -> Status
boolStatus msg b = if b then Valid else (Invalid msg)

addFileToManifest :: FSRep fs => (FilePath -> content -> IO ()) -> FilePath -> FilePath -> content -> Manifest fs -> ForestM fs (Manifest fs)
addFileToManifest printContent canpath path content man = do
		tmpFile <- tempPath
		forestIO $ printContent tmpFile content -- writes the content to the temporary file
		return $ addFileToManifest' canpath path tmpFile man

addFileToManifest' :: FSRep fs => FilePath -> FilePath -> FilePath -> Manifest fs -> (Manifest fs)
addFileToManifest' canpath path tmpFile man = {-debug ("addFileToManifest: "++show canpath ++" "++show path) $ -} if (isAbsolute canpath && isValid canpath)
	then
		let updEntry = ManifestEntry [Local tmpFile] [makeRelative (pathRoot man) path] NotValidated
		    newEntry = ManifestEntry [Local tmpFile] [makeRelative (pathRoot man) path] Valid -- a single entry is always valid
		in  man { entries = Map.insertWith (\y x -> mappend x y) (makeRelative (pathRoot man) canpath) newEntry (entries man) }
	else addTestToManifest (liftM (boolStatus "invalid path") $ return False) man
	
addLinkToManifest :: FSRep fs => FilePath -> FilePath -> FilePath -> Manifest fs -> (Manifest fs)
addLinkToManifest canpath path linkPath man = {-debug ("addLinkToManifest: "++show canpath ++" "++show path) $ -} if (isAbsolute canpath && isValid canpath)
	then
		let updEntry = ManifestEntry [Link linkPath] [makeRelative (pathRoot man) path] NotValidated
		    newEntry = ManifestEntry [Link linkPath] [makeRelative (pathRoot man) path] Valid -- a single entry is always valid
		in  man { entries = Map.insertWith (\y x -> mappend x y) (makeRelative (pathRoot man) canpath) newEntry (entries man) }
	else addTestToManifest (liftM (boolStatus "invalid path") $ return False) man

removePathFromManifest :: FSRep fs => FilePath -> FilePath -> Manifest fs -> Manifest fs
removePathFromManifest canpath path man = {-debug ("removePathFromManifest: "++show canpath ++" "++show path) $ -} if (isAbsolute canpath && isValid canpath)
	then
		let updEntry = ManifestEntry [None] [makeRelative (pathRoot man) path] NotValidated
		    newEntry = ManifestEntry [None] [makeRelative (pathRoot man) path] Valid -- a single entry is always valid
		in man { entries = Map.insertWith (\y x -> mappend x y) (makeRelative (pathRoot man) canpath) newEntry (entries man) }
	else addTestToManifest (liftM (boolStatus "invalid path") $ return False) man

addTestToManifest :: FSRep fs => ForestM fs Status -> Manifest fs -> Manifest fs
addTestToManifest testm man = man { tests = testm : tests man }

addDirToManifest :: FSRep fs => FilePath -> FilePath -> Manifest fs -> Manifest fs
addDirToManifest canpath path man = {-debug ("addDirToManifest: "++show canpath ++" "++show path) $ -} if (isAbsolute canpath && isValid canpath)
	then
		let updEntry = ManifestEntry [Dir] [makeRelative (pathRoot man) path] NotValidated
		    newEntry = ManifestEntry [Dir] [makeRelative (pathRoot man) path] Valid
		in man { entries = Map.insertWith (\y x -> mappend x y) (makeRelative (pathRoot man) canpath) newEntry (entries man) }
	else addTestToManifest (liftM (boolStatus "invalid path") $ return False) man

collectManifestErrors :: Manifest fs -> Status
collectManifestErrors manifest = mconcat (collectErrorsManifestTable (entries manifest))

collectErrorsManifestTable :: ManifestTable -> [Status]
collectErrorsManifestTable entries = List.map (\(fp, entry) -> status entry) (Map.toList entries)


	

