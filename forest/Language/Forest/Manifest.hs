{-# LANGUAGE ConstraintKinds, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, StandaloneDeriving, NamedFieldPuns, RecordWildCards #-}

module Language.Forest.Manifest where

import System.Directory
import System.FilePath.Posix
import System.Cmd
import System.IO

import Language.Forest.FS.FSRep
import Language.Forest.MetaData
import Language.Pads.Padsc
import Language.Forest.Syntax

import Data.Map (Map(..))
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set(..))
import qualified Data.Set as Set
import Language.Forest.FS.Diff
import Data.Monoid
import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Prelude hiding (const,read)
import qualified Prelude
import Data.IORef

-- ** Manifest data types

type Message = String

-- Note: we should be checking that permissions and owners are also consistent?

data Content = Local FilePath     -- contents are in file in temp directory with given name
             | Link FilePath      -- contents are symbolic link, with given target
             | Dir
             | ListComp [FilePath]
             | None               -- file path should not exist
             | LocalArchive [ArchiveType] FilePath      -- like local, except for archives; takes a list of archive types, like tar.gz
   deriving (Show, Eq,Data,Typeable)

instance (Layer l inc r m) => Display l inc r m Content where
	displaysPrec x rest = return $ showsPrec 0 x rest

data Status = Invalid Message | Valid | NotValidated
   deriving (Show, Eq,Data,Typeable)

instance (Layer l inc r m) => Display l inc r m Status where
	displaysPrec x rest = return $ showsPrec 0 x rest

data ManifestEntry =  ManifestEntry
         { content       :: [Content]    -- list of content sources for canonical path (temporary files stored by Forest)
         , sources       :: [FilePath]   -- list of files that mapped to canonical path (for error reporting only)
         , status        :: Status       -- report on validity of manifest
         }  -- the two lists should have the same length
       deriving (Show,Eq,Data,Typeable)

instance (Layer l inc r m) => Display l inc r m ManifestEntry where
	displaysPrec x rest = return $ showsPrec 0 x rest

type ManifestTable = Map FilePath ManifestEntry

--should we store the manifest in a top-level thunk?
--storing is an inherently strict operation, so there is no point in more precise laziness
-- unless we incrementally generate manifests, but for incremental storing the generated manifest should be proportional to the size of the modifications and that would suffice

data Manifest fs = MakeManifest
     {
--      pathToRoot    :: Maybe FilePath     -- path to parent directory of FileStore
      manifestTree :: FSTree fs -- the FileStore version against which the manifest has been generated and to which it can be consistently applied; applying the manifest to any other filestore may yield inconsistent results
--     , tempDir :: FilePath        -- temp directory in which local files are written
     , entries :: ManifestTable   -- map from canonical paths to manifest entries
     , tests :: [ForestO fs Status] -- a *delayed* sequence of validity tests on the data (they should never return @NotValidated@)
     } deriving (Typeable)

deriving instance (Data (ForestO fs Status),Typeable fs,Data (FSTree fs)) => Data (Manifest fs)

-- showing a manifest does not evaluate the tests
instance Show (FSTree fs) => Show (Manifest fs) where
	show man = "(MakeManifest " ++ show (manifestTree man) ++ " " ++ show (entries man) ++ " " ++ show (map (Prelude.const "<test>") $ tests man) ++ ")"

-- displaying a manifest will evaluate the tests
instance (ForestLayer fs Outside,Typeable fs,Show (FSTree fs)) => Display Outside (IncForest fs) IORef IO (Manifest fs) where
	displaysPrec man rest = do
		stests <- displaysPrec (tests man) (')':rest)
		let sentries = showsPrec 0 (entries man) (' ':stests)
		return $ "(MakeManifest " ++ show (manifestTree man) ++ " " ++ sentries

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

-- | creates a fresh manifest with the latest tree
newManifest :: FSRep fs => ForestO fs (Manifest fs)
newManifest = latestTree >>= newManifestForTree

newManifestForTree :: FSRep fs => FSTree fs -> ForestO fs (Manifest fs)
newManifestForTree tree = return $ MakeManifest tree Map.empty []

-- ** manifest validation

-- | Validates a manifest against the filestore tree to which it is supposed to be applied and returns a validated manifest
-- conflicts only arise from trying to write different content to the same file?
validateManifest :: FSRep fs => Manifest fs -> ForestO fs (Manifest fs)
validateManifest man = do
	entries' <- mapM detectConflictInEntry $ Map.toList (entries man)
	tests' <- mapM (\m -> m >>= return . return) (tests man) -- evaluates each test
	return $ man { entries = Map.fromList entries', tests = tests' }

detectConflictInEntry :: FSRep fs => (FilePath,ManifestEntry) -> ForestO fs (FilePath,ManifestEntry)
detectConflictInEntry (dskpath,entry) = do
	status' <- detectContentConflict (zip (content entry) (sources entry))
	return (dskpath,entry { status = status' })

detectContentConflict :: FSRep fs => [(Content,FilePath)] -> ForestO fs Status
detectContentConflict content_fp = case content_fp of
	[] ->  return Valid
	[x] -> return Valid
	(x:y:rest) -> do
		cxy    <- detectPairConflict x y
		cyrest <- detectContentConflict (y:rest)
		return $ cxy `mappend` cyrest
		
detectPairConflict :: FSRep fs => (Content,FilePath) -> (Content,FilePath) -> ForestO fs Status
detectPairConflict (c1,s1) (c2,s2) = case (c1,c2) of
	(Local fp1,Local fp2) -> do
		isDifferent <- forestIO $ fileIsDifferent fp1 fp2
		if isDifferent
			then return $ Invalid $ "File " ++ s1 ++ " is in conflict with file " ++ s2
			else return Valid
	(LocalArchive exts1 fp1,LocalArchive exts2 fp2) -> do
		isDifferent <- forestIO $ fileIsDifferent fp1 fp2
		if isDifferent
			then return $ Invalid $ "Archived file " ++ addArchiveTypes s1 exts1 ++ " is in conflict with archived file " ++ addArchiveTypes s2 exts2
			else return Valid
	(None,None) -> return Valid
	(Link target1,Link target2) -> if target1 == target2
		then return Valid
		else return $ Invalid $ "Link " ++ target1 ++ " clashed with target " ++ target2
	(Dir,Dir) -> return Valid   --- update this to manage conflicts with contained comprehensions
	(ListComp f1,ListComp f2) -> return NotValidated  -- We will check this in the next stage...
	(ListComp _,Dir) -> return Valid
	(Dir,ListComp _) -> return Valid
	(t1,t2) -> return $ Invalid $ show t1 ++ " is in conflict with "  ++ show t2

-- ** manifest storing

storeManifest :: FSRep fs => Manifest fs -> ForestO fs ()
storeManifest = mapM_ storeManifestEntry . Map.toList . entries
	
storeManifestEntry :: FSRep fs => (FilePath,ManifestEntry) -> ForestO fs ()
storeManifestEntry = undefined
--storeManifestEntryAt destDir clipPath tempDir (canonPath, (ManifestEntry {content, ..}))  = do
--  { let targetPath = getTargetPath' destDir clipPath canonPath
--  ; createDirectoryIfMissing True (takeDirectory targetPath)
--  ; case List.head content of 
--      Local localName -> do 
--        { let srcPath = combine tempDir localName
--        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
--        ; return ()
--        }
--      LocalTar localName -> do 
--        { let srcPath = combine tempDir localName
--        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
--        ; return ()
--        }
--      LocalGzip localName -> do 
--        { let srcPath = combine tempDir localName
--        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
--        ; return ()
--        }
--      Link linkDest -> do
--        { doShellCmd ("ln -s " ++ linkDest ++ " " ++ targetPath)
--        ; return ()
--        }
--      Dir -> return ()  -- Nothing needs to be done in this case because dir is already created
--      ListComp files -> return () -- Nothing needs to be done in this case because files are listed separately
--      None -> do
--        { doShellCmd ("rm -rf "  ++ targetPath)
--        ; return ()
--        } 
--
--  }

-- ** manifest manipulation functions

boolStatus :: String -> Bool -> Status
boolStatus msg b = if b then Valid else (Invalid msg)

addFileToManifest :: FSRep fs => (FilePath -> content -> IO ()) -> FilePath -> FilePath -> content -> Manifest fs -> ForestO fs (Manifest fs)
addFileToManifest printContent canpath path content man = do
	tmpFile <- tempPath
	forestIO $ printContent tmpFile content -- writes the content to the temporary file
	let updEntry = ManifestEntry [Local tmpFile] [path] NotValidated
	let newEntry = ManifestEntry [Local tmpFile] [path] Valid -- a single entry is always valid
	return $ man { entries = Map.insertWith (\y x -> mappend x y) canpath newEntry (entries man) }

removeFileFromManifest :: FSRep fs => FilePath -> FilePath -> Manifest fs -> Manifest fs
removeFileFromManifest canpath path man =
	let updEntry = ManifestEntry [None] [path] NotValidated
	    newEntry = ManifestEntry [None] [path] Valid -- a single entry is always valid
	in man { entries = Map.insertWith (\y x -> mappend x y) canpath newEntry (entries man) }

addTestToManifest :: FSRep fs => ForestO fs Status -> Manifest fs -> Manifest fs
addTestToManifest testm man = man { tests = testm : tests man }

addDirToManifest :: FSRep fs => FilePath -> FilePath -> Manifest fs -> Manifest fs
addDirToManifest canpath path man =
	let updEntry = ManifestEntry [Dir] [path] NotValidated
	    newEntry = ManifestEntry [Dir] [path] Valid
	in man { entries = Map.insertWith (\y x -> mappend x y) canpath newEntry (entries man) }





--
--collectManifestErrors :: Manifest -> Status
--collectManifestErrors manifest = joinStatus (collectErrorsManifestTable (entries manifest))
--
--collectErrorsManifestTable :: ManifestTable -> [Status]
--collectErrorsManifestTable entries = List.map (\(fp, entry) -> status entry) (Map.toList entries)
--
--detectConflictsInListComps :: Manifest -> Manifest -> Manifest
--detectConflictsInListComps (Manifest{count,pathToRoot=orig_pathToRoot,tempDir,entries=orig_entries}) test_manifest = 
--  let orig_rootDir = dropFileName (Maybe.fromJust orig_pathToRoot)
--      test_rootDir = dropFileName (Maybe.fromJust (pathToRoot test_manifest))
--      orig_entries' = listCompConflictsInManifestEntities test_rootDir (entries test_manifest) orig_rootDir orig_entries
--  in Manifest{count,pathToRoot=orig_pathToRoot,tempDir,entries=orig_entries'}
--
--listCompConflictsInManifestEntities test_rootDir test_entries orig_rootDir orig_entries = 
--  Map.mapWithKey (updateOne test_rootDir test_entries orig_rootDir) orig_entries
--
--updateOne test_rootDir test_entries orig_rootDir orig_pathToEntry orig_entry = 
--  let orig_RelPath = Maybe.fromJust (List.stripPrefix orig_rootDir orig_pathToEntry)
--      test_pathToEntry = combine test_rootDir orig_RelPath
--      test_entry = test_entries Map.! test_pathToEntry
--  in listCompConflictsInManifestEntry orig_entry test_entry
--
--listCompConflictsInManifestEntry (ManifestEntry{content=orig_content,sources=orig_sources,status=orig_status})
--                                 (ManifestEntry{content=test_content,sources=test_sources,status=test_status}) = 
--  let orig_complists = List.filter isListComp orig_content
--      test_complists = List.filter isListComp test_content
--      new_status = if orig_complists == test_complists then 
--                      promoteNotValidated orig_status
--                   else Invalid ("Conflict detected in list comprehensions: original comprehension had "  
--                                 ++ (show orig_complists) 
--                                 ++ "while after printing comprehension had "
--                                 ++ (show test_complists))
--  in ManifestEntry{content=orig_content,sources=orig_sources,status=new_status}
--
--
--diffManifest :: Manifest -> Manifest -> [(FilePath, [FilePath])]
--diffManifest (Manifest{count,pathToRoot=source_pathToRoot,tempDir,entries=source_entries}) target_manifest = []
--{-
--  let source_rootDir  = dropFileName (Maybe.fromJust source_pathToRoot)
--      target_rootDir  = dropFileName (Maybe.fromJust (pathToRoot target_manifest))
--      source_entriesL = Map.toList source_entries
--      target_entriesL = Map.toList (entries target_manifest)
--      source_complists = unwrapComp (List.filter isListComp' source_entriesL)
--      target_complists = unwrapComp (List.filter isListComp' target_entriesL)
--      source_complists' = List.groupBy (\(f1,c1) (f2,c2)  -> f1 == f2) source_complists
--      target_complists' = List.groupBy (\(f1,c1) (f2,c2)  -> f1 == f2) target_complists
--      source_complists'' = let (fps, contents) = List.unzip source_complists' in (List.head fps, contents)
--      target_complists'' = let (fps, contents) = List.unzip target_complists' in (List.head fps, contents)
--  -- r
--  in -- Map.toList (diffManifestEntities target_rootDir (entries target_manifest) source_rootDir source_entries)
--     []
---}
--
--diffManifestEntities target_rootDir target_entries source_rootDir source_entries = 
--  Map.mapWithKey (diffOne target_rootDir target_entries source_rootDir) source_entries
--
--diffOne target_rootDir target_entries source_rootDir source_pathToEntry source_entry = 
--  let source_RelPath = Maybe.fromJust (List.stripPrefix source_rootDir source_pathToEntry)
--      target_pathToEntry = combine target_rootDir source_RelPath
--  in case Map.lookup target_pathToEntry target_entries of
--       Nothing -> []
--       Just target_entry -> diffManifestEntry  source_entry target_entry
--
--diffManifestEntry (ManifestEntry{content=source_content,sources=source_sources,status=source_status})
--                  (ManifestEntry{content=target_content,sources=target_sources,status=target_status}) = 
--  let source_complists = List.concatMap unwrapComp (List.filter isListComp source_content)
--      target_complists = List.concatMap unwrapComp (List.filter isListComp target_content)
--  -- return everything in targets that is not in source
--      sourceSet = Set.fromList source_complists
--      targetSet = Set.fromList target_complists
--      diff = Set.difference targetSet sourceSet
--  in Set.toList diff
-- 
--
--unwrapComp (ListComp x) = x
--unwrapComp' (fp, ListComp x) = (fp,x)
--
--isListComp :: Content -> Bool
--isListComp c = case c of
--  ListComp _ -> True
--  otherwise  -> False
--
--isListComp' :: (FilePath, Content) -> Bool
--isListComp' (fp,c) = case c of
--  ListComp _ -> True
--  otherwise  -> False
--
--
--
--

--
--promoteNotValidated :: Status -> Status
--promoteNotValidated status = case status of 
--  NotValidated -> Valid
--  x -> x
--
--
--  
--getTargetPath' destDir clipPath fullPath = 
--  case List.stripPrefix clipPath fullPath of
--    Nothing -> combine destDir fullPath
--    Just suffix -> combine destDir suffix
--
--getClipPathFromTarManifest :: Manifest -> FilePath -> FilePath
--getClipPathFromTarManifest manifest scratchDir = 
--  let entries' = entries manifest
--      paths = Map.keys entries'
--      paths' = List.filter (List.isPrefixOf scratchDir) paths
--    in (List.head paths') ++ "/"
--      
--getTempForestOanifestDirectory :: IO FilePath
--getTempForestOanifestDirectory = do 
--  { tempDir <- getTempForestDirectory 
--  ; let forestManifestDir = combine tempDir "Manifests"
--  ; createDirectoryIfMissing False forestManifestDir
----  ; (suffix :: Int) <- randomIO
----  ; let fp = "Manifest_"  ++ (show suffix)
--  ; (fp, handle) <- openTempFile forestManifestDir "Manifest"
--  ; hClose handle
--  ; system ("rm -f " ++ fp)
--  ; print ("temp directory name is: " ++ fp)
--  ; createDirectoryIfMissing False fp
--  ; return fp
--  }
--
--
--getTempForestListDirectory :: IO FilePath
--getTempForestListDirectory = do 
--  { tempDir <- getTempForestDirectory 
--  ; let forestManifestDir = combine tempDir "Lists"
--  ; createDirectoryIfMissing False forestManifestDir
--  ; (fp, handle) <- openTempFile forestManifestDir "List"
--  ; hClose handle
--  ; system ("rm -f " ++ fp)
--  ; print ("temp list directory name is: " ++ fp)
--  ; createDirectoryIfMissing False fp
--  ; return fp
--  }
--
--
--
--
--gzipManifestEntry :: ForestMD fmd => fmd -> Manifest -> IO Manifest
--gzipManifestEntry fmd (Manifest {count, pathToRoot, tempDir, entries}) = do
--  { fp <- canonicalizePath (get_fullpath fmd)
--  ; let fp_noZip = dropExtension fp
--  ; print ("gzipping filepath: " ++ fp_noZip)
--  ; let (ManifestEntry {content = (c1:cs), sources, status=orig_status}) = entries Map.! fp_noZip
--  ; entry' <-  case c1 of 
--                Local localName -> do 
--                   { let srcPath = combine tempDir localName
--                   ; doShellCmd ("gzip " ++ srcPath )
--                   ; return (ManifestEntry {content = LocalGzip (localName ++ ".gz"):cs, sources, status = combineStatus NotValidated orig_status})
--                   }
--                LocalTar tarName -> do 
--                   { let srcPath = combine tempDir tarName
--                   ; doShellCmd ("gzip " ++ srcPath )
--                   ; return (ManifestEntry {content = LocalGzip (tarName ++ ".gz"):cs, sources, status = combineStatus NotValidated orig_status})
--                   }
--                otherwise -> do
--                  { let newStatus = Invalid ("Cannot zip a " ++ (show c1))
--                  ; return (ManifestEntry {content = c1:cs, sources, status = combineStatus orig_status newStatus})
--                  }
--  ; let entries'  = Map.delete fp_noZip entries
--  ; let entries'' = Map.insert fp entry' entries'
--  ; return (Manifest{count, pathToRoot, tempDir, entries=entries''})
--  }
--
--getNewLocalName :: FilePath -> Manifest -> (FilePath, FilePath, Manifest)
--getNewLocalName baseName (Manifest{count=oldCount,pathToRoot, tempDir,entries}) = 
-- let localName =  baseName ++ (show oldCount)
-- in ( localName
--    , combine tempDir localName
--    , Manifest {count = oldCount + 1, pathToRoot,tempDir, entries}
--    )
--
--
--stripSharedPrefix :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
--stripSharedPrefix [] [] shared = (List.reverse shared, [], [])
--stripSharedPrefix [] b  shared = (List.reverse shared, [], b)
--stripSharedPrefix a  [] shared = (shared, a,  [])
--stripSharedPrefix a @ (x:xs)  b @ (y:ys) shared = 
--   if x == y then stripSharedPrefix  xs ys (x:shared)
--             else (List.reverse shared, a, b)
--
--getTargetPath :: FilePath -> FilePath -> FilePath
--getTargetPath targetDir fullPath = 
--  let (shared, target, residual) = stripSharedPrefix targetDir fullPath []
--  in  combine targetDir residual
--
--
--
---- Add a file
--updateManifestPads :: (Pads rep md, ForestMD fmd) => (rep, (fmd,md)) -> Manifest -> IO Manifest
--updateManifestPads (rep, (fmd, md)) manifest = updateManifestWith printFile (rep,md) fmd manifest
--
--updateManifestPads1 :: (Pads1 arg rep md, ForestMD fmd) => arg -> (rep, (fmd,md)) ->  Manifest -> IO Manifest
--updateManifestPads1 arg (rep, (fmd,md)) manifest = updateManifestWith (printFile1 arg) (rep,md) fmd manifest
--
--
--updateManifestWith :: ForestMD fmd => (FilePath -> payload -> IO ()) -> payload -> fmd -> Manifest -> IO Manifest
--updateManifestWith printF payload fmd manifest0 = do
--	let fullPath = get_fullpath fmd
--	let isLink   = get_symLink fmd
--	canonPath <- canonicalizePath fullPath -- path in the value metadata
--	mostlyCanonPath <- mostlyCanonicalizeLink fullPath --path with everything canonicalized except the filename?
--	let baseName = takeBaseName canonPath
--	let (localName, targetPath, manifest1) = getNewLocalName baseName manifest0
--	printF targetPath payload 
--	return $ updateManifestRaw canonPath mostlyCanonPath localName isLink manifest1
	
--
--updateManifestRaw :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> Manifest -> Manifest
--updateManifestRaw  canonPath origPath localName optLinkTarget (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
--   let entries_withFile = case Map.lookup canonPath orig_entries of 
--                            Nothing -> addFile canonPath origPath localName orig_entries
--                            Just manifestEntry -> updateFile manifestEntry canonPath origPath localName manifest orig_entries
--       new_entries = case optLinkTarget of 
--                            Just target -> 
--                              case Map.lookup origPath entries_withFile of
--                                 Nothing ->  addLink origPath canonPath orig_entries   -- record link from source of sym link to canon Path
--                                 Just manifestEntry -> updateWithLink manifestEntry origPath canonPath manifest orig_entries
--                            Nothing ->   entries_withFile
--   in  Manifest{count, pathToRoot, tempDir, entries=new_entries}
--
--
--addFile :: FilePath -> FilePath -> FilePath -> ManifestTable -> ManifestTable
--addFile canonPath origPath local mp = 
--     Map.insert canonPath (ManifestEntry{content = [Local local], sources= [origPath], status = Valid}) mp
--
--
--updateFile :: ManifestEntry -> FilePath -> FilePath -> FilePath -> Manifest -> ManifestTable -> ManifestTable
--updateFile (ManifestEntry {content=lN0, sources=src1, status=status1}) canonPath origPath localName manifest table = 
--    let localDir = tempDir manifest
--        newEntry = ManifestEntry{ content = (Local localName) : lN0
--                                 , sources = origPath : src1
--                                 , status = combineStatus status1 NotValidated
--                                 }
--   in Map.insert canonPath newEntry table      
--
--
--updateManifestWithTar :: ForestMD fmd => FilePath -> Status -> fmd -> Manifest -> IO Manifest
--updateManifestWithTar tarredFile status fmd manifest = do
--   { let fullPath = get_fullpath fmd
--   ; canonPath <- canonicalizePath fullPath
--   ; let manifest' = updateManifestWithTarRaw tarredFile status canonPath manifest
--   ; return manifest'
--   }
--
--updateManifestWithTarRaw :: FilePath -> Status -> FilePath ->  Manifest ->  Manifest
--updateManifestWithTarRaw  tarredFile status tarPath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
--  let  new_entries =  case Map.lookup tarPath orig_entries of
--                          Nothing -> addTar tarredFile status tarPath orig_entries   -- record link from source of sym link to canon Path
--                          Just manifestEntry -> updateWithTar manifestEntry tarredFile status tarPath manifest orig_entries
--  in (Manifest{count, pathToRoot,tempDir, entries=new_entries})
--
--addTar :: FilePath -> Status -> FilePath -> ManifestTable -> ManifestTable
--addTar tarredFile tarStatus tarPath mp = 
--     Map.insert tarPath  (ManifestEntry{content= [LocalTar tarredFile], sources = [tarPath], status = tarStatus}) mp
--
--updateWithTar :: ManifestEntry -> FilePath -> Status -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
--updateWithTar (ManifestEntry {content=lN0, sources=src1, status=status1}) tarredPath tarStatus tarPath manifest table = 
--   let newEntry = ManifestEntry{ content = (LocalTar tarredPath) : lN0
--                               , sources = tarPath : src1
--                               , status = combineStatus tarStatus status1
--                               }
--   in Map.insert tarPath newEntry table      
--
--
--updateManifestWithNone :: ForestMD fmd => fmd -> Manifest -> IO Manifest
--updateManifestWithNone fmd manifest = do
--   { let fullPath = get_fullpath fmd
--   ; canonPath <- canonicalizePath fullPath
--   ; let manifest' = updateManifestWithNoneRaw canonPath manifest
--   ; return manifest'
--   }
--
--updateManifestWithNoneRaw :: FilePath ->  Manifest ->  Manifest
--updateManifestWithNoneRaw  nonePath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
--  let  new_entries =  case Map.lookup nonePath orig_entries of
--                          Nothing -> addNone nonePath orig_entries   -- record link from source of sym link to canon Path
--                          Just manifestEntry -> updateWithNone manifestEntry nonePath manifest orig_entries
--  in (Manifest{count, pathToRoot, tempDir, entries=new_entries})
--
--addNone :: FilePath -> ManifestTable -> ManifestTable
--addNone nonePath mp = 
--     Map.insert nonePath  (ManifestEntry{content= [None], sources = [nonePath], status = Valid}) mp
--
--updateWithNone :: ManifestEntry -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
--updateWithNone (ManifestEntry {content=lN0, sources=src1, status=status1}) nonePath manifest table = 
--   let newEntry = ManifestEntry{ content = None : lN0
--                               , sources = nonePath : src1
--                               , status = combineStatus NotValidated status1
--                               }
--   in Map.insert nonePath newEntry table      
--
--updateManifestWithDir :: ForestMD fmd => fmd -> Manifest -> IO Manifest
--updateManifestWithDir fmd manifest = do
--   { let fullPath = get_fullpath fmd
--   ; canonPath <- canonicalizePath fullPath
--   ; let manifest' = updateManifestWithDirRaw canonPath manifest
--   ; return manifest'
--   }
--
--updateManifestWithDirRaw :: FilePath ->  Manifest ->  Manifest
--updateManifestWithDirRaw  dirPath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
--  let  new_entries =  case Map.lookup dirPath orig_entries of
--                          Nothing -> addDir dirPath orig_entries   -- record link from source of sym link to canon Path
--                          Just manifestEntry -> updateWithDir manifestEntry dirPath manifest orig_entries
--  in (Manifest{count, pathToRoot,tempDir, entries=new_entries})
--
--addDir :: FilePath -> ManifestTable -> ManifestTable
--addDir dirPath mp = 
--     Map.insert dirPath  (ManifestEntry{content= [Dir], sources = [dirPath], status = Valid}) mp
--
--updateWithDir :: ManifestEntry -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
--updateWithDir (ManifestEntry {content=lN0, sources=src1, status=status1}) dirPath manifest table = 
--   let newEntry = ManifestEntry{ content = Dir : lN0
--                               , sources = dirPath : src1
--                               , status = combineStatus NotValidated status1
--                               }
--   in Map.insert dirPath newEntry table      
--
--updateManifestWithLink :: ForestMD fmd => fmd -> Manifest -> IO Manifest
--updateManifestWithLink fmd manifest = case get_symLink fmd of 
--  Nothing ->  return manifest
--  Just linkTarget -> do 
--    { let fullPath = get_fullpath fmd
--    ; print ("entering symlink: source = " ++ fullPath ++ " Target = " ++ linkTarget)
--    ; updateManifestWithLinkRaw fullPath linkTarget manifest   
--    }
--
--updateManifestWithLinkRaw :: FilePath -> FilePath ->  Manifest -> IO Manifest
--updateManifestWithLinkRaw  source target (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = do
--  { new_entries <-  case Map.lookup source orig_entries of
--                          Nothing -> return (addLink source target orig_entries)   -- record link from source of sym link to canon Path
--                          Just manifestEntry -> return (updateWithLink manifestEntry source target manifest orig_entries)
--  ; return (Manifest{count, pathToRoot,tempDir, entries=new_entries})
--  }
--
--
--mostlyCanonicalizeLink :: FilePath -> IO FilePath
--mostlyCanonicalizeLink fp = do
-- { let root = dropFileName fp
-- ; let fileName = takeFileName fp
-- ; root_canon <- canonicalizePath root
-- ; return (combine root_canon fileName)
-- }
--
--addLink :: FilePath -> FilePath -> ManifestTable -> ManifestTable
--addLink source target mp = 
--     Map.insert source    (ManifestEntry{content= [Link target], sources = [source], status= Valid}) mp
--
--
--updateWithLink :: ManifestEntry -> FilePath -> FilePath ->  Manifest -> ManifestTable -> ManifestTable
--updateWithLink (ManifestEntry {content=lN0, sources=src1, status=status1}) source target manifest table = 
--   let newEntry = ManifestEntry{ content = (Link  target) : lN0
--                                , sources = source : src1
--                                , status = combineStatus NotValidated status1
--                                }
--   in (Map.insert source newEntry table)      
--
--
--updateManifestWithComp :: ForestMD fmd => fmd -> [FilePath] -> Manifest -> IO Manifest
--updateManifestWithComp fmd files manifest = do
--    { let fullPath = get_fullpath fmd
--    ; canonPath <- canonicalizePath fullPath
--    ; print ("entering comprehension in directory  " ++ canonPath)
--    ; updateManifestWithCompRaw canonPath files manifest   
--    }
--
--updateManifestWithCompRaw :: FilePath -> [FilePath] ->  Manifest -> IO Manifest
--updateManifestWithCompRaw  compDir files (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = do
--  { new_entries <-  case Map.lookup compDir orig_entries of
--                          Nothing -> return (addComp compDir files orig_entries)   -- record link from source of sym link to canon Path
--                          Just manifestEntry -> return (updateWithComp manifestEntry compDir files manifest orig_entries)
--  ; return (Manifest{count, pathToRoot,tempDir, entries=new_entries})
--  }
--
--
--addComp :: FilePath -> [FilePath] -> ManifestTable -> ManifestTable
--addComp compDir files mp = 
--     Map.insert compDir    (ManifestEntry{content= [ListComp files], sources = [compDir], status= NotValidated}) mp
--
--
--updateWithComp :: ManifestEntry -> FilePath -> [FilePath] ->  Manifest -> ManifestTable -> ManifestTable
--updateWithComp (ManifestEntry {content=lN0, sources=src1, status=status1}) compDir files manifest table = 
--   let newEntry = ManifestEntry{ content = (ListComp  files) : lN0
--                                , sources = compDir : src1
--                                , status = combineStatus NotValidated status1
--                                }
--   in (Map.insert compDir newEntry table)      
--
 

	

