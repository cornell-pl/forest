{-# LANGUAGE FlexibleContexts, NamedFieldPuns, RecordWildCards #-}

module Language.Forest.Writing where

import System.Directory
import System.FilePath.Posix
import System.Process
import System.IO

import Language.Forest.MetaData
import Language.Pads.Padsc


import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

type Message = String

-- Note: we should be checking that permissions and owners are also consistent?

data Content = Local FilePath     -- contents are in file in temp directory with given name
             | Link FilePath      -- contents are symbolic link, with given target
             | Dir
             | ListComp [FilePath]
             | None               -- file path should not exist
             | LocalGzip FilePath      -- like local, except file has been gzipped
             | LocalTar  FilePath      -- like local, except file has been tarred
   deriving (Show, Eq)

data Status = Invalid Message | Valid | NotValidated
   deriving (Show, Eq)

data ManifestEntry =  ManifestEntry
         { content       :: [Content]    -- list of content sources for canonical path
         , sources       :: [FilePath]   -- list of files that mapped to canonical path
         , status        :: Status       -- report on validity of manifest
         }  
       deriving (Show,Eq)

type ManifestTable = Map.Map FilePath ManifestEntry

data Manifest = Manifest 
     { count   :: Int             -- used for generating unique local names
     , pathToRoot    :: Maybe FilePath     -- path to parent directory of FileStore
     , tempDir :: FilePath        -- temp directory in which local files are written
     , entries :: ManifestTable   -- map from canonical names to manifest entries
     } deriving (Show)

newManifest :: IO Manifest
newManifest = do 
  forestDir <- getTempForestManifestDirectory
  canonTemp <- canonicalizePath forestDir
  return (Manifest
           { count = 0
           , pathToRoot = Nothing
           , tempDir = canonTemp
           , entries = Map.empty
           })


storeManifest :: Manifest -> IO ()
storeManifest  (Manifest {tempDir, entries, ..}) = do
  { let mlist = Map.toList entries
  ; mapM_ (storeManifestEntryAt [] [] tempDir) mlist
  }

storeManifestAt :: FilePath -> Manifest -> IO ()
storeManifestAt destDir (Manifest {tempDir, pathToRoot, entries, ..}) = do
  { let mlist = Map.toList entries
  ; let clipPath = case pathToRoot of {Nothing -> [] ; Just r -> dropFileName r}
  ; mapM_ (storeManifestEntryAt destDir clipPath tempDir) mlist
  }

storeManifestAt' :: FilePath -> FilePath -> Manifest -> IO ()
storeManifestAt' destDir clipPath (Manifest {tempDir, entries, ..}) = do
  { let mlist = Map.toList entries
  ; mapM_ (storeManifestEntryAt destDir clipPath tempDir) mlist
  }


validateManifest :: Manifest -> IO Manifest
validateManifest (Manifest {tempDir, entries, count, pathToRoot}) = do
  { let mlist = Map.toList entries
  ; mlist' <- mapM (detectConflictInEntry tempDir) mlist
  ; return (Manifest{count, pathToRoot, tempDir, entries = (Map.fromList mlist') })
  }

cleanManifest :: Manifest -> IO ()
cleanManifest manifest = do
  { let forestDir = tempDir manifest
  ; let cmd = "rm -rf " ++ forestDir
  ; doShellCmd  cmd
  ; return ()
  }



setManifestRoot :: (ForestMD fmd) => fmd -> Manifest -> IO Manifest
setManifestRoot fmd (m @ Manifest{count,pathToRoot,tempDir,entries}) = case pathToRoot of 
  Nothing -> do { canon_path <- canonicalizePath (get_fullpath fmd)
                ; return  ( Manifest{count,pathToRoot = Just canon_path,tempDir,entries})
                }
  Just _  -> return m

collectManifestErrors :: Manifest -> Status
collectManifestErrors manifest = joinStatus (collectErrorsManifestTable (entries manifest))

collectErrorsManifestTable :: ManifestTable -> [Status]
collectErrorsManifestTable entries = List.map (\(fp, entry) -> status entry) (Map.toList entries)

detectConflictsInListComps :: Manifest -> Manifest -> Manifest
detectConflictsInListComps (Manifest{count,pathToRoot=orig_pathToRoot,tempDir,entries=orig_entries}) test_manifest = 
  let orig_rootDir = dropFileName (Maybe.fromJust orig_pathToRoot)
      test_rootDir = dropFileName (Maybe.fromJust (pathToRoot test_manifest))
      orig_entries' = listCompConflictsInManifestEntities test_rootDir (entries test_manifest) orig_rootDir orig_entries
  in Manifest{count,pathToRoot=orig_pathToRoot,tempDir,entries=orig_entries'}

listCompConflictsInManifestEntities test_rootDir test_entries orig_rootDir orig_entries = 
  Map.mapWithKey (updateOne test_rootDir test_entries orig_rootDir) orig_entries

updateOne test_rootDir test_entries orig_rootDir orig_pathToEntry orig_entry = 
  let orig_RelPath = Maybe.fromJust (List.stripPrefix orig_rootDir orig_pathToEntry)
      test_pathToEntry = combine test_rootDir orig_RelPath
      test_entry = test_entries Map.! test_pathToEntry
  in listCompConflictsInManifestEntry orig_entry test_entry

listCompConflictsInManifestEntry (ManifestEntry{content=orig_content,sources=orig_sources,status=orig_status})
                                 (ManifestEntry{content=test_content,sources=test_sources,status=test_status}) = 
  let orig_complists = List.filter isListComp orig_content
      test_complists = List.filter isListComp test_content
      new_status = if orig_complists == test_complists then 
                      promoteNotValidated orig_status
                   else Invalid ("Conflict detected in list comprehensions: original comprehension had "  
                                 ++ (show orig_complists) 
                                 ++ "while after printing comprehension had "
                                 ++ (show test_complists))
  in ManifestEntry{content=orig_content,sources=orig_sources,status=new_status}


diffManifest :: Manifest -> Manifest -> [(FilePath, [FilePath])]
diffManifest (Manifest{count,pathToRoot=source_pathToRoot,tempDir,entries=source_entries}) target_manifest = []
{-
  let source_rootDir  = dropFileName (Maybe.fromJust source_pathToRoot)
      target_rootDir  = dropFileName (Maybe.fromJust (pathToRoot target_manifest))
      source_entriesL = Map.toList source_entries
      target_entriesL = Map.toList (entries target_manifest)
      source_complists = unwrapComp (List.filter isListComp' source_entriesL)
      target_complists = unwrapComp (List.filter isListComp' target_entriesL)
      source_complists' = List.groupBy (\(f1,c1) (f2,c2)  -> f1 == f2) source_complists
      target_complists' = List.groupBy (\(f1,c1) (f2,c2)  -> f1 == f2) target_complists
      source_complists'' = let (fps, contents) = List.unzip source_complists' in (List.head fps, contents)
      target_complists'' = let (fps, contents) = List.unzip target_complists' in (List.head fps, contents)
  -- r
  in -- Map.toList (diffManifestEntities target_rootDir (entries target_manifest) source_rootDir source_entries)
     []
-}

diffManifestEntities target_rootDir target_entries source_rootDir source_entries = 
  Map.mapWithKey (diffOne target_rootDir target_entries source_rootDir) source_entries

diffOne target_rootDir target_entries source_rootDir source_pathToEntry source_entry = 
  let source_RelPath = Maybe.fromJust (List.stripPrefix source_rootDir source_pathToEntry)
      target_pathToEntry = combine target_rootDir source_RelPath
  in case Map.lookup target_pathToEntry target_entries of
       Nothing -> []
       Just target_entry -> diffManifestEntry  source_entry target_entry

diffManifestEntry (ManifestEntry{content=source_content,sources=source_sources,status=source_status})
                  (ManifestEntry{content=target_content,sources=target_sources,status=target_status}) = 
  let source_complists = List.concatMap unwrapComp (List.filter isListComp source_content)
      target_complists = List.concatMap unwrapComp (List.filter isListComp target_content)
  -- return everything in targets that is not in source
      sourceSet = Set.fromList source_complists
      targetSet = Set.fromList target_complists
      diff = Set.difference targetSet sourceSet
  in Set.toList diff
 

unwrapComp (ListComp x) = x
unwrapComp' (fp, ListComp x) = (fp,x)

isListComp :: Content -> Bool
isListComp c = case c of
  ListComp _ -> True
  otherwise  -> False

isListComp' :: (FilePath, Content) -> Bool
isListComp' (fp,c) = case c of
  ListComp _ -> True
  otherwise  -> False




detectConflictInEntry :: FilePath -> (FilePath, ManifestEntry) -> IO (FilePath, ManifestEntry)
detectConflictInEntry manifestDir (fp, ManifestEntry{content,sources,status}) = do 
  { status' <- detectContentConflict manifestDir (List.zip content sources)
  ; return (fp, ManifestEntry{content,sources,status = status'})
  }

detectContentConflict :: FilePath -> [(Content, FilePath)] -> IO Status
detectContentConflict manifestDir content_fp = case content_fp of
  [] ->  return Valid
  [x] -> return Valid
  (x:y:rest) -> do { cxy    <- detectPairConflict manifestDir x y
                   ; cyrest <- detectContentConflict manifestDir (y:rest)
                   ; return (combineStatus cxy cyrest)
                   }

joinStatus :: [Status] -> Status
joinStatus ss = List.foldl combineStatus Valid ss

promoteNotValidated :: Status -> Status
promoteNotValidated status = case status of 
  NotValidated -> Valid
  x -> x

combineStatus :: Status -> Status -> Status
combineStatus s1 s2 = case (s1,s2) of
  (Invalid m1, Invalid m2) -> (Invalid (m1 ++ "; " ++ m2))
  (i @ (Invalid _), _) -> i
  (_, i @ (Invalid _)) -> i
  (Valid, NotValidated) -> NotValidated
  (NotValidated, Valid) -> NotValidated
  (NotValidated, NotValidated) -> NotValidated
  (Valid, Valid) -> Valid

detectPairConflict :: FilePath -> (Content, FilePath) -> (Content, FilePath) -> IO Status
detectPairConflict manifestDir (c1,s1) (c2,s2) = case (c1,c2) of
  (Local fp1, Local fp2) -> do
        { isDifferent <- fileIsDifferent (combine manifestDir fp1) (combine manifestDir fp2)
        ; if isDifferent then return (Invalid ("File " ++ s1 ++ " is in conflict with file " ++ s2))
                         else return Valid
        }
  (LocalGzip fp1, LocalGzip fp2) -> do
        { isDifferent <- fileIsDifferent (combine manifestDir fp1) (combine manifestDir fp2)
        ; if isDifferent then return (Invalid ("Gzipped file " ++ s1 ++ " is in conflict with gzipped file " ++ s2))
                         else return Valid
        }
  (LocalTar fp1, LocalTar fp2) -> do
        { isDifferent <- fileIsDifferent (combine manifestDir fp1) (combine manifestDir fp2)
        ; if isDifferent then return (Invalid ("Tarred file " ++ s1 ++ " is in conflict with Tarred file " ++ s2))
                         else return Valid
        }
  (None, None) -> return Valid
  (Link target1, Link target2) -> 
        if target1 == target2 then return Valid
                              else return (Invalid ("Link " ++ target1 ++ " clashed with target " ++ target2))
  (Dir, Dir) -> return Valid   --- update this to manage conflicts with contained comprehensions
  (ListComp f1, ListComp f2) -> return NotValidated  -- We will check this in the next stage...
  (ListComp _, Dir) -> return Valid
  (Dir, ListComp _) -> return Valid
  (t1, t2) -> return (Invalid ((show t1) ++ " is in conflict with "  ++ (show t2)))

  
getTargetPath' destDir clipPath fullPath = 
  case List.stripPrefix clipPath fullPath of
    Nothing -> combine destDir fullPath
    Just suffix -> combine destDir suffix

getClipPathFromTarManifest :: Manifest -> FilePath -> FilePath
getClipPathFromTarManifest manifest scratchDir = 
  let entries' = entries manifest
      paths = Map.keys entries'
      paths' = List.filter (List.isPrefixOf scratchDir) paths
    in (List.head paths') ++ "/"
      
getTempForestManifestDirectory :: IO FilePath
getTempForestManifestDirectory = do 
  { tempDir <- getTempForestDirectory 
  ; let forestManifestDir = combine tempDir "Manifests"
  ; createDirectoryIfMissing False forestManifestDir
--  ; (suffix :: Int) <- randomIO
--  ; let fp = "Manifest_"  ++ (show suffix)
  ; (fp, handle) <- openTempFile forestManifestDir "Manifest"
  ; hClose handle
  ; system ("rm -f " ++ fp)
  ; print ("temp directory name is: " ++ fp)
  ; createDirectoryIfMissing False fp
  ; return fp
  }


getTempForestListDirectory :: IO FilePath
getTempForestListDirectory = do 
  { tempDir <- getTempForestDirectory 
  ; let forestManifestDir = combine tempDir "Lists"
  ; createDirectoryIfMissing False forestManifestDir
  ; (fp, handle) <- openTempFile forestManifestDir "List"
  ; hClose handle
  ; system ("rm -f " ++ fp)
  ; print ("temp list directory name is: " ++ fp)
  ; createDirectoryIfMissing False fp
  ; return fp
  }


storeManifestEntryAt :: FilePath -> FilePath -> FilePath -> (FilePath, ManifestEntry) -> IO ()
storeManifestEntryAt destDir clipPath tempDir (canonPath, (ManifestEntry {content, ..}))  = do
  { let targetPath = getTargetPath' destDir clipPath canonPath
  ; print ("DestDir = " ++ destDir)
  ; print ("canonPath = " ++ canonPath)
  ; print ("clipPath = " ++ clipPath)
  ; print ("targetPath = " ++ targetPath)
  ; createDirectoryIfMissing True (takeDirectory targetPath)
  ; case List.head content of 
      Local localName -> do 
        { let srcPath = combine tempDir localName
        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
        ; return ()
        }
      LocalTar localName -> do 
        { let srcPath = combine tempDir localName
        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
        ; return ()
        }
      LocalGzip localName -> do 
        { let srcPath = combine tempDir localName
        ; doShellCmd ("cp " ++ srcPath ++ " " ++ targetPath )
        ; return ()
        }
      Link linkDest -> do
        { doShellCmd ("ln -s " ++ linkDest ++ " " ++ targetPath)
        ; return ()
        }
      Dir -> return ()  -- Nothing needs to be done in this case because dir is already created
      ListComp files -> return () -- Nothing needs to be done in this case because files are listed separately
      None -> do
        { doShellCmd ("rm -rf "  ++ targetPath)
        ; return ()
        } 

  }


gzipManifestEntry :: ForestMD fmd => fmd -> Manifest -> IO Manifest
gzipManifestEntry fmd (Manifest {count, pathToRoot, tempDir, entries}) = do
  { fp <- canonicalizePath (get_fullpath fmd)
  ; let fp_noZip = dropExtension fp
  ; print ("gzipping filepath: " ++ fp_noZip)
  ; let (ManifestEntry {content = (c1:cs), sources, status=orig_status}) = entries Map.! fp_noZip
  ; entry' <-  case c1 of 
                Local localName -> do 
                   { let srcPath = combine tempDir localName
                   ; doShellCmd ("gzip " ++ srcPath )
                   ; return (ManifestEntry {content = LocalGzip (localName ++ ".gz"):cs, sources, status = combineStatus NotValidated orig_status})
                   }
                LocalTar tarName -> do 
                   { let srcPath = combine tempDir tarName
                   ; doShellCmd ("gzip " ++ srcPath )
                   ; return (ManifestEntry {content = LocalGzip (tarName ++ ".gz"):cs, sources, status = combineStatus NotValidated orig_status})
                   }
                otherwise -> do
                  { let newStatus = Invalid ("Cannot zip a " ++ (show c1))
                  ; return (ManifestEntry {content = c1:cs, sources, status = combineStatus orig_status newStatus})
                  }
  ; let entries'  = Map.delete fp_noZip entries
  ; let entries'' = Map.insert fp entry' entries'
  ; return (Manifest{count, pathToRoot, tempDir, entries=entries''})
  }

getNewLocalName :: FilePath -> Manifest -> (FilePath, FilePath, Manifest)
getNewLocalName baseName (Manifest{count=oldCount,pathToRoot, tempDir,entries}) = 
 let localName =  baseName ++ (show oldCount)
 in ( localName
    , combine tempDir localName
    , Manifest {count = oldCount + 1, pathToRoot,tempDir, entries}
    )


stripSharedPrefix :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
stripSharedPrefix [] [] shared = (List.reverse shared, [], [])
stripSharedPrefix [] b  shared = (List.reverse shared, [], b)
stripSharedPrefix a  [] shared = (shared, a,  [])
stripSharedPrefix a @ (x:xs)  b @ (y:ys) shared = 
   if x == y then stripSharedPrefix  xs ys (x:shared)
             else (List.reverse shared, a, b)

getTargetPath :: FilePath -> FilePath -> FilePath
getTargetPath targetDir fullPath = 
  let (shared, target, residual) = stripSharedPrefix targetDir fullPath []
  in  combine targetDir residual



-- Add a file
updateManifestPads :: (Pads rep md, ForestMD fmd) => (rep, (fmd,md)) -> Manifest -> IO Manifest
updateManifestPads (rep, (fmd, md)) manifest = updateManifestWith printFile (rep,md) fmd manifest

updateManifestPads1 :: (Pads1 arg rep md, ForestMD fmd) => arg -> (rep, (fmd,md)) ->  Manifest -> IO Manifest
updateManifestPads1 arg (rep, (fmd,md)) manifest = updateManifestWith (printFile1 arg) (rep,md) fmd manifest


updateManifestWith :: ForestMD fmd => (FilePath -> payload -> IO ()) -> payload -> fmd -> Manifest -> IO Manifest
updateManifestWith printF payload fmd manifest0 = do
   { let fullPath = get_fullpath fmd
   ; let isLink   = get_symLink fmd
   ; if Maybe.isJust isLink then print ("found symlink:" ++ fullPath) else print ("not symlink: " ++ fullPath)
   ; canonPath <- canonicalizePath fullPath
   ; mostlyCanonPath <- mostlyCanonicalizeLink fullPath
   ; let baseName = takeBaseName canonPath
   ; let (localName, targetPath, manifest1) = getNewLocalName baseName manifest0
   ; printF targetPath payload 
   ; return (updateManifestRaw canonPath mostlyCanonPath localName isLink manifest1)
   }

updateManifestRaw :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> Manifest -> Manifest
updateManifestRaw  canonPath origPath localName optLinkTarget (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
   let entries_withFile = case Map.lookup canonPath orig_entries of 
                            Nothing -> addFile canonPath origPath localName orig_entries
                            Just manifestEntry -> updateFile manifestEntry canonPath origPath localName manifest orig_entries
       new_entries = case optLinkTarget of 
                            Just target -> 
                              case Map.lookup origPath entries_withFile of
                                 Nothing ->  addLink origPath canonPath orig_entries   -- record link from source of sym link to canon Path
                                 Just manifestEntry -> updateWithLink manifestEntry origPath canonPath manifest orig_entries
                            Nothing ->   entries_withFile
   in  Manifest{count, pathToRoot, tempDir, entries=new_entries}


addFile :: FilePath -> FilePath -> FilePath -> ManifestTable -> ManifestTable
addFile canonPath origPath local mp = 
     Map.insert canonPath (ManifestEntry{content = [Local local], sources= [origPath], status = Valid}) mp


updateFile :: ManifestEntry -> FilePath -> FilePath -> FilePath -> Manifest -> ManifestTable -> ManifestTable
updateFile (ManifestEntry {content=lN0, sources=src1, status=status1}) canonPath origPath localName manifest table = 
    let localDir = tempDir manifest
        newEntry = ManifestEntry{ content = (Local localName) : lN0
                                 , sources = origPath : src1
                                 , status = combineStatus status1 NotValidated
                                 }
   in Map.insert canonPath newEntry table      


updateManifestWithTar :: ForestMD fmd => FilePath -> Status -> fmd -> Manifest -> IO Manifest
updateManifestWithTar tarredFile status fmd manifest = do
   { let fullPath = get_fullpath fmd
   ; canonPath <- canonicalizePath fullPath
   ; let manifest' = updateManifestWithTarRaw tarredFile status canonPath manifest
   ; return manifest'
   }

updateManifestWithTarRaw :: FilePath -> Status -> FilePath ->  Manifest ->  Manifest
updateManifestWithTarRaw  tarredFile status tarPath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup tarPath orig_entries of
                          Nothing -> addTar tarredFile status tarPath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithTar manifestEntry tarredFile status tarPath manifest orig_entries
  in (Manifest{count, pathToRoot,tempDir, entries=new_entries})

addTar :: FilePath -> Status -> FilePath -> ManifestTable -> ManifestTable
addTar tarredFile tarStatus tarPath mp = 
     Map.insert tarPath  (ManifestEntry{content= [LocalTar tarredFile], sources = [tarPath], status = tarStatus}) mp

updateWithTar :: ManifestEntry -> FilePath -> Status -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
updateWithTar (ManifestEntry {content=lN0, sources=src1, status=status1}) tarredPath tarStatus tarPath manifest table = 
   let newEntry = ManifestEntry{ content = (LocalTar tarredPath) : lN0
                               , sources = tarPath : src1
                               , status = combineStatus tarStatus status1
                               }
   in Map.insert tarPath newEntry table      


updateManifestWithNone :: ForestMD fmd => fmd -> Manifest -> IO Manifest
updateManifestWithNone fmd manifest = do
   { let fullPath = get_fullpath fmd
   ; canonPath <- canonicalizePath fullPath
   ; let manifest' = updateManifestWithNoneRaw canonPath manifest
   ; return manifest'
   }

updateManifestWithNoneRaw :: FilePath ->  Manifest ->  Manifest
updateManifestWithNoneRaw  nonePath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup nonePath orig_entries of
                          Nothing -> addNone nonePath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithNone manifestEntry nonePath manifest orig_entries
  in (Manifest{count, pathToRoot, tempDir, entries=new_entries})

addNone :: FilePath -> ManifestTable -> ManifestTable
addNone nonePath mp = 
     Map.insert nonePath  (ManifestEntry{content= [None], sources = [nonePath], status = Valid}) mp

updateWithNone :: ManifestEntry -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
updateWithNone (ManifestEntry {content=lN0, sources=src1, status=status1}) nonePath manifest table = 
   let newEntry = ManifestEntry{ content = None : lN0
                               , sources = nonePath : src1
                               , status = combineStatus NotValidated status1
                               }
   in Map.insert nonePath newEntry table      

updateManifestWithDir :: ForestMD fmd => fmd -> Manifest -> IO Manifest
updateManifestWithDir fmd manifest = do
   { let fullPath = get_fullpath fmd
   ; canonPath <- canonicalizePath fullPath
   ; let manifest' = updateManifestWithDirRaw canonPath manifest
   ; return manifest'
   }

updateManifestWithDirRaw :: FilePath ->  Manifest ->  Manifest
updateManifestWithDirRaw  dirPath (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup dirPath orig_entries of
                          Nothing -> addDir dirPath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithDir manifestEntry dirPath manifest orig_entries
  in (Manifest{count, pathToRoot,tempDir, entries=new_entries})

addDir :: FilePath -> ManifestTable -> ManifestTable
addDir dirPath mp = 
     Map.insert dirPath  (ManifestEntry{content= [Dir], sources = [dirPath], status = Valid}) mp

updateWithDir :: ManifestEntry -> FilePath ->  Manifest -> ManifestTable ->  ManifestTable
updateWithDir (ManifestEntry {content=lN0, sources=src1, status=status1}) dirPath manifest table = 
   let newEntry = ManifestEntry{ content = Dir : lN0
                               , sources = dirPath : src1
                               , status = combineStatus NotValidated status1
                               }
   in Map.insert dirPath newEntry table      

updateManifestWithLink :: ForestMD fmd => fmd -> Manifest -> IO Manifest
updateManifestWithLink fmd manifest = case get_symLink fmd of 
  Nothing ->  return manifest
  Just linkTarget -> do 
    { let fullPath = get_fullpath fmd
    ; print ("entering symlink: source = " ++ fullPath ++ " Target = " ++ linkTarget)
    ; updateManifestWithLinkRaw fullPath linkTarget manifest   
    }

updateManifestWithLinkRaw :: FilePath -> FilePath ->  Manifest -> IO Manifest
updateManifestWithLinkRaw  source target (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = do
  { new_entries <-  case Map.lookup source orig_entries of
                          Nothing -> return (addLink source target orig_entries)   -- record link from source of sym link to canon Path
                          Just manifestEntry -> return (updateWithLink manifestEntry source target manifest orig_entries)
  ; return (Manifest{count, pathToRoot,tempDir, entries=new_entries})
  }


mostlyCanonicalizeLink :: FilePath -> IO FilePath
mostlyCanonicalizeLink fp = do
 { let root = dropFileName fp
 ; let fileName = takeFileName fp
 ; root_canon <- canonicalizePath root
 ; return (combine root_canon fileName)
 }

addLink :: FilePath -> FilePath -> ManifestTable -> ManifestTable
addLink source target mp = 
     Map.insert source    (ManifestEntry{content= [Link target], sources = [source], status= Valid}) mp


updateWithLink :: ManifestEntry -> FilePath -> FilePath ->  Manifest -> ManifestTable -> ManifestTable
updateWithLink (ManifestEntry {content=lN0, sources=src1, status=status1}) source target manifest table = 
   let newEntry = ManifestEntry{ content = (Link  target) : lN0
                                , sources = source : src1
                                , status = combineStatus NotValidated status1
                                }
   in (Map.insert source newEntry table)      


updateManifestWithComp :: ForestMD fmd => fmd -> [FilePath] -> Manifest -> IO Manifest
updateManifestWithComp fmd files manifest = do
    { let fullPath = get_fullpath fmd
    ; canonPath <- canonicalizePath fullPath
    ; print ("entering comprehension in directory  " ++ canonPath)
    ; updateManifestWithCompRaw canonPath files manifest   
    }

updateManifestWithCompRaw :: FilePath -> [FilePath] ->  Manifest -> IO Manifest
updateManifestWithCompRaw  compDir files (manifest @ Manifest{count,pathToRoot,tempDir,entries=orig_entries}) = do
  { new_entries <-  case Map.lookup compDir orig_entries of
                          Nothing -> return (addComp compDir files orig_entries)   -- record link from source of sym link to canon Path
                          Just manifestEntry -> return (updateWithComp manifestEntry compDir files manifest orig_entries)
  ; return (Manifest{count, pathToRoot,tempDir, entries=new_entries})
  }


addComp :: FilePath -> [FilePath] -> ManifestTable -> ManifestTable
addComp compDir files mp = 
     Map.insert compDir    (ManifestEntry{content= [ListComp files], sources = [compDir], status= NotValidated}) mp


updateWithComp :: ManifestEntry -> FilePath -> [FilePath] ->  Manifest -> ManifestTable -> ManifestTable
updateWithComp (ManifestEntry {content=lN0, sources=src1, status=status1}) compDir files manifest table = 
   let newEntry = ManifestEntry{ content = (ListComp  files) : lN0
                                , sources = compDir : src1
                                , status = combineStatus NotValidated status1
                                }
   in (Map.insert compDir newEntry table)      





       

fileIsDifferent :: FilePath -> FilePath -> IO Bool
fileIsDifferent f1 f2 = do
  { let cmd = "diff " ++ f1 ++ " " ++ f2
  ; result <- doShellCmd cmd
  ; return (result /= "")
  }

