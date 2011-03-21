{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Language.Forest.Writing where

import System.Directory
import System.FilePath.Posix
import System.Cmd
import System.IO

import Language.Forest.MetaData
import Language.Pads.Padsc


import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Message = String

-- Note: we should be checking that permissions and owners are also consistent?

data Content = Local FilePath     -- contents are in file in temp directory with given name
             | Link FilePath      -- contents are symbolic link, with given target
             | Dir
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
       deriving (Show)

type ManifestTable = Map.Map FilePath ManifestEntry

data Manifest = Manifest 
     { count   :: Int             -- used for generating unique local names
     , tempDir :: FilePath        -- temp directory in which local files are written
     , entries :: ManifestTable   -- map from canonical names to manifest entries
     } deriving (Show)

newManifest :: IO Manifest
newManifest = do 
  forestDir <- getTempForestManifestDirectory
  return (Manifest
           { count = 0
           , tempDir = forestDir
           , entries = Map.empty
           })


storeManifest :: Manifest -> IO ()
storeManifest  (Manifest {tempDir, entries, ..}) = do
  { let mlist = Map.toList entries
  ; mapM_ (storeManifestEntryAt [] [] tempDir) mlist
  }

storeManifestAt :: FilePath -> FilePath -> Manifest -> IO ()
storeManifestAt destDir clipPath (Manifest {tempDir, entries, ..}) = do
  { let mlist = Map.toList entries
  ; mapM_ (storeManifestEntryAt destDir clipPath tempDir) mlist
  }


validateManifest :: Manifest -> IO Manifest
validateManifest (Manifest {tempDir, entries, count}) = do
  { let mlist = Map.toList entries
  ; mlist' <- mapM (detectConflictInEntry tempDir) mlist
  ; return (Manifest{count, tempDir, entries = (Map.fromList mlist') })
  }

cleanManifest :: Manifest -> IO ()
cleanManifest manifest = do
  { let forestDir = tempDir manifest
  ; let cmd = "rm -rf " ++ forestDir
  ; doShellCmd  cmd
  ; return ()
  }

collectManifestErrors :: Manifest -> Status
collectManifestErrors manifest = joinStatus (collectErrorsManifestTable (entries manifest))

collectErrorsManifestTable :: ManifestTable -> [Status]
collectErrorsManifestTable entries = List.map (\(fp, entry) -> status entry) (Map.toList entries)


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
      None -> do
        { doShellCmd ("rm -rf "  ++ targetPath)
        ; return ()
        } 

  }


-- should check to make sure that resulting gzip file does not conflict with any other files
-- really have to do this at the end; perhaps all validation should be done after manifest is generated?
-- ie, split into three phases: 1. generate manifest, 2. validate manifest, 3. store manifest?
-- need to handle possible failure of gzip command as well.

gzipManifestEntry :: ForestMD fmd => fmd -> Manifest -> IO Manifest
gzipManifestEntry fmd (Manifest {count, tempDir, entries}) = do
  { let fp = get_fullpath fmd
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
  ; return (Manifest{count, tempDir, entries=entries''})
  }

getNewLocalName :: FilePath -> Manifest -> (FilePath, FilePath, Manifest)
getNewLocalName baseName (Manifest{count=oldCount,tempDir,entries}) = 
 let localName =  baseName ++ (show oldCount)
 in ( localName
    , combine tempDir localName
    , Manifest {count = oldCount + 1, tempDir, entries}
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
   ; let baseName = takeBaseName canonPath
   ; let (localName, targetPath, manifest1) = getNewLocalName baseName manifest0
   ; printF targetPath payload 
   ; return (updateManifestRaw canonPath fullPath localName isLink manifest1)
   }

updateManifestRaw :: FilePath -> FilePath -> FilePath -> Maybe FilePath -> Manifest -> Manifest
updateManifestRaw  canonPath origPath localName optLinkTarget (manifest @ Manifest{count,tempDir,entries=orig_entries}) = 
   let entries_withFile = case Map.lookup canonPath orig_entries of 
                            Nothing -> addFile canonPath origPath localName orig_entries
                            Just manifestEntry -> updateFile manifestEntry canonPath origPath localName manifest orig_entries
       new_entries = case optLinkTarget of 
                            Just target -> 
                              case Map.lookup origPath entries_withFile of
                                 Nothing ->  addLink origPath canonPath orig_entries   -- record link from source of sym link to canon Path
                                 Just manifestEntry -> updateWithLink manifestEntry origPath canonPath manifest orig_entries
                            Nothing ->   entries_withFile
   in  Manifest{count, tempDir, entries=new_entries}


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
updateManifestWithTarRaw  tarredFile status tarPath (manifest @ Manifest{count,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup tarPath orig_entries of
                          Nothing -> addTar tarredFile status tarPath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithTar manifestEntry tarredFile status tarPath manifest orig_entries
  in (Manifest{count, tempDir, entries=new_entries})

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
updateManifestWithNoneRaw  nonePath (manifest @ Manifest{count,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup nonePath orig_entries of
                          Nothing -> addNone nonePath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithNone manifestEntry nonePath manifest orig_entries
  in (Manifest{count, tempDir, entries=new_entries})

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
updateManifestWithDirRaw  dirPath (manifest @ Manifest{count,tempDir,entries=orig_entries}) = 
  let  new_entries =  case Map.lookup dirPath orig_entries of
                          Nothing -> addDir dirPath orig_entries   -- record link from source of sym link to canon Path
                          Just manifestEntry -> updateWithDir manifestEntry dirPath manifest orig_entries
  in (Manifest{count, tempDir, entries=new_entries})

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
updateManifestWithLinkRaw  source target (manifest @ Manifest{count,tempDir,entries=orig_entries}) = do
  { new_entries <-  case Map.lookup source orig_entries of
                          Nothing -> return (addLink source target orig_entries)   -- record link from source of sym link to canon Path
                          Just manifestEntry -> return (updateWithLink manifestEntry source target manifest orig_entries)
  ; return (Manifest{count, tempDir, entries=new_entries})
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





       

fileIsDifferent :: FilePath -> FilePath -> IO Bool
fileIsDifferent f1 f2 = do
  { let cmd = "diff " ++ f1 ++ " " ++ f2
  ; result <- doShellCmd cmd
  ; return (result /= "")
  }

