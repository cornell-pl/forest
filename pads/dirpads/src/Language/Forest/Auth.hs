module Language.Forest.Auth where
import Language.Forest.Forestc
import Data.Map
import qualified Data.Set  as S
import qualified Data.List as L

import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)

restrictingPaths :: [(FilePath, [FilePath])] -> [FilePath]
restrictingPaths files = 
  let causes = foldr (\(f,files) accum -> (S.fromList files) `S.union` accum) S.empty files
  in S.toList causes



canRead :: ForestMD md => md -> FilePath -> String -> Either String (Bool, [FilePath])
canRead md fileName id = canDoOpFromMap (mapInfoFiles md) Read id fileName 

canWrite :: ForestMD md => md -> FilePath -> String -> Either String (Bool, [FilePath])
canWrite md fileName id = canDoOpFromMap (mapInfoFiles md) Write id fileName 

canExec :: ForestMD md => md -> FilePath -> String -> Either String (Bool, [FilePath])
canExec md fileName id = canDoOpFromMap (mapInfoFiles md) Execute id fileName 

readProhibited :: ForestMD md => md -> String ->  [(FilePath, [FilePath])]
readProhibited md id = opProhibited md Read id

writeProhibited :: ForestMD md => md -> String ->  [(FilePath, [FilePath])]
writeProhibited md id = opProhibited md Write id

execProhibited :: ForestMD md => md -> String ->  [(FilePath, [FilePath])]
execProhibited md id = opProhibited md Execute id

{- Collecting operations on files. -}
opProhibited :: ForestMD md => md -> Permission -> String ->  [(FilePath, [FilePath])]
opProhibited md op id = 
  let mapInfo = mapInfoFiles md
      files = keys mapInfo
      groups = unsafePerformIO $ getGroups id
      allResults = L.map (\f -> (f,canDoOpFromMap' mapInfo op id groups (mapInfo!f) f )) files
      badFiles = L.filter (\(f,(b,files)) -> not b) allResults
      formatedFiles = L.map (\(f,(b,files)) -> (f,files)) badFiles
  in formatedFiles


{- Supplied fileName should be relative to the same starting point given to the load function
   that produced the supplied meta data.
-}
canDoOp :: ForestMD md => md -> Permission -> FilePath -> String -> Either String (Bool, [FilePath])
canDoOp md op fileName id = canDoOpFromMap (mapInfoFiles md) op id fileName 


canDoOpFromMap :: Map FilePath FileInfo -> Permission -> String -> FilePath -> Either String (Bool, [FilePath])
canDoOpFromMap infoMap op id filePath = 
  case Data.Map.lookup filePath infoMap of
    Nothing -> Left ("File " ++ filePath ++ " does not exist in argument directory structure")
    Just fInfo -> let groups = unsafePerformIO $ getGroups id
                  in Right (canDoOpFromMap' infoMap op id groups fInfo filePath)

canDoOpFromMap' :: Map FilePath FileInfo -> Permission -> String -> [String] -> FileInfo -> FilePath -> (Bool, [FilePath])
canDoOpFromMap' infoMap op id groups fInfo filePath = 
  let canDo = op `elem` (getPermissions id groups fInfo)
      problemFiles = if canDo then [] else [filePath]
      parentPath = takeDirectory filePath
  in canDoOpOnPath infoMap op id groups parentPath canDo problemFiles

canDoOpOnPath :: Map FilePath FileInfo -> Permission -> String -> [String] -> FilePath -> Bool -> [FilePath] -> (Bool,[FilePath])
canDoOpOnPath infoMap op id groups filePath canDo problemFiles = 
  case Data.Map.lookup filePath infoMap of
    Nothing -> (canDo, problemFiles)       -- Have reached the top of the directory tree
    Just fInfo -> let fpPermissions = getPermissions id groups fInfo
                      parentPath = takeDirectory filePath
                      newCanDo = op `elem` fpPermissions
                      newProblemFiles = if newCanDo then [] else [filePath]
                  in  canDoOpOnPath infoMap op id groups parentPath (canDo && newCanDo) (newProblemFiles ++ problemFiles)




{- Supplied fileName should be relative to the same starting point given to the load function
   that produced the supplied meta data.
-}
checkAuth :: ForestMD md => md -> FilePath -> String -> Either String ([Permission], [Permission])
checkAuth md fileName id = checkAuthFromMap (mapInfoFiles md) fileName id


checkAuthFromMap :: Map FilePath FileInfo -> FilePath -> String -> Either String ([Permission],[Permission])
checkAuthFromMap infoMap filePath id = 
  case Data.Map.lookup filePath infoMap of
    Nothing -> Left ("File " ++ filePath ++ " does not exist in argument directory structure")
    Just fInfo -> let groups = unsafePerformIO $ getGroups id
                      filePermissions = getPermissions id groups fInfo
                      parentPath = takeDirectory filePath
                  in Right (checkAuthOnPath infoMap parentPath id groups filePermissions [Read,Write,Execute])

checkAuthOnPath :: Map FilePath FileInfo -> FilePath -> String -> [String] -> [Permission] -> [Permission] -> ([Permission],[Permission])
checkAuthOnPath infoMap filePath id groups filePermissions dirPermissions = 
  case Data.Map.lookup filePath infoMap of
    Nothing -> (dirPermissions, filePermissions)       -- Have reached the top of the directory tree
    Just fInfo -> let fpPermissions = getPermissions id groups fInfo
                      newDirPermissions = L.intersect fpPermissions dirPermissions
                      parentPath = takeDirectory filePath
                  in  checkAuthOnPath infoMap parentPath id groups filePermissions newDirPermissions

