{-# LANGUAGE ScopedTypeVariables #-}
module Language.Forest.Graph where

import Language.Forest.Forestc
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as MG
import Data.GraphViz
import System.FilePath.Posix
import Data.Map 
import Data.List

type NodeTy = Forest_md
type EdgeLabel = ()


defaultGraphVizParams :: GraphvizParams NodeTy EdgeLabel () NodeTy = Params
  { isDirected = True
  , globalAttributes = [GraphAttrs [Ordering "out"]]
  , clusterBy = N
  , clusterID = const Nothing
  , fmtCluster = const []
  , fmtNode =  displayNodes
  , fmtEdge =  const []
  }

displayNodes :: LNode Forest_md -> Attributes
displayNodes (n,fmd) = 
  let fInfo = fileInfo fmd
      full = fullpath fInfo
      name = takeFileName full
      shape = case kind fInfo of 
               BinaryK  -> [Shape Parallelogram]
               AsciiK   -> [Shape BoxShape]
               otherwise -> []
      color = if numErrors fmd > 0
                then [Color[myred]]
                else []
  in [FontName "Courier", mkLabel name] ++ color ++ shape


mdToPDF md filePath = mdToPDFWtihParams defaultGraphVizParams md filePath 

mdToPDFWtihParams params md filePath = let 
  dg = toDotGraphWithParams params md
  in runGraphviz dg Pdf filePath

toDotGraph md = toDotGraphWithParams defaultGraphVizParams md 

toDotGraphWithParams params md = let
  (nodes,edges) = getNodesAndEdgesMD md
  g = myMkGraph nodes edges
  dg = graphToDot params g
  in dg

getNodesAndEdgesMD md = 
  let allpaths = Data.List.nub(listMDNonEmptyFiles md)
      fileNames = Prelude.map (fullpath . fileInfo) allpaths
      idMap = Data.Map.fromList (zip fileNames [0..])
  in getNodesAndEdges idMap allpaths ([], [])

getNodesAndEdges idMap []        (nodes, edges) = (nodes, edges)
getNodesAndEdges idMap (finfo:finfos) (nodes, edges) = 
   let f = fullpath(fileInfo finfo)
       f_id = idMap ! f
       node = (f_id,finfo)
       parent = takeDirectory f
       new_edges = case Data.Map.lookup parent idMap of
                    Just parent_id -> [(parent_id, f_id, ())]
                    Nothing -> []
   in getNodesAndEdges idMap finfos (node:nodes, new_edges++edges)

myMkGraph :: [LNode NodeTy] -> [LEdge EdgeLabel] -> MG.Gr NodeTy EdgeLabel
myMkGraph = mkGraph

mkLabel s = Label (StrLabel s)
myred = X11Color Red
