{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-
** *********************************************************************
*                                                                      *
*              This software is part of the pads package               *
*           Copyright (c) 2005-2011 AT&T Knowledge Ventures            *
*                      and is licensed under the                       *
*                        Common Public License                         *
*                      by AT&T Knowledge Ventures                      *
*                                                                      *
*                A copy of the License is available at                 *
*                    www.padsproj.org/License.html                     *
*                                                                      *
*  This program contains certain software code or other information    *
*  ("AT&T Software") proprietary to AT&T Corp. ("AT&T").  The AT&T     *
*  Software is provided to you "AS IS". YOU ASSUME TOTAL RESPONSIBILITY*
*  AND RISK FOR USE OF THE AT&T SOFTWARE. AT&T DOES NOT MAKE, AND      *
*  EXPRESSLY DISCLAIMS, ANY EXPRESS OR IMPLIED WARRANTIES OF ANY KIND  *
*  WHATSOEVER, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF*
*  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF  *
*  TITLE OR NON-INFRINGEMENT.  (c) AT&T Corp.  All rights              *
*  reserved.  AT&T is a registered trademark of AT&T Corp.             *
*                                                                      *
*                   Network Services Research Center                   *
*                          AT&T Labs Research                          *
*                           Florham Park NJ                            *
*                                                                      *
*              Kathleen Fisher <kfisher@research.att.com>              *
*                                                                      *
************************************************************************
-}

module Language.Forest.Graph where

import Data.WithClass.MData
import System.IO
import System.IO.Unsafe
import Control.Monad
--import Control.Monad.IO.Class

import Language.Forest.FS.FSRep
import Language.Forest.Class
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as MG
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import System.FilePath.Posix
import Data.Map 
import Data.List
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Maybe as Maybe
import Control.Monad.Incremental

type NodeTy = Forest_md
type EdgeLabel = ()

type ForestGraphParams fs = GraphvizParams Int (NodeTy fs) EdgeLabel () (NodeTy fs)
defaultGraphVizParams :: (ForestGraphParams fs)
defaultGraphVizParams = defaultParams
  { isDirected = True
  , globalAttributes = [GraphAttrs [Ordering OutEdges, RankDir FromLeft]]
  , clusterBy = N
  , clusterID = Prelude.const (Num $ Int 0)
  , fmtCluster = Prelude.const []
  , fmtNode =  displayNodes
  , fmtEdge =  Prelude.const []
  }

displayNodes :: (Int, Forest_md fs) -> Attributes
displayNodes (_,fmd) =
	let fInfo = fileInfo fmd
	    full = fullpath fInfo
	    name = takeFileName full
	    shape = case kind fInfo of 
			DirectoryK -> [Shape BoxShape]
			BinaryK  -> [PenWidth 2.0]
			AsciiK   -> []
			otherwise -> []
	    symLink' = if Maybe.isJust (symLink fInfo)
			then [Style [SItem Dashed []]]
			else []
--	color <- get_errors fmd >>= \err -> if numErrors err > 0
--		then return [FillColor myGrey, Style[SItem Filled []]]
--		else return []
	    color = []
	in [FontName (Lazy.pack "Courier"), mkLabel (Lazy.pack name)] ++ color ++ shape ++ symLink'

mdToPDF :: (MData NoCtx (ForestO fs) md,ForestMD fs md) => md -> FilePath -> ForestO fs (Maybe String)
mdToPDF md filePath = mdToPDFWithParams defaultGraphVizParams md filePath 

mdToPDFWithParams :: (MData NoCtx (ForestO fs) md,ForestMD fs md) => ForestGraphParams fs -> md -> FilePath -> ForestO fs (Maybe String)
mdToPDFWithParams params md filePath = do
	dg <- toDotGraphWithParams params md
	let txt = printDotGraph dg
	inL $ Text.writeFile "/home/hpacheco/papers2.dot" txt
	result <- forestIO $ runGraphviz dg Pdf filePath
	return (Just result)

toDotGraph md = toDotGraphWithParams defaultGraphVizParams md 

toDotGraphWithParams params md = do
  (nodes,edges) <- getNodesAndEdgesMD md
  let g = myMkGraph nodes edges
      dg = graphToDot params g
  return dg

getNodesAndEdgesMD md = do
  allpaths <- liftM Data.List.nub $ listMDNonEmptyFiles md
  let fileNames = Prelude.map (fullpath . fileInfo) allpaths
      idMap = Data.Map.fromList (zip fileNames [0..])
  return $ getNodesAndEdges idMap allpaths ([], [])

getNodesAndEdges idMap []        (nodes, edges) = (nodes, edges)
getNodesAndEdges idMap (finfo:finfos) (nodes, edges) = 
   let f = fullpath(fileInfo finfo)
       f_id = idMap ! f
       node = (f_id, finfo)
       parent = takeDirectory f
       new_edges = case Data.Map.lookup parent idMap of
                    Just parent_id -> [(parent_id, f_id, ())]
                    Nothing -> []
   in getNodesAndEdges idMap finfos (node:nodes, new_edges++edges)

myMkGraph :: FSRep fs => [LNode (NodeTy fs)] -> [LEdge EdgeLabel] -> MG.Gr (NodeTy fs) EdgeLabel
myMkGraph = mkGraph

mkLabel s = Label (StrLabel s)
myGrey = toColorList [X11Color LightGray]
