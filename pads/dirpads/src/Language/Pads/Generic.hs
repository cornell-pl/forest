module Language.Pads.Generic (
      Pads(..), 
      Pads1(..), 
      gdef
   )

where

import Language.Pads.MetaData
import Language.Pads.PadsParser
import qualified Language.Pads.Source as S

import Data.Data

class (Data pads, PadsMD md) => Pads pads md | pads -> md  where
  def :: pads
  def = gdef
  getMetaD :: (rep, md) -> Base_md
  getMetaD (rep, md) = get_md_header md
  parsePP  :: PadsParser(pads,md)
  parseS   :: String -> ((pads, md), String) 
  parseS cs = case (runPP parsePP  (S.padsSourceFromString cs)) of
                 Good ((r,rest):alternates) -> (r, S.padsSourceToString rest)  
                 Bad   (r,rest)             -> (r, S.padsSourceToString rest)  


class Data pads => Pads1 arg pads md | pads->md, pads->arg where
  def1 :: arg -> pads
  def1 =  \_ -> gdef
  parsePP1  :: arg -> PadsParser(pads,md)
  parseS1   :: arg -> String -> ((pads, md), String) 
  parseS1 arg cs = case (runPP (parsePP1 arg)) (S.padsSourceFromString cs) of
                      Good ((r,rest):alternates) -> (r, S.padsSourceToString rest)  
                      Bad   (r,rest)             -> (r, S.padsSourceToString rest)  



{- Generic function for computing the default for any type supporting Data a interface -}
getConstr :: DataType -> Constr
getConstr ty = 
   case dataTypeRep ty of
        AlgRep cons -> head cons
        IntRep      -> mkIntegralConstr ty 0
        FloatRep    -> mkRealConstr ty 0.0 
        CharRep     -> mkCharConstr ty '\NUL'
        NoRep       -> error "PADSC: Unexpected NoRep in PADS type"

gdef :: Data a => a
gdef = def_help 
  where
    def_help
     =   let ty = dataTypeOf (def_help)
             constr = getConstr ty
         in fromConstrB gdef constr 
