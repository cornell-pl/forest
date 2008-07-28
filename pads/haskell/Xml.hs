-- Author: Michel Blockelet (supervised by David Walker)
-- Institution: Princeton University
-- Date: July 08

module Xml where

import Data.Typeable
import Text.XML.HaXml

import Types
import Printer
import Aux


data XmlAnnot where
  Verbose :: Bool -> XmlAnnot
  deriving Typeable


{--- XmlAnnot lookup functions ---}
{- xa_verbose :: verbose mode, output tags for every part of the Pads description -}
xa_verbose :: [Annot] -> Bool
xa_verbose [] = False
xa_verbose (x:xs) = case x of Annot y -> case (cast y) of Just (Verbose b) -> b
                                                          _ -> xa_verbose xs


{--- Helper functions ---}
{- mk_attr_pd :: make the attribute list from the PDErrorCode -}
mk_attr_pd :: PDErrorCode -> [Attribute]
mk_attr_pd Good = []
mk_attr_pd (Bad err rec) = [("error", AttValue [Left "yes"]),
                            ("error_type", AttValue [Left "bad"]),
                            ("error_msg", AttValue [Left err]),
                            ("error_recovered", AttValue [Left (if rec then "yes" else "no")])]
mk_attr_pd (Nested rec) = [("error", AttValue [Left "yes"]),
                           ("error_type", AttValue [Left "nested"]),
                           ("error_recovered", AttValue [Left (if rec then "yes" else "no")])]



{--- XML Generator ---}
toXML :: [Annot] -> PADS x -> (x, PD) -> [Content]
toXML env Pint = \(x, pd) -> case pd of PDbase cpd -> [CElem (Elem "val" (mk_attr_pd cpd) [CString True (printer env Pint x)])]
toXML env Pchar = \(x, pd) -> case pd of PDbase cpd -> [CElem (Elem "val" (mk_attr_pd cpd) [CString True (printer env Pchar x)])]
toXML env Pfloat = \(x, pd) -> case pd of PDbase cpd -> [CElem (Elem "val" (mk_attr_pd cpd) [CString True (printer env Pfloat x)])]
toXML env Punit = \(x, pd) -> case pd of PDbase cpd -> [CElem (Elem "val" (mk_attr_pd cpd) [])]
toXML env (Pbefore s p) = \(x, pd) -> case pd of
                                        (PDdata cpd ipd) -> if (xa_verbose env)
                                                            then [CElem (Elem "before" (mk_attr_pd cpd) (toXML env p (x, ipd)))]
                                                            else toXML env p (x, ipd)
toXML env (Pafter p s) = \(x, pd) -> case pd of
                                        (PDdata cpd ipd) -> if (xa_verbose env)
                                                            then [CElem (Elem "after" (mk_attr_pd cpd) (toXML env p (x, ipd)))]
                                                            else toXML env p (x, ipd)
toXML env (Ppair p1 p2) = \((x1, x2), pd) -> case pd of
                                               (PDlist cpd [pd1, pd2]) -> if (xa_verbose env)
                                                                          then [CElem (Elem "pair" (mk_attr_pd cpd) (concat [toXML env p1 (x1, pd1), toXML env p2 (x2, pd2)]))]
                                                                          else (concat [toXML env p1 (x1, pd1), toXML env p2 (x2, pd2)])
toXML env (Pdpair s p f) = \((x, y), pd) -> case pd of
                                              (PDlist cpd [pd1, pd2]) -> [CElem (Elem "deppair" (mk_attr_pd cpd) [CElem (Elem "var" [] (toXML env p (x, pd1))), CElem (Elem "dep" [] (toXML env (f x) (y, pd2)))])]
toXML env (Plist p s) = \(l, pd) -> case pd of
                                      (PDlist cpd lpd) -> [CElem (Elem "list" (("length", AttValue [Left (show (length l))]):(mk_attr_pd cpd)) (concat (map (\(x, epd) -> (toXML env p (x, epd))) (zip l lpd))))]
toXML env (PlistFW p n) = \(l, pd) -> case pd of
                                        (PDlist cpd lpd) -> [CElem (Elem "list" (("length", AttValue [Left (show (length l))]):(mk_attr_pd cpd)) (concat (map (\(x, epd) -> (toXML env p (x, epd))) (zip l lpd))))]
toXML env (Pnamed p s) = \(x, pd) -> [CElem (Elem s [] (toXML env p (x, pd)))]
toXML env (Pannot p a) = \(x, pd) -> toXML (a:env) p (x, pd)
toXML env (Pdata l) = \(x, pd) -> case (brancher l x) of (BranchedValue (p, y)) -> (toXML env p (y, pd))



{--- XML parser filter generator ---}
-- fromXML :: [Annot] -> PADS x -> Content -> (x, PD)



{--- Export verbatim ... ---}
showXML :: Verbatim a => a -> String
showXML = verbatim
