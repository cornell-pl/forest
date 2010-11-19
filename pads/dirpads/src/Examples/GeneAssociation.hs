{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.GeneAssociation where
import Language.Pads.Padsc
import Language.Forest.Forestc
import System.IO.Unsafe (unsafePerformIO)
import Language.Pads.GenPretty

{-
config_file = "/Users/kfisher/Sites/cgi-bin/PLConfig.pm"
(config_rep, config_md) :: (Config_f, Config_f_md) = unsafePerformIO $ parseFile config_file
(head_rep, head_md) :: (Header_t, Header_t_md) = unsafePerformIO $ parseFile config_file
-}

ws = RE "[ \t]+"
title = "gene_association"
get_gz_file f = title ++ '.' ++ f ++".gz"
get_readme_file f = f ++ ".README"
get_conf_file f = title ++ '.'  ++ f ++ ".conf"

{- each source is a pair (institute name, list of organisms the institute provides) -}
sources = [
	  ("Compugen", [])
	, ("GeneDB", ["Lmajor","Pfalciparum","Spombe","Tbrucei","tsetse"])
	, ("PAMGO", ["Atumefaciens","Ddadantii","Mgrisea","Oomycetes"])
	, ("aspgd", [])
	, ("cgd", [])
	, ("dictyBase", [])
	, ("ecocyc", [])
	, ("fb", [])
	, ("goa", ["arabidopsis","chicken","cow","human","mouse","pdb","rat","uniprot","uniprot_noiea","zebrafish"])
	, ("gramene", ["oryza"])
	, ("jcvi", ["Aphagocytophilum","Banthracis","Cburnetii","Chydrogenoformans","Cjejuni","Cperfringens",
		    "Cpsychrerythraea","Dethenogenes","Echaffeensis","Gsulfurreducens","Hneptunium","Lmonocytogenes",
		    "Mcapsulatus","Nsennetsu","Pfluorescens","Psyringae","phaseolicola","Soneidensis","Spomeroyi",
		    "Vcholerae"])
	, ("mgi", [])
	, ("pseudocap", [])
	, ("reactome", [])
	, ("rgd", [])
	, ("sgd", [])
	, ("sgn", [])
	, ("tair", [])
	, ("wb", [])
	, ("zfin", []) ]


comb_source ((inst, organs):sources) = 
   let cl = case organs of
	  [] -> [inst]
	  _ -> map (\organism -> inst ++ '_' ++ organismm) organs
   in cl ++ (comb_source sources) 

{- the GO files, when unzipped, contain a header like the following:
!CVS Version: Revision: 1.19 $
!GOC Validation Date: 01/27/2007 $
!Submission Date: 1/15/2007
-}

[pads|
  type Version_t =    	("!CVS Version: Revision: ", Pfloat, ws, '$')
  type Valid_date_t = 	("!GOC Validation Date: ", Pdate, ws, '$')
  type Sub_date_t =   	("!Submission Date: ", Pdate)
  type Project_name_t = ("!Project_name: ", Pstringln)
  type URL_t =		("!URL: ", Purl)
  type Email_t =	("!Contact Email: ", Pstringln)
  type Funding_t =	("!Funding: ", Pstringln)
  type Gaf_ver_t =	("!gaf-version: ", Pfloat)
  type Organism_t =  	("!organism:", ws, Pstringln)
  type Date_t =		("date:", ws, Pdate)
  type Note_t =	('!', ws, Pstringln)
 
  data Header_line_t = 
	Version Version_t
	| Valid_date Valid_date_t
	| Sub_date Sub_date_t
	| Project_name Project_name_t
	| URL URL_t
	| Email Email_t
	| Funding Funding_t
	| Gaf_ver Gaf_ver_t
	| Organism Organism_t
	| Date Date_t
	| Note_t
	| Other ('!', Pstringln)
  type Other_line_t = Pstringln
 
  type GA_f = ([Line Header_line_t], [Line Other_line_t] with term Eof)
|]

[pads|
  type Pair_t = {key::Pstring, '=', val::Pstring}
  type Conf_f = [Line Pair_t] with term Eof 
|]

{- isReadOnly md = get_modes md == "-rw-r--r--" -}

[forest|
  type Readme_d = Directory {
    readmes is [rm :: Maybe Text | rm <- <|map get_readme_file (comb_source sources)|>]
  }

  type PTHR_d (name :: String)  = Directory {
   attr is  <| name ++ ".save.attr" |>  :: Text,
   gaf  is  <| name ++ ".save.gaf" |>   :: Text,
   msa  is  <| name ++ ".save.msa" |>   :: Text,
   paint is <| name ++ ".save.paint" |> :: XML,
   sfan is  <| name ++ ".save.sfan" |>  :: Text,
   tree is  <| name ++ ".save.tree" |>  :: Text,
   txt  is  <| name ++ ".save.txt" |>   :: Text, 
   wts  is  <| name ++ ".save.txt" |>   :: Text
  }

  type Pre_sub_d = Directory {
    gz_files is   [gz   :: Maybe (Gzip (File GA_f)) | gz <- <|map get_gz_files (comb_source sources)|>],
    conf_files is [conf :: Maybe (File Conf_f) | conf <- <|map get_conf_files (comb_source sources)|>]
  }

  type Paint_d = Directory {
    pthr_dirs is [dir_name :: PTHR_d (dir_name) | dir_name <- matches RE "PTHR[0-9]+"],
    pre_sub   is "pre-submission" :: Pre_sub_d
  }
 
  type Submission_d = Directory {
    gz_files is   [gz   :: Maybe (Gzip (File GA_f)) | gz <- <|map get_gz_files (comb_source sources)|>],
    conf_files is [conf :: Maybe (File Conf_f) | conf <- <|map get_conf_files (comb_source sources)|>],
    paint_files is  [cs :: Maybe Conf_f | cs <- <|map (\x -> get_conf_files ("paint" ++ x)) (comb_source sources)|>], 
    paint_d     is  "paint"               :: Paint_d
  }

  type Top_d = Directory {
    data_files is [<|get_gz_file cs|> :: Maybe (Gzip (File GA_f)) | cs <- <|comb_source sources|>],
    readme     is "readme"             :: Readme_d,
    sub        is "submission"         :: Submission_d
  }
|]


