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

ws = RE "/\s+/"
gz_suffix = ".gz"
readme_suffix = ".README"
conf_suffix = ".conf"

title = "gene_association"
origins = [
	"Compugen"
	, "GeneDB"
	, "PAMGO"
	, "aspgd"
	, "cgd"
	, "dictyBase"
	, "ecocyc"
	, "fb"
	, "goa"
	, "gramene"
	, "jcvi"
	, "mgi"
	, "pseudocap"
	, "reactome"
	, "rgd"
	, "sgd"
	, "sgn"
	, "tair"
	, "wb"
	, "zfin"
]

organisms = [
	 "Lmajor"
	,"Pfalciparum"
	,"Spombe"
	,"Tbrucei"
	,"tsetse"
	,"Atumefaciens"
	,"Ddadantii"
	,"Mgrisea"
	,"Oomycetes"
	,"arabidopsis"
	,"chicken"
	,"cow"
	,"human"
	,"mouse"
	,"pdb"
	,"rat"
	,"uniprot"
	,"noiea"
	,"zebrafish"
	,"oryza"
	,"Aphagocytophilum"
	,"Banthracis"
	,"Cburnetii"
	,"Chydrogenoformans"
	,"Cjejuni"
	,"Cperfringens"
	,"Cpsychrerythraea"
	,"Dethenogenes"
	,"Echaffeensis"
	,"Gsulfurreducens"
	,"Hneptunium"
	,"Lmonocytogenes"
	,"Mcapsulatus"
	,"Nsennetsu"
	,"Pfluorescens"
	,"Psyringae"
	,"phaseolicola"
	,"Soneidensis"
	,"Spomeroyi"
	,"Vcholerae"
]

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
  type Submission_d = Directory {
    pair_files = [Maybe (zipf :: Gzip (File GA_f), conf :: Conf_f) | 

  }
|]


