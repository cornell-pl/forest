{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.LearnPADS where
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
[pads|
  type Comment_t = ('#', Pstringln)
  type Def_t = ("def", ws, PstringSE ws, ws, Pstringln)
  type Exp_t = ("exp", ws, PstringSE ws, ws, Pstringln)
  data Line_t = Def Def_t | Exp Exp_t
  type Config_f = [Line Line_t]
|]

[pads|
  type Header = (RE "/\(\*+/", Eor, RE "/\*\s+\*/", Eor,
  "*              This software is part of the infer package              *", Eor)
  type Lines_t = [PstringLn]
  type Src_t = (Head, Lines_t)
|]

[pads|
  type Command = P "#!/usr/bin/perl" | E "#!/usr/bin/env perl"
  type Lines_t = [PstringLn]
  type Perl_t = (Command, Lines_t)
|]

isReadOnly md = get_modes md == "-rw-r--r--"

benchmarks = [
  "1967transactions.short",    
  "mer_t01_01.csv",            
  "ai.3000",
  "asl.log",                   
  "boot.log",                  
  "convert.log",               
  "crashreporter.log",         
  "crashreporter.log.modified",
  "dibbler.1000",              
  "ls-l.txt",                  
  "netstat-an",                
  "page_log",                  
  "quarterlypersonalincome",   
  "railroad.txt",              
  "rpmpkgs.txt",               
  "scrollkeeper.log",          
  "windowserver_last.log",     
  "yum.txt" 
]

let benchmarks_alt = [
  "1967transactions",    
  "mer_t01_01.csv",            
  "ai",
  "asl",                   
  "boot",                  
  "convert",               
  "crashreporter",         
  "dibbler",              
  "ls-l",                  
  "netstat-an",                
  "page_log",                  
  "quarterincome",   
  "railroad",              
  "rpmpkgs",               
  "scrollkeeper",          
  "windowserver_last.log",     
  "yum" 
]

[forest|
  type TextRX      = Text          where <| get_modes this_att ==  "-rwxr-xr-x" |>
  type TextRO      = Text          where <| get_modes this_att ==  "-rw-r--r--" |>
  type PADSRO      = File Pads_t   where <| get_modes this_att ==  "-rw-r--r--" |>
  type SRC	   = File Src_t    where <| get_modes this_att ==  "-rw-r--r--" |>
  type Perl 	   = File Perl_t   where <| get_modes this_att ==  "-rwxr-xr-x" |>
  type Config      = File Config_f where <| get_modes this_att ==  "-rw-r--r--" |>

  {- Directory of data files -}
  type Data_d = Directory {
    dataFiles is [ f :: TextRO | f <- benchmarks ]
    {- And some other files -}
  }

  {- Directory of gold data files -}
  type Gold_data_d = Directory {
    dataFiles is [ f :: TextRO | f <- benchmarks ]
  }
 
  {- Gold IR description as sml programs -}
  type Gold_ir_d = Directory {
    irs is [ d ++ ".sml" :: SRC | d <- benchmarks ]
  }
 
  {- Gold PADS description -}
  type Gold_pads_d = Directory {
    descs is [ d ++ ".p" :: PADSRO | d <-benchmarks_alt ]
  }

  {- examples/gold directory -}
  type Gold_d = Directory {
    makefile is "GNUmakefile" 	:: TextRO,
    data_dir is "data" 		:: Gold_data_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    ir_dir   is "ir"		:: Gold_ir_d   where <| get_modes ir_dir_md == "drwxr-xr-x" |>,
    p_dir    is "p"		:: Gold_pads_d where <| get_modes p_dir_md == "drwxr-xr-x" |>
  }

  {- examples/results directory - contains a single makefile -}
  type Results_d = Directory {
    makefile is "GNUmakefile" 	:: TextRO
  }

  {- examples directory -}
  type Examples_d = Directory {
    readme      is "README" 		:: TextRO,
    makefile    is "GNUmakefile" 	:: TextRO,
    data_dir    is "data"		:: Data_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    gold_dir    is "gold"		:: Gold_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    results_dir is "results"		:: Results_d where <| get_modes data_dir_md == "drwxr-xr-x" |>
  }

 {- include directory -}
  type Include_d = Directory {
    readme      is "README" 		:: TextRO,
    makefile    is "GNUmakefile.output" :: TextRO,
    basetokens  is "basetoken.p"	:: TextRO, 
    tokens      is "tokens.config"	:: Config, 
    vanilla     is "vanilla.config"	:: Config
  }

  {- scripts directory -}
  type Scripts_d = Directory {
    config  is "config.pl" 	  :: Perl,
    grapher is "grapher.template" :: TextRX,
    learn   is "learn"		  :: Perl
  }
  
  {- the source directory contains a main.cm and a list of sml files -}
  type Src_d = Directory {
    main is "main.cm" :: SRC,
    sources is [ s :: SRC | s <- matches (GL  "*.sml") ]
  }
  
  {- the root infer directory -}
  type infer_d = Directory {
    readme      is "README" 		:: TextRO,
    install	is "INSTALL_NOTES" 	:: TextRO,
    makefile    is "GNUmakefile.output" :: TextRO,
    txt_license  is "cpl-1.0-license.txt" :: TextRO,
    html_license is "cpl-1.0-license.html" :: TextRO,
    examples_dir is "data"		:: Data_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    include_dir is "include"		:: Include_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    scripts_dir is "scripts"		:: Scripts_d where <| get_modes data_dir_md == "drwxr-xr-x" |>,
    src_dir     is "src"		:: Src_d where <| get_modes data_dir_md == "drwxr-xr-x" |>
|]

{-
info_dir = "/Users/kfisher/Sites"
(info_rep, info_md) :: (Info_d, Info_d_md) = unsafePerformIO $ load  info_dir

scripts_dir = "/Users/kfisher/Sites/cgi-bin"
(scripts_rep, scripts_md) :: (Scripts_d, Scripts_d_md) = unsafePerformIO $ load  scripts_dir

cgi_dir = "/Users/kfisher/Sites/cgi-bin"
(cgi_rep, cgi_md) :: (Cgi_d, Cgi_d_md) = unsafePerformIO $ load  cgi_dir

static_dir = "/Users/kfisher/Sites"
(static_rep, static_md) :: (Static_d, Static_d_md) = unsafePerformIO $ load  static_dir

image_dir = "/Users/kfisher/Sites/images"
(img_rep, img_md) :: (Imgs_d, Imgs_d_md) = unsafePerformIO $ load  image_dir



{- Type of directory containing stock data files named by sourceNames -}
 type DataSource_d(sourceNames :: SourceNames_t) = Directory {
      dataSources is [ s :: Text | s <- sourceNames];
  }

{- Type of a link with get_modes rwxrwxr-x to location p with type dataFile_t -}
 type SymLink_f (sourceName :: String) = SymLink where <| this == sourceName |>

{- Directory of optional links to source data files -}
 type Data_d ((sourceNames,d) :: (SourceNames_f, DataSource_d)) = Directory {
     datareps  is [s :: Maybe Text          | s <- sourceNames]
     datalinks is [s :: Maybe (Symlink_f s) | s <- sourceNames]        -- this path needs to be augmented from d parameter's location
 }

{- Directory that stores the generated machine-dependent output for data source named source -}
 type MachineDep_d (source :: String) = Directory {
   pads_c    is <| source ++ ".c"    |> :: Text,
   pads_h    is <| source ++ ".h"    |> :: Text,
   pads_o    is <| source ++ ".o"    |> :: Binary,
   pads_pxml is <| source ++ ".pxml" |> :: Text,  -- PADS description in xml syntax
   pads_xsd  is <| source ++ ".xsd"  |> :: Text,  -- xschema of xml syntax for <| source description
   pads_acc  is <| source ++ "-accum"|> :: Maybe Binary,
   pads_fmt  is <| source ++ "-fmt"  |> :: Mabye Binary,
   pads_xml  is <| source ++ "-xml"  |> :: Maybe Binary
}

{- Directory that store the generated output for data source named source. -}
 type Example_d (source :: String) = Directory {
   pads_p         is <| source ++ ".p" |>            :: Text,    -- padsc description of data source
   pads_pml       is <| source ++ ".pml" |>          :: Text,    -- padsml description of data source
   vanilla        is "vanilla.p"                     :: Text,    -- input tokenization
   makefile       is "GNUmakefile"                   :: Text,    
   machine        is <| getEnv "AST_ARCH"|>  :: Maybe (MachineDep_d source),   -- Platform dependent files
   accum_c        is <| source ++ "-accum.c" |>      :: Maybe Text,    -- template for generating accumulator output
   accum_out      is <| source ++ "-accum.out"|>     :: Maybe Text,   -- ASCII Accumulator output
   accum_xml_out  is <| source ++ "-accum_xml.out"|> :: Maybe Text,   -- XML Accumulator output
   xml_c          is <| source ++ "-xml.c"|>         :: Maybe Text,   -- template for generating xml output
   xml_out        is <| source ++ "-xml.out"|>       :: Maybe Text,   -- XML representation of <| source
   xml_xsd        is <| source ++ ".xsd" |>          :: Maybe Text,   -- XSchema for XML representation of source
   fmt_c          is <| source ++ "-fmt.c" |>        :: Maybe Text,   -- template for generating fmt output
   fmt_out        is <| source ++ "-fmt.out" |>      :: Maybe Text    -- Formatted representation of source
};

{- Directory that stores all information for one user. -}
{- This type is an example where it isn't easy to factor type of element out into containing list -}
 type User_d( arg @ (sources, dataSourcE) :: (SourceNames_f, DataSource_d)) = Directory {
    data        :: Maybe (Data_d arg),
    runExamples is [ s  :: Maybe (Example_d s) | s <- sources]
  }


{- Directory stores temporary information associated with all users. -}
 type Users_d((info, dataSource) :: (Info_d, DataSource_d)) = 
    [userDir :: Maybe (User_d <|(sources info, dataSource) |> ) | userDir <- userDirs(users info) ]



 type Website_d(config::FilePath)  = Directory {
  c               is config              :: Config_f,        -- configuration file with locations
  static_content  is <| static_dst c  |> :: Static_d,        -- static web site content
  dynamic_content is <| cgi_path c    |> :: Cgi_d,           -- dynamic web site content
  scripts         is <| script_path c |> :: Scripts_d,       -- shell scripts invoked by cgi to run learning system
  admin_info      is <| static_dst c  |> :: Info_d,         -- administrative information about website
  data_dir        is <| (learn_home c)++"/examples/data" |>
                                         :: DataSource_d(sources admin_info),      -- stock data files for website
  usr_data        is <| tmp_root c |>    :: Users_d(admin_info, data_dir)      -- per user information
 } 
|]

let config_location = "Root/cgi-bin/PLConfig.PM"
doLoadWebsite = do
 { (rep,md) <- website_d_load config_location config ""
 }

          
-}
