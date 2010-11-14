{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}


module Examples.PWSe where
import Language.Pads.Padsc
import Language.Forest.Forestc
--import System.IO.Unsafe (unsafePerformIO)

{- Description of configuration file for learning demo web site -}
[pads| 
  type Config_entry_t = Line ("\"",  Pstring '\"',  "\";")   
  data Config_f =  {
                      header      :: PstringSE(RE "$"),
   "$host_name   =",  host_name   :: Config_entry_t,  -- Name of machine hosting web site
   "$static_path =",  static_path :: Config_entry_t,  -- URL prefix for static content
   "$cgi_path    =",  cgi_path    :: Config_entry_t,  -- URL prefix for cgi content
   "$script_path =",  script_path :: Config_entry_t,  -- Path to directory of scripts in live web site
   "$tmp_root    =",  tmp_root    :: Config_entry_t,  -- Path to directory for demo user data
   "$pads_home   =",  pads_home   :: Config_entry_t,  -- Path to directory containing pads system
   "$learn_home  =",  learn_home  :: Config_entry_t,  -- Path to directory containing learning system
   "$sml_home    =",  sml_home    :: Config_entry_t,  -- Path to directory containing SML executable
   "$insrall_src =",  install_src :: Config_entry_t,  -- Path to directory containing learning demo website source
   "$static_dst  =",  static_dst  :: Config_entry_t,  -- Path to directory for static content in live web site
   "$cgi_dst     =",  cgi_dst     :: Config_entry_t,  -- Path to directory for cgi content in live web site site
                      trailer     :: Pstringln
   }
|]

[pads|
  {- Format for file listing data sources for web site -}
  type SourceNames_f = [Pstringln]

  {- Information related to a single user's use of the web site -}

  {- Each visitor gets assigned a userId that is passed as a ? parameter in URL.
     Security considerations preclude using user-modifiable values as part of file paths.
     Thus, we map each userId to a corresponding dirId.
     The dirId names the directory containing the associated user's data. 
     A userEntry_t contains a single such mapping.
     A file with type userEntries_t describes a collection of such mappings.
  -}
  data UserEntry_t =  {
     "id.",   userId :: Pint,               ',',
     "id.",   dirId  :: (Pint, '.', Pint)    where <| userId = first dirId |> 
  } 

  type UserEntries_f = [userEntry_t]
  type IP_t = (Pint, '.', Pint, '.', Pint, '.', Pint)

  {- Log of requests.  Used to prevent denial of service attacks. -}
  data LogEntry_t = {
    userId :: Pint,        ',',
    ip     :: IP_t,         ',',
    script :: Pstring ' ', ' ',
    userDir:: Pstring ' ', ' ',
    pads   :: Pstring ' ', ' ',
    sml    :: PstringSE(RE " "),     -- This may need to also enclude end of line as an end.
    msg    :: Maybe Pstringln 
  }

  type LogFile_f = [logEntry_t]
|]

{- Helper function to map a list of userIds, to the associated list of directory names -}
userDirs :: UserEntries_t -> [(Pint,Pint)]
userDirs f = List.map dirId f


{- Directory of image files -}
[forest|
  type Imgs_d = Directory {
    logo is "pads_small.jpg" :: Binary
  }

{- Directory of static content -}
  type Static_d = Directory {
    style_sheet is "pads.css"           :: Text,   
    intro_redir is "learning-demo.html" :: Text,   
    title_frame is "atitle.html"        :: Text,   
    logo_frame  is "top-left.html"      :: Text,   
    top_frame   is "banner.html"        :: Text,   
    empty_frame is "nothing.html"       :: Text,
    images      is "images"             :: Imgs_d 
  }

{- Directory of dynamic content -}
  type Cgi_d = Directory {
    config1       is "PLConfig.pm"             :: Config_f,
    config2       is "PLConfig.pm"             :: Text,     
    perl_utils    is "PLUtilities.pm"          :: Text,     
    intro         is "learning-demo.cgi"       :: Text,     
    intro_nav     is "navbar-orig.cgi"         :: Text,     
    select_data   is "pads.cgi"                :: Text,     
    result_nav    is "navbar.cgi"              :: Text,     
    format_chosen is "data-results.cgi"        :: Text,     
    gen_desc      is "build-description.cgi"   :: Text,     
    get_user_data is "build-roll-your-own.cgi" :: Text,     
    gen_desc_usr  is "genData.cgi"             :: Text,     
    build_lib     is "build-library.cgi"       :: Text,     
    build_accum   is "build-accum.cgi"         :: Text,     
    build_xml     is "build-xml.cgi"           :: Text,     
    build_fmt     is "build-fmt.cgi"           :: Text     
  }

{- Directory of shell scripts invoked by CGI to run learning system -}
 type Scripts_d = Directory {
    rlearn                    :: Text,      -- Shell script for running PADS comiler on stock format
    rlearnown is "rlearn-own" :: Text,      -- Shell script for running PADS compiler on user format
    raccum    is "r-accum"    :: Text,      -- Shell script to generate and run accumulator
    rxml      is "r-xml"      :: Text,      -- Shell script to generate and run XML converter
    rfmt      is "r-fmt"      :: Text      -- Shell script to generate and run formating program
  }


{- Directory containing administrative files used by demo web site -}
 type Info_d = Directory {
     sources is "sourceNames" :: SourceNames_f,  -- List of source data files whose formats can be learned
     users   is "usersFile"   :: UserEntries_f,  -- Mapping from userIDs to associated directory names
     log     is "logFile"     :: LogFile_f       -- Log of server actions.
  }                          

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
  static_content  is <| static_path c |> :: Static_d,        -- static web site content
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

          
