{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables #-}

module Examples.PWS where
import Language.Pads.Padsc
import Language.Forest.Forestc
import System.IO.Unsafe (unsafePerformIO)
import Language.Pads.GenPretty
import Language.Forest.Graph

{- Description of configuration file for learning demo web site -}
[pads| 
  type Config_entry_t = Line (" \"",  Pstring '\"',  "\";")   
  type Header_t = [Pstringln] with term length of 13
  data Config_f =  {
                      header      :: [Pstringln] with term length of 13,
   "$host_name   =",  host_name   :: Config_entry_t,  -- Name of machine hosting web site
   "$static_path =",  static_path :: Config_entry_t,  -- URL prefix for static content
   "$cgi_path    =",  cgi_path    :: Config_entry_t,  -- URL prefix for cgi content
   "$script_path =",  script_path :: Config_entry_t,  -- Path to directory of scripts in live web site
   "$tmp_root    =",  tmp_root    :: Config_entry_t,  -- Path to directory for demo user data
   "$pads_home   =",  pads_home   :: Config_entry_t,  -- Path to directory containing pads system
   "$learn_home  =",  learn_home  :: Config_entry_t,  -- Path to directory containing learning system
   "$sml_home    =",  sml_home    :: Config_entry_t,  -- Path to directory containing SML executable
   "$install_src =",  install_src :: Config_entry_t,  -- Path to directory containing learning demo website source
   "$static_dst  =",  static_dst  :: Config_entry_t,  -- Path to directory for static content in live web site
   "$cgi_dst     =",  cgi_dst     :: Config_entry_t,  -- Path to directory for cgi content in live web site site
                      trailer     :: [Pstringln]
   }


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
     "id.",   usrId :: Pint,               
    ",id.",   dirId :: (Pint, '.', Pint)    where <| usrId == fst dirId |> 
  } 

  type UserEntries_f = [Line UserEntry_t] with term Eor

  type IP_t = (Pint, '.', Pint, '.', Pint, '.', Pint)

  {- Log of requests.  Used to prevent denial of service attacks. -}
  data LogEntry_t = {
    userId :: Pint,        ',',
    ip     :: IP_t,        ',',
    script :: Pstring ' ', ' ',
    userDir:: Pstring ' ', ' ',
    padsv  :: Pstring ' ', ' ',
    sml    :: PstringSE(RE " "),     -- This may need to also enclude end of line as an end.
    msg    :: Maybe Pstringln 
  }

  type LogFile_f = [LogEntry_t]
|]



{- Helper function to map a list of userIds, to the associated list of directory names -}
userNames info = getUserEntries (users info)
getUserEntries (UserEntries (UserEntries_f users)) = map userEntryToFileName users
userEntryToFileName userEntry = pairToFileName (dirId userEntry)
pairToFileName (Pint n1, Pint n2) = "id."++(show n1)++"."++(show n2)

userDirs :: [UserEntry_t] -> [(Pint,Pint)]
userDirs f = map dirId f

isReadOnly md = get_modes md == "-rw-r--r--"

cToS (Config_entry_t (Pstring s)) = s
ghost_name   (Config c) = cToS $ host_name c
gstatic_path (Config c) = cToS $ static_path c
gcgi_path    (Config c) = cToS $ cgi_path c
gscript_path (Config c) = cToS $ script_path c
glearn_home  (Config c) = cToS $ learn_home c
gtmp_root    (Config c) = cToS $ tmp_root c
gstatic_dst  (Config c) = cToS $ static_dst c


[forest|
  type BinaryRO    = Binary        where <| get_modes this_att ==  "-rw-r--r--" |>
  type BinaryRX    = Binary        where <| get_modes this_att ==  "-rwxr-xr-x" |>
  type TextRX      = Text          where <| get_modes this_att ==  "-rwxr-xr-x" |>
  type TextRO      = Text          where <| get_modes this_att ==  "-rw-r--r--" |>
  type Config      = File Config_f where <| get_modes this_att ==  "-rw-r--r--" |>
  type SourceNames = File SourceNames_f where <| isReadOnly this_att |>
  type UserEntries = File UserEntries_f where <| isReadOnly this_att |>
  type LogFile     = File LogFile_f     where <| isReadOnly this_att |>

  {- Directory of image files -}
  type Imgs_d = Directory {
    logo     is "pads_small.jpg" :: BinaryRO,
    favicon  is "favicon.ico"    :: BinaryRO
  }

  {- Directory of static content -}
  type Static_d = Directory {
    style_sheet is "pads.css"           :: TextRO,   
    intro_redir is "learning-demo.html" :: TextRO,   
    title_frame is "atitle.html"        :: TextRO,   
    logo_frame  is "top-left.html"      :: TextRO,   
    top_frame   is "banner.html"        :: TextRO,   
    empty_frame is "nothing.html"       :: TextRO,
    images      is "images"             :: Imgs_d where <| get_modes images_md == "drwxr-xr-x" |>
  }

{- Directory of dynamic content -}
  type Cgi_d = Directory {
    config1       is "PLConfig.pm"             :: Config,
    config2       is "PLConfig.pm"             :: TextRO,     
    perl_utils    is "PLUtilities.pm"          :: TextRO,     
    intro         is "learning-demo.cgi"       :: TextRX,     
    intro_nav     is "navbar-orig.cgi"         :: TextRX,     
    select_data   is "pads.cgi"                :: TextRX,     
    result_nav    is "navbar.cgi"              :: TextRX,     
    format_chosen is "data-results.cgi"        :: TextRX,     
    gen_desc      is "build-description.cgi"   :: TextRX,     
    get_user_data is "build-roll-your-own.cgi" :: TextRX,     
    gen_desc_usr  is "genData.cgi"             :: TextRX,     
    build_lib     is "build-library.cgi"       :: TextRX,     
    build_accum   is "build-accum.cgi"         :: TextRX,     
    build_xml     is "build-xml.cgi"           :: TextRX,     
    build_fmt     is "build-fmt.cgi"           :: TextRX     
  }

{- Directory of shell scripts invoked by CGI to run learning system -}
 type Scripts_d = Directory {
    rlearn                    :: TextRX,      -- Shell script for running PADS comiler on stock format
    rlearnown is "rlearn-own" :: TextRX,      -- Shell script for running PADS compiler on user format
    raccum    is "r-accum"    :: TextRX,      -- Shell script to generate and run accumulator
    rxml      is "r-xml"      :: TextRX,      -- Shell script to generate and run XML converter
    rfmt      is "r-fmt"      :: TextRX       -- Shell script to generate and run formating program
  }

{- Directory containing administrative files used by demo web site -}
 type Info_d = Directory {
     sources is "sampleFiles" :: SourceNames,  -- List of source data files whose formats can be learned
     users   is "userFile"    :: UserEntries,  -- Mapping from userIDs to associated directory names
     logFile is "logFile"     :: LogFile       -- Log of server actions.
  }                          

{- Type of directory containing actual data files named by sourceNames -}
 type DataSource_d(sources :: [String]) =  [ s :: Text | s <- sources ]

{- Type of a link with get_modes rwxrwxr-x to location p with type dataFile_t -}
 type SymLink_f (source :: String) = SymLink where <| this == source |>

{- Directory of optional links to source data files -}
 type Data_d ((root,sourceNames) :: (String, [String])) = Directory {
     datareps  is [s :: Maybe Text                            | s <- sourceNames],
     datalinks is [s :: Maybe (SymLink_f <| root++"/"++ s |>) | s <- sourceNames]        
 }

 type OptBinaryRX = Maybe BinaryRX

{- Directory that stores the generated machine-dependent output for data source named source -}
 type MachineDep_d (source :: String) = Directory {
   pads_c    is <| source ++ ".c"    |> :: TextRO,
   pads_h    is <| source ++ ".h"    |> :: TextRO,
   pads_o    is <| source ++ ".o"    |> :: BinaryRO,
   pads_pxml is <| source ++ ".pxml" |> :: TextRO,  -- PADS description in xml syntax
   pads_xsd  is <| source ++ ".xsd"  |> :: TextRO,  -- xschema of xml syntax for <| source description
   pads_acc  is <| source ++ "-accum"|> :: OptBinaryRX,
   pads_fmt  is <| source ++ "-fmt"  |> :: OptBinaryRX,
   pads_xml  is <| source ++ "-xml"  |> :: OptBinaryRX
}


{- Directory that stores the generated output for data source named "source". -}
 type Example_d (source :: String) = Directory {
   pads_p         is <| source ++ ".p" |>            :: TextRO,    -- padsc description of data source
   pads_pml       is <| source ++ ".pml" |>          :: Maybe TextRO,    -- padsml description of data source
   vanilla        is "vanilla.p"                     :: TextRO,    -- input tokenization
   makefile       is "GNUmakefile"                   :: Text,    
   machine        is <| envVar "AST_ARCH"|>          :: Maybe (MachineDep_d source),   -- Platform dependent files
   accum_c        is <| source ++ "-accum.c" |>      :: Maybe TextRO,    -- template for generating accumulator output
   accum_out      is <| source ++ "-accum.out"|>     :: Maybe TextRO,   -- ASCII Accumulator output
   accum_xml_out  is <| source ++ "-accum_xml.out"|> :: Maybe TextRO,   -- XML Accumulator output
   xml_c          is <| source ++ "-xml.c"|>         :: Maybe TextRO,   -- template for generating xml output
   xml_out        is <| source ++ "-xml.out"|>       :: Maybe TextRO,   -- XML representation of <| source
   xml_xsd        is <| source ++ ".xsd" |>          :: Maybe TextRO,   -- XSchema for XML representation of source
   fmt_c          is <| source ++ "-fmt.c" |>        :: Maybe TextRO,   -- template for generating fmt output
   fmt_out        is <| source ++ "-fmt.out" |>      :: Maybe TextRO    -- Formatted representation of source
 }

{- Directory that stores all information for one user. -}
 type User_d(arg@ (r, sources) :: (String, [String])) = Directory {
    dataSets    is "data"    :: Maybe (Data_d arg),
    runExamples is       [ s :: Maybe (Example_d s) | s <- sources]
  }

{- Directory stores temporary information associated with all users. -}
 type Users_d((r,info) :: (String, Info_d)) = 
    [userDir :: User_d <|(r, getSources info) |>  | userDir <- <| userNames info |> ]

 type Website_d(config::FilePath)  = Directory {
  c               is config               :: Config,             -- configuration file with locations
  static_content  is <| gstatic_dst c  |> :: Static_d,           -- static web site content
  dynamic_content is <| gcgi_path c    |> :: Cgi_d,              -- dynamic web site content
  scripts         is <| gscript_path c |> :: Scripts_d,          -- shell scripts invoked by cgi to run learning system
  admin_info      is <| gstatic_dst c  |> :: Info_d,             -- administrative information about website
  data_dir        is <| (glearn_home c)++"/examples/data" |>
                                                  :: DataSource_d <|(getSources admin_info)|>,      -- stock data files for website
  usr_data        is <| gtmp_root c |>     :: Users_d <|(get_fullpath data_dir_md, admin_info)|>      -- per user information
 }
 
|]

config_location = "/Users/kfisher/Sites/cgi-bin/PLConfig.PM"
doLoadWebsite = website_d_load config_location "/Users/kfisher/Sites"

{- print graph of website -}
doGraph md =  mdToPDF md "Examples/website.pdf"
          
users_dir = "/Users/kfisher/Sites/cgi-bin/gen"
(users'_rep, users'_md) :: (Users_d, Users_d_md) = unsafePerformIO $ load1 ("/Users/kfisher/pads/infer/examples/data", info_rep) users_dir

user_dir = "/Users/kfisher/Sites/cgi-bin/gen/id.1192115633.7"
(userE_rep, userE_md) :: (User_d, User_d_md) = unsafePerformIO $ load1 ("/Users/kfisher/pads/infer/examples/data", ["ai.3000"]) user_dir

graphUserIO = mdToPDF userE_md "Examples/users.pdf"

example_dir = "/Users/kfisher/Sites/cgi-bin/gen/id.1192115633.7/ai.3000"
(example_rep, example_md) :: (Example_d, Example_d_md) = unsafePerformIO $ load1 "ai.3000" example_dir

machine_dir = "/Users/kfisher/Sites/cgi-bin/gen/id.1192115633.7/ai.3000/darwin.i386"
(machinedep_rep, machinedep_md) :: (MachineDep_d, MachineDep_d_md) = unsafePerformIO $ load1 "ai.3000" machine_dir

root_data_dir = "/Users/kfisher/pads/infer/examples/data"
data_dir_path = "/Users/kfisher/Sites/cgi-bin/gen/id.1192115633.7/data"
(data_d_rep, data_d_md) :: (Data_d, Data_d_md) = unsafePerformIO $ load1 (root_data_dir, datasources) data_dir_path

link_path = "Examples/data/Simple/mylink"
(link_rep,link_md) :: (SymLink_f, SymLink_f_md) = unsafePerformIO $ load1 "quantum" link_path


info_dir = "/Users/kfisher/Sites"
(info_rep, info_md) :: (Info_d, Info_d_md) = unsafePerformIO $ load  info_dir

dataSource_dir = "/Users/kfisher/pads/infer/examples/data"
(datasource_rep, datasource_md) :: (DataSource_d, DataSource_d_md) = unsafePerformIO $ load1 datasources dataSource_dir

getStrings (Pstringln (PstringSE s)) = s
getSources' (SourceNames (SourceNames_f  pstrlns)) = map getStrings pstrlns

getSources :: Info_d -> [String]
getSources info = getSources' (sources info)

datasources :: [String]
datasources = getSources info_rep


scripts_dir = "/Users/kfisher/Sites/cgi-bin"
(scripts_rep, scripts'_md) :: (Scripts_d, Scripts_d_md) = unsafePerformIO $ load  scripts_dir

cgi_dir = "/Users/kfisher/Sites/cgi-bin"
(cgi_rep, cgi_md) :: (Cgi_d, Cgi_d_md) = unsafePerformIO $ load  cgi_dir

static_dir = "/Users/kfisher/Sites"
(static_rep, static_md) :: (Static_d, Static_d_md) = unsafePerformIO $ load  static_dir

image_dir = "/Users/kfisher/Sites/images"
(img_rep, img_md) :: (Imgs_d, Imgs_d_md) = unsafePerformIO $ load  image_dir

config_file = "/Users/kfisher/Sites/cgi-bin/PLConfig.pm"
(config_rep, config_md) :: (Config_f, Config_f_md) = unsafePerformIO $ parseFile config_file
(head_rep, head_md) :: (Header_t, Header_t_md) = unsafePerformIO $ parseFile config_file

sampleFiles = "/Users/kfisher/Sites/sampleFiles"
(sample_rep, sample_md) :: (SourceNames_f, SourceNames_f_md) = unsafePerformIO $ parseFile sampleFiles

userEntries = "/Users/kfisher/Sites/userFile"
(user_rep, user_md) :: (UserEntries_f, UserEntries_f_md) = unsafePerformIO $ parseFile userEntries

logFiles = "/Users/kfisher/Sites/logFile"
(logFiles_rep, logFiles_md) :: (LogFile_f, LogFile_f_md) = unsafePerformIO $ parseFile logFiles
