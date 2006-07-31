#define TD "/\\<td\\>|\\<td width=\"\\d*%\"\\>/"
#define WS "/\\s*/"

Precord Ptypedef Pstring_SE(:Peor:) Pstringln;

Parray myheader_t(:char * stop:) {
  Pstringln[] : Pterm(Pre stop);
}

Pstruct tagS_t(:char *s:){
  '<'; s; '>';
  Pstring(:'<':) entry;
  '<'; '/'; s; '>';
}

Precord Ptypedef tagS_t(:s:) tagSln_t(:char *s:);

Pstruct age_t {
  " (The age of ";
  Puint8 age;
  ")";
}

Pstruct tagAge_t(:char *s:){
  '<'; s; '>';
  Puint32 i;
  Popt age_t a;
  '<'; '/'; s; '>';
};

Precord Ptypedef tagAge_t(:s:) tagAgeln_t(:char *s:);

Ptypedef Pchar PnotChar(: char bad :) : PnotChar c => {c != bad};
Ptypedef Pchar PisChar(: char good :) : PnotChar c => {c == good};

Pstruct sessionNumber_t {
  Puint32 num;
  Popt PnotChar(:'\"':) suff;
};

Precord Pstruct sessionHeader_t{
  "<h3><a name=\"";
  sessionNumber_t number;
  "\">";
  Pstring(:'<':) name;
  "</a></h3>";
};

Pstruct artist_t{
  Pstring_SE(:"/ \\(/":) artist;
  " (";
  Pstring(:')':) role;  /-- should this be an enumerated type?
  ')';
  Pre "/[.]?/";
};

Punion artistDesc_t{
  artist_t person;
  Pstring_SE(:Peor:) comment;
};

Precord Parray artists_t{
  artistDesc_t[] : Psep(' ') && Pterm(Peor);
};

// This type should be refined to separate the location
// and the date.  We need to extend pads with the ability
// to terminate arrays by arbitrary PADS types to make 
// this possible. 
Precord Pstruct sessionDate_t {
  "<div class=\"date\">";
  Pstring(:'<':) date;
  "</div>";
};

Pstruct itemNum_t{
  Puint32 num;
  '.';
  Pre WS;
};

Pstruct songNumPre_t{
  Pstring_ME(:"/[[:alpha:]]+/":) prefix;
};

Pstruct songNumSuf_t{
  "-";
  Puint32 suf;
};

Pstruct altSongNum_t{
  " | ";
  Puint32 secondNum;
};

Pstruct songNum_t{
  Popt songNumPre_t pre;
  Puint32           songNum;
  Popt songNumSuf_t suf;
  Popt altSongNum_t alt;
};

Pstruct take_t{
  "tk.";
  Puint8 take;
};

Pstruct VKNum_t{
  Puint16 pre;
  "VK";
  Puint16 post;
};

Pstruct RFNum_t{
  "RF-";
  Puint16 num;
};

Punion songNumEntry_t{
  VKNum_t         vkNum;
  RFNum_t         rfNum;
  songNum_t       num;
  PisChar(:'-':)  ditto;
  take_t          takeNum;
  Pcompute Puint8 missing = 1;
};

Penum whichTag {
  OJCCD,
  CDPS Pfrom ("CDP 7"),
  SKAO Pfrom ("SKA O"),   
  MQ Pfrom("MQ9"), 
  KCT Pfrom ("KC2"),
  OJCD, EBCD, VJLP, FHCY, VNCD, MRCD, PACD, Live, AROC, SABB, SVBB, SWBB, SWBO, TOCJ, POCJ,
  YD Pfrom("32YD"), 
  EPJ Pfrom("18PJ"), TPJ Pfrom("25PJ"), 
  OHNFJ Pfrom("195J"), TAP Pfrom ("20AP"),
  VVS Pfrom("V/V6"), 
  BSTT Pfrom("BST2"),
  BNLA Pfrom("BN-LA"),
  CFX Pfrom("C5X"),
  CSK  Pfrom ("C6K"),
  CFK  Pfrom ("C4K"),
  CDP, DEM, EMS, HCY, JWC, SJL, BLP, FCD, OJC, RLP, MCD, MGW, JLP, SMJ, BNJ,
  LLP, LCD, VIJ, CAL, UPS, CK, SXL,
  CJ, CL, CP, CS, DR, EB, EP, GW, PG, WP, PA, RS, SR, ST, MG, VK,
  CT Pfrom ("C2"), QN Pfrom ("Q9"),
  C, F, G, M, O, P, R, T, W 
};

Penum prod_t{
  ColumbiaLegacy Pfrom("Columbia/Legacy"),
  EmArcy, Savoy, Mosaic, Mercury, Riverside, Columbia, Fantasy, Verve, Wing, Alto,
  Milestone, Capitol, Jazzland, Landmark, Calliope, Atlantic, Session, Baybridge,
  Dobre, FDC,Joker, Magnetic, Motown, Pablo,Prestige, Roulette, Universal, Limelight,
  VirginNight Pfrom ("Virgin Night"),
  JazzBand Pfrom ("Jazz Band"),
  CBSSony Pfrom("CBS/Sony"),
  WorldPacific Pfrom ("World Pacific"),
  BlueNote Pfrom ("Blue Note"),
  PSJZ Pfrom("PSJ Z"),
  PaloAlto Pfrom("Palo Alto Jazz"),
  PacificJazz Pfrom("Pacific Jazz"),
  UlysseMusique Pfrom("Ulysse Musique"),
  VJ Pfrom("Vee-Jay") 
};

Pstruct J_t{
  '(';
  Pstring(:')':) j;
  ") ";
};

Pstruct numPair_t{
  Puint32 f;
  Pre "/x|-|\\//";
  Pstring_SE(:Pre "/[),;]|$/":) suffix;
};

Punion number_t(:prod_t prod:) {
  numPair_t pair;
  Puint32   single : prod != Verve;
  "rejected";
  Pstring_SE(:Pre "/[),;]|$/":) otherNum;
}

Pstruct albumNum_t(:prod_t prod:){
  Popt J_t j;
  Popt whichTag which;
  Pre "/ ?/";
  number_t(:prod:) num;
};

Pstruct album_t{
  prod_t prod;
  ' ';
  albumNum_t(:prod:)[] indxs : Psep(", ") && Pterm(Pre "/[);]|$/");
};

Punion albumInfo_t{
  PisChar(:'-':) sameAlbum;
  album_t        isKnown;
  "unissued";
  Pstring_SE(:Peor:) rest;
};


Precord Pstruct song_t {
  "<tr>";
  Pre TD;
  Popt itemNum_t    itm;
  songNumEntry_t songNumber;
  Pre TD;
  Pstring_SE(:"/\\<td/":) title;
  Pre TD;
  albumInfo_t[] albums : Psep("; ") && Pterm(Peor);
};

// Need some way to put end-of-record consume markers into literals
Pstruct songs_t{
  Pstringln twidth; /-- <table width="100%">
  song_t[] song_list : Pterm(Pre "/\\<\\/tab/");
  Pomit Pstringln etwidth; /-- </table>
};

Pstruct ArtAndTitle_t{
  Pstring_SE(:"/ \\- /":) artists;
  " - ";
  Pstring(:'<':) title;
}

Punion v_t {
  ArtAndTitle_t artAndTitle;
  Pstring(:'<':) title;
};

Precord Pstruct venue_t{
  Pre "/[*=]/";
  " <i>";
  v_t v;
  "</i> ";
  '(';
  album_t[] albs : Psep("; ") && Pterm(')');
  ')';
  Pre "/(\\<br\\>)?/";
};

// Need some way to put end-of-record consume markers into literals
// Then separator can be <br>EOR

Pstruct session_t{
  sessionHeader_t sessionHeader;
  artists_t       artists;
  sessionDate_t   date;
  songs_t         songsTable;
  venue_t []      venues : Pterm(Pre "/\\<h/");
};


Pstruct jazzHeader_t{
  myheader_t(:"/\\<h1\\>/":) h;
  tagSln_t(:"h1":) title;
  Pstringln header;
};

Pstruct year_t{
  tagAgeln_t(:"h2":) year;
  session_t[] sessions : Pterm(Pre "/\\<h2\\>|\\<h4/");
};

Pstruct jazzTrailer_t{
  Pstringln[] r;
};

Psource Pstruct jazz_t{
  jazzHeader_t h;
  year_t[] data : Pterm ("<h4");
  jazzTrailer_t r;
};

