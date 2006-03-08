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
}


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

Pstruct MG_t{
  "MG ";
  Puint32 num;
};

Pstruct numPair_t{
  Puint32 f;
  '-';
  Puint32 s;
};

Penum whichTag {
  EP, EMS
};

Pstruct concise_t{
  whichTag which;
  ' ';
  numPair_t num;
};


Punion EmArcyNum_t{
  MG_t mg;
  concise_t ep;
}

Pstruct EmArcy_t{
  "EmArcy ";
  //Pstring_SE(:Peor:) r;
  EmArcyNum_t[] indxs : Psep(", ") && Pterm(';');
};

Punion album_t{
  PisChar(:'-':) sameAlbum;
  EmArcy_t       isEmArcy;
  Pstring_SE(:Peor:) rest;
};

Precord Pstruct song_t {
  "<tr>";
  Pre TD;
  Popt itemNum_t    i;
  songNumEntry_t songNumber;
  Pre TD;
  Pstring_SE(:"/\\<td/":) title;
  Pre TD;
  album_t album;
};

// Need some way to put end-of-record consume markers into literals
Pstruct songs_t{
  Pstringln twidth; /-- <table width="100%">
  song_t[] song_list : Pterm(Pre "/\\<\\/tab/");
  Pomit Pstringln etwidth; /-- </table>
};


Pstruct session_t{
  sessionHeader_t sessionHeader;
  artists_t       artists;
  sessionDate_t   date;
  songs_t         songsTable;
  myheader_t(:"/\\<h3\\>|\\<h2\\>|\\<h4/":) body;
};


Pstruct year_t{
  tagAgeln_t(:"h2":) year;
  session_t[] sessions : Pterm(Pre "/\\<h2\\>|\\<h4/");
};

Parray rest_t{
  Pstringln[];
}

Psource Pstruct entries_t{
  myheader_t(:"/\\<h1\\>/":) h;
  tagSln_t(:"h1":) title;
  Pstringln header;
  year_t[] data : Pterm (Pre "/\\<h4/");
  rest_t r;
}
