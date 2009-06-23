Ptypedef Pstring_SE(:Peor:) Pstringln;

Punion Host_t{
  Pip             ipAddress;
  Pstring(:' ':)  hostName;
};

Punion Auth_t{
  Pchar          unavailable : unavailable == '-';
  Pstring(:' ':) name;
};

Penum Method_t{GET,HEAD, OPTIONS, POST, T, PROPFIND};

Ptypedef Pstring(:' ':) Path_t; 
Ptypedef Pstring_SE(:"/\" /":) Url_t;

Pstruct Version_t{
  "HTTP/";  Puint8         major;
  '.';      Puint8_FW(:1:) minor;
};

Ptypedef Puint16 Response_t : Response_t x => {(x == 0) || (200 <= x && x < 600)};

Punion Platform_t{
  Pstring_ME(:"/PSP \\(Playstation Portable\\); 2.00/":) PSP;
  Pstring_SE(:"/[;)]/":)                                 Other;
}

Pstruct PlatformInfo_t{
   ' '; 
   '('; Platform_t[] platform : Psep("; ") && Pterm(Pre "/;?\\)/"); 
   Pre "/;?/";
   ')';
};

Pstruct Browser_t{
        Pstring_SE(:"=[/ ]=":) browserName;
   Pre "=[/ ]=";
   Pstring_SE(:"/[ \"]/":)     version;
   Popt PlatformInfo_t         platformInfo;
   Pre "/(  \\[en\\])?/";
};

Pstruct Request_t{
  '"'; Method_t   method;
  ' '; Path_t     path;
  ' '; Version_t  version;
  '"';
};

Pstruct Keep_t{
        Host_t  requestSource;
  ' ';  Auth_t  remoteID;
  ' ';  Auth_t  authID;
  " ["; Ptimestamp(:']':) ts;                  
  "] "; Request_t         request;
  ' ';  Response_t        response;
  ' ';  Puint32           numBytes;
}

Precord Ptypedef Keep_t Keepln_t;


Precord Pstruct entry_t{
     Keep_t            keep;
     Pstringln         rest;
};
