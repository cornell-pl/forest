Parray ip_t {
  Puint8 [4] : Psep('.') && Pterm(Pnosep);          
};

Parray hostname_t{
  Pstring_SE(:"/[. ]/":) [] : Psep('.') && Pterm(Pnosep); 
};

Punion client_t {
  ip_t       ip;      /- 135.207.23.32
  hostname_t host;    /- www.research.att.com
};

Punion auth_id_t {
  Pchar unauthorized : unauthorized == '-'; /- non-authenticated http session
  Pstring(:' ':) id;                         /- login supplied during authentication
};

Pstruct version_t {
  "HTTP/";
  Puint8 major; '.';
  Puint8 minor;           /- http minor mode
};

Penum method_t {
    GET,     PUT,     POST,     HEAD,     DELETE,     LINK,     UNLINK 
};

int  checkVersion(version_t version, method_t meth) {
  if ((version.major == 1) && (version.minor == 1)) return 1;
  if ((meth == LINK)  || (meth == UNLINK )) return 0;
  return 1;
}

Pstruct request_t {
  '\"';   method_t       meth;             /- Method used during request
  ' ';    Pstring(:' ':) req_uri;          /- Requested uri.
  ' ';    version_t      version : checkVersion(version, meth); /- HTTP version number of request 
  '\"';
};

Ptypedef Puint16_FW(:3:) response_t : response_t x => { 100 <= x && x < 600};

Punion length_t {
  Pchar unavailable : unavailable == '-';
  Puint32 len;    
};

Precord Pstruct entry_t {
         client_t       client;          /- Host/IP address of client requesting service
   ' ';  auth_id_t      remoteID;        /- Remote identity; '-' indicates not obtained.
   ' ';  auth_id_t      auth;            /- Name of authenticated user.
   " ["; Pdate(:']':)   date;            /- Timestamp of request.
   "] "; request_t      request;         /- Request.
   ' ';  response_t     response;        /- 3-digit response code
   ' ';  length_t       length;          /- Number of bytes in request response.
};

Psource Parray clt_t {
  entry_t [];
}
