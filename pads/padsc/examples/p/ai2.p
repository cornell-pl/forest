parray nIP {
  auint8 [4] : sep == '.' && term == ' ';          
};

parray sIP{
  astringSE(:"[. ]":) [] : sep == '.' && term == ' '; 
}

punion host_t  {
  nIP resolved;    /- 135.207.23.32
  sIP symbolic;    /- www.research.att.com
};

ptypedef a_char unknown_t :: unknown_t x => { x == '-'};

punion auth_id_t {
  unknown_t unauthorized;                      /- non-authenticated http session
  a_string(:' ':) id;                           /- login supplied during authentication
};

punion contentOpt_t {
  a_uint32 len;                                 /- length available
  unknown_t unavailable;
};

pstruct http_v_t {
  "HTTP/";
  a_uint8 major; '.';
  a_uint8 minor;           /- http minor mode
};

penum http_method_t {
    GET, 
    PUT, 
    POST, 
    HEAD, 
    DELETE, 
    LINK,        /- Unused after http 1.1
    UNLINK       /- Unused after http 1.1
};

int  checkVersion(http_v_t version, http_method_t meth) {
  if ((version.major == 1) && (version.minor == 1)) return 1;
  if ((meth == LINK)  || (meth == UNLINK ))  return 0;
  return 1;
}

pstruct http_request_t {
  '\"';  http_method_t  meth;            /- Method used during request
  ' ';   a_string(:' ':) req_uri;         /- Requested uri.
  ' ';   http_v_t       version : checkVersion(version, meth);
                                         /- HTTP version number of request 
  '\"';
};

precord pstruct http_clf_t {
   host_t host;                   /- IP address of client requesting service
   auth_id_t remoteID;            /- Remote identity; '-' indicates not obtained.
   ' ';   auth_id_t auth;                /- Name of authenticated user.
   " [";  a_date(:']':) date;             /- Timestamp of request.
   "] ";  http_request_t request;        /- Request.
   ' ';   a_uint16FW(:3:) response;       /- 3-digit response code
   ' ';   contentOpt_t contentLength;    /- Number of bytes in request response.
};

