parray nIP {
  auint8 [4] : sep == '.' && term == ' ';          
};

parray sIP{
  astringSRE(:"[. ]":) [] : sep == '.' && term == ' '; 
}

punion host_t  {
  nIP resolved;    //- 135.207.23.32
  sIP symbolic;    //- www.research.att.com
};

punion auth_id_t {
  auint8 unauthorized : unauthorized == '-';   //- non-authenticated http session
  astring(:' ':) id;                           //- login supplied during authentication
};

pstruct http_v_t {
  "HTTP//";
  auint8 major; '.';
  auint8 minor;
};

int checkVersion(http_v_t version, astring meth) {
  if ((version.major == 1) && (version.minor == 1))
     return 1;
  if ((strcmp(meth.str, "LINK") == 0 ) || strcmp(meth.str, "UNLINK") == 0 )
     return 0;
  return 1;
}

pstruct http_request_t {
  '\"';
  astring(:' ':) meth;             //- Method used during request
  ' ';
  astring(:' ':) req_uri;          //- Requested uri.
  ' ';
  http_v_t       version : checkVersion(version, meth);
                                  //- HTTP version number of request 
  '\"';
};

pstruct http_clf_t {
   host_t host;                   //- IP address of client requesting service
   ' ';
   auth_id_t remoteID;            //- Remote identity; '-' indicates not obtained.
   ' ';
   auth_id_t auth;                //- Name of authenticated user.
   " [";
     adate(:']':) date;           //- Timestamp of request.
   "] ";
   http_request_t request;        //- Request.
   ' ';
   auint16FW(:3:) response;       //- 3-digit response code
   ' ';
   auint32 contentLength;         //- Number of bytes in request response.
   '\n';
};

