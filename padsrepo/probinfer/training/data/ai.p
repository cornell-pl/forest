typedef int bool;
#define true 1
#define false 0

#include "basetokens.p"

Punion client_t {
  PPip       ip;      /- 135.207.23.32
  PPhostname host;    /- www.research.att.com
};

Punion auth_id_t {
  PPpunc_hyphen  h1; 
  PPid id1;                        
};

Penum method_t {
    GET,    PUT,  POST,  HEAD,     
    DELETE, LINK, UNLINK 
};

Pstruct version_t {
  "HTTP/";
  Puint8 major; '.';
  Puint8 minor;          
};

bool chkVersion(version_t v, method_t m) {
  if ((v.major == 1) && (v.minor == 1)) return true;
  if ((m == LINK) || (m == UNLINK)) return false;
  return true;
};

Pstruct request_t {
          PPpunc_dquote   dquote1;
//          method_t       meth;
          PPword word2;  
          PPwhite        w1;   
//          Pstring(:' ':) req_uri;
          PPpath         req_uri;
          Popt PPpunc_star star1;
          Popt PPpunc_star star2;
          PPwhite        w2;  
//          version_t      version : 
//                  chkVersion(version, meth);
//          Pstring(:'\"':)         blob;
          PPword word1;
          PPpunc_slash slash1;
          PPfloat float1;
          PPpunc_dquote   dquote2;
};

Ptypedef Puint16_FW(:3:) response_t : 
         response_t x => { 100 <= x && x < 600};

Punion length_t {
//  Pchar unavailable : unavailable == '-';
  PPpunc_hyphen hyphen1;
  Puint32 len;    
};

Precord Pstruct entry_t {
         client_t       client;          
         PPwhite        white1;
         auth_id_t      remoteID;
         PPwhite        white2;        
         auth_id_t      auth;
         PPwhite        white6;
       PPpunc_lsqubrac    lsqubrac;        
/*
        Pdate(:':':)   date;
        PPpunc_colon  colon;
        Ptime(:' ':)    time;  
         PPwhite        white3;   
//       Pstring(:']':) timezone;
         PPpunc_hyphen hyphen7;
         PPint int7;
*/
        PPdate date;
        PPpunc_colon colon;
        PPtime time;
         PPpunc_rsqubrac   rsqubrac;   
         PPwhite       white7;
         request_t      request;
         PPwhite        white4;         
         response_t     response;
         PPwhite        white5;        
         length_t        length;          
};

Psource Parray clt_t {
  entry_t [];
}
