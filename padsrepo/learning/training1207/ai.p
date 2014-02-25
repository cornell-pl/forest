#include <training.p>

typedef int bool;
#define true 1
#define false 0

Punion client_t {
  Pip       ip;      /- 135.207.23.32
  Phostname host;    /- www.research.att.com
};

Punion auth_id_t {
  dash Pfrom('-'); 
  Pid id;                        
};

Pstruct version_t {
  Pword http;  '/';  /- change from "HTTP/"
  Puint8 major; '.';
  Puint8 minor;          
};

Pstruct request_t {
  '\"';   Pword       meth;     
  ' ';    Purlbody req_uri;            /- wasn't sure if path or url (path in ai.3000?) 
  ' ';    version_t      version;
  '\"';
};

Ptypedef Puint16_FW(:3:) response_t : 
         response_t x => { 100 <= x && x < 600};

Punion length_t {
  unavailable Pfrom('-');
  Puint32 len;    
};

Precord Pstruct entry_t {
         client_t       client;          
   ' ';  auth_id_t      remoteID;        
   ' ';  auth_id_t      auth;            
   " ["; Pdate(:':':)   date;   
   ':';  Ptime(:']':);  time;  /- note change from Ptime(' ') + timezone    
   "] "; request_t      request;         
   ' ';  response_t     response;        
   ' ';  length_t        length;          
};

Psource Parray clt_t {
  entry_t [];
}
