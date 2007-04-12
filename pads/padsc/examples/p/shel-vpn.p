
Pstruct repeat_t {
  "last message repeated ";
  Puint8 count;
  " time";
  Pre "/s{0,1}/";
};

/*
Penum SysName_t {
  alivpn1, alivpn2, alivpn3, alivpn4, alivpn5, alivpn6, alivpn,
  kcivpn1, kcivpn2, kcivpn3, kcivpn4, kcivpn5, kcivpn6, 
  ukvpn1,  ukvpn2,  ukvpn3 
};
*/

Ptypedef Pstring(:' ':) SysName_t;


Pstruct CSFW_t {
  " [";         Puint8          id1;
  "] Rule[";    Pstring(:' ':)  ruleName;
  "  ";         Puint8          id2;
  "] Firewall:  [";  Pip        ip1;
  ':';          Puint32         port1;
  '-';          Pip             ip2;
  ':';          Puint32         port2;
  ", proto:";   Pstring(:']':)  protocol;
  "], action: "; Pstring(:'\\':) action;
  "\\n";
};

Pstruct ISAKMP_message_t{
  " in message from ";
  Pip msgSource;
};

Pstruct ISAKMP_version_t{
  ' ';    Puint32[] version : Psep('.') && Pterm(' ');
  ' ';
};

Pstruct ISAKMP_client_t{
  ": ";   Pstring_SE(:"/( \\d)|$/":) clientName;  /* end with space digit or end of record */
          Popt ISAKMP_version_t  clientVersion;
};

Pstruct ISAKMP_invalid_t{
  "! ";     Pstring(:':':) programName;
  ":line "; Puint32        lineNumber;
};

Pstruct ISAKMP_error_t{
  " (";
  Pstring(:')':) errorReason;
  ") received from ";
  Pip errorFrom;
};

Pstruct  ISAKMP_other_t {
        Pip entity;
   ' '; Pstring_SE(:Peor:) msg;
};

Penum ISAKMP_tag_t{
  NoProposal         Pfrom("No proposal chosen"), 
  ClientVersion      Pfrom("Client Version Information"),
  invalid_id         Pfrom("invalid id"),
  Invalid_ID         Pfrom("Invalid ID information"),
  Invalid_Payload    Pfrom("Invalid payload type"),
  ErrorNotification  Pfrom("Error notification"),
  ISAKMP_AuthFailure Pfrom("Authentication failure"),
  ISAKMP_Other       Pfrom("")
};


Punion ISAKMP_body_t (:ISAKMP_tag_t tag:){
  Pswitch(tag){ 
    Pcase NoProposal:         ISAKMP_message_t   noProposalMsg;
    Pcase ClientVersion:      ISAKMP_client_t    clientMsg;
    Pcase invalid_id:         ISAKMP_invalid_t   invalidMsg;
    Pcase Invalid_ID:         ISAKMP_message_t   InvalidMsg;
    Pcase Invalid_Payload:    ISAKMP_message_t   invalidPayloadMsg;
    Pcase ErrorNotification:  ISAKMP_error_t     errorMsg;
    Pcase ISAKMP_AuthFailure: ISAKMP_message_t   authFailureMsg;
    Pcase ISAKMP_Other:       ISAKMP_other_t     ipMsg;
  }
};

Pstruct ISAKMP_t{
  " ["; Puint32                     ISAKMP_id;
  "] "; ISAKMP_tag_t                ISKAMP_tag;
        ISAKMP_body_t(:ISKAMP_tag:) ISKAMP_body;
};

Pstruct HWAccel_t{
  " ["; Puint32                   HWAccel_id;
  "] "; Pstring_SE(:Peor:)      HWAccellMsg;
};


Punion Host_t{
  Pchar                unavailable : unavailable == '-';
  Pstring(:']':)       noNetwork : Pstring_eq_cstr(&noNetwork, "No Access Network");
  Pip                  ipAddress;
  Pstring(:']':)       namedHost;
};

Punion Trailer_t{
  Pstring_ME(:"/Access Network Passed/":)       accessNetwork;
  Puint32                                       trailer_id;
}

Penum AddressType_t{IPSEC, LOCAL};

Pstruct address_t{
           AddressType_t addressTy;
  '[';     Host_t        host; 
  "]:";   Trailer_t      trailer;
};

Pstruct PhysicalAddresses_t{
  "physical addresses: ";
  "remote ";  Pip remoteIP;
  " local ";  Pip localIP;
};

Punion PipOpt{
  Pchar unavailable : unavailable == '-';
  Pip IPAddress;
};

Pstruct IPPair_t{
  PipOpt ip1;
  ' '; 
  PipOpt ip2;
};

Pstruct AssignedIP_t{
  "assigned IP address ";
  Pip assignedIP;
  ", mask ";  
  Pip assignedMask;
};

Punion SecuritySessionBody_t{
    PhysicalAddresses_t                physicalAddresses;
    Pstring_ME(:"/logged out/":)       loggedOut;
    Pstring_ME(:"/No response from client - logging out/":)       
                                       noResponse;
    Pstring_ME(:"/- authentication failed using all authservers/":)       
                                       authFailed;
    Pstring_ME(:"/authentication failed using RADIUS/":)       
                                       radiusAuthFailed;
    IPPair_t                           ipPair;
    AssignedIP_t                       assignedIP;
};


Pstruct SecuritySessionPayload_t{
  Popt Pstring_ME(:"/[0-9a-z]+/":) sessionID;
  Pre "/(:  )?/";
  address_t address;
  ' ';  SecuritySessionBody_t securitySessionBody;
};

Punion SecuritySession_t{
  Pstring_ME(:"/IPSEC uid invalid - authentication failed/":) securitySessionAuthFailure;
  SecuritySessionPayload_t securitySessionPayload;
}

Pstruct SecurityAlert_t{
  "IP OPTIONS RECEIVED from "; Pip            src;
  " (";                        Pstring(:')':) explanation;
  ')';
};

Pstruct SecurityRadius_t{
   "\"";  Pstring(:'\"':) securityRadiusName;
   "\" access DENIED by server \"";
   Pip serverIP;
   "\".";
};

Pstruct SecurityAccount_t{
  "IPSEC[";
  Pstring(:']':) accountID;     /* hex number? */
  "] no LOCAL account found";
};

Penum SecurityMsgTag_t{ SecuritySession Pfrom("Session"), Alert, RADIUS, Account};

Punion SecurityBody_t(:SecurityMsgTag_t tag:){
  Pswitch(tag){
    Pcase (SecuritySession): SecuritySession_t securitySession;
    Pcase (Alert):           SecurityAlert_t   securityAlert;
    Pcase (RADIUS):          SecurityRadius_t  securityRadius;
    Pcase (Account):         SecurityAccount_t securityAccount;
  }
};

Pstruct Security_t {
  " ["; Puint32 security_id;
  "] "; SecurityMsgTag_t securityTag;
  Pre "/:? /"; SecurityBody_t(:securityTag:) securityBody;
};

Pstruct FailedLogIn_t{
  Pre "/([.]   Invalid Account)?/";
  ": Username=";  Pstring(:':':)   userName;
  ": Date/Time="; Pdate(:' ':)     loginDate;
  ' ';            Ptime_SE(:Peor:) loginTime;
};

Penum SNMPTrapTag_t{
    SNMPServer    Pfrom ("SNMP Servers"), 
    SNMPDiskRed   Pfrom ("Disk Redundancy"),
    SNMPHardDisk  Pfrom ("Hard Disk"),
    SNMPDualPower Pfrom ("Dual Power Supply")
};

Pstruct SNMPservMsg_t{
  ": "; Pstring(:';':) srvMessage;
};

Pstruct SNMPDiskRed_t{
  ": "; Pstring(:';':) diskRedMessage;
};

Pstruct SNMPHardDisk_t{
  ' ';           Puint32        diskID;
  ": Device ";   Pstring(:' ':) deviceName;
  " not available";
};

Pstruct SNMPDualPower_t{
  ": "; Pstring(:';':) dualPowerMessage;
};


Punion SNMPTrapBody_t(:SNMPTrapTag_t tag:){
  Pswitch(tag){
    Pcase SNMPServer:    SNMPservMsg_t   srvMessage;
    Pcase SNMPDiskRed:   SNMPDiskRed_t   diskRedMessage;
    Pcase SNMPHardDisk:  SNMPHardDisk_t  hardDisk;
    Pcase SNMPDualPower: SNMPDualPower_t dualPower;
  }
};

Pstruct SNMPTraps_t{
  " [";  Puint32   snmpTrapsID;
  "] ";  Puint32[] snmpTrapsList : Psep('.') && Pterm(' ');
  ' ';   Puint32   snmpInt1;
  ' ';   Puint32   snmpInt2;
  ' ';   SNMPTrapTag_t snmptag;
         SNMPTrapBody_t(:snmptag:) snmpBody;  
  "; \\n";
};

Pstruct VersionFuture_t{
  "Future Version (ID ";  
  Puint32 versionID;
  ')';
};

Punion Version_t{
  "unknown";
  VersionFuture_t future;
  Pstring(:')':) vName;
};

Punion Action_t{
  "none";
  Pstring(:')':) aName;
};


Pstruct Session_t{
  ": "; address_t sessionAddress;
  " Incoming client version (";  Version_t incVersion;
  "), minimum version (";        Version_t minVersion;
  ") push action (";             Action_t  action;
  "), action not needed";
};

Pstruct SNMPAuth_t{
  " on ";                Pstring(:' ':) requestType;
  " request from host "; Pip            hostIP;
  " using community string ";
                         Pstring_SE(:Peor:) communityString;
};

Pstruct InboundESP_t{
  " from "; Pip sourceIP;
  " to ";   Pip destIP;
  " SPI ";  Pstring(:' ':) hexID; /-- should be hex type; not currently implemented.
  " [";     Puint32 inboundESPID;
  "] ";
  "authentication failure detected--npbuf ";
  Pstring_SE(:Peor:) hexID2;
};

Pstruct NTP_t{
  " ["; Puint32 NIP_id;
  "] time reset "; Pfloat32 NTPtime; 
  " s";
};

Pstruct FTPSynch_t{
  "Synchronizing Pri [/";
  Pstring(:'/':) primary;
  "/] to Sec [/";
  Pstring(:'/':) secondary;
  "/] ...";
};

Punion FTPBody_t{
  Pstring_ME(:"/Update completed successfully/":) updateSuccess;
  Pstring_ME(:"/Update done. Check Status to assure the success/":) updateDone;
  FTPSynch_t FTPSynch;
};

Pstruct FTP_t{
  " ["; Puint32 FTP_Id;
  "] ";
  FTPBody_t FTPbody;
};

Penum MsgTy_t{
  CSFW, ISAKMP, Security, Session, SNMPTraps, NTP,
  InboundESP Pfrom ("Inbound ESP"),
  SNMPAuth Pfrom ("SNMP Authentication Failure"),
  failed Pfrom("Failed Login Attempt"),  
  failedRemoteLogin Pfrom("Failed Remote Network Login"),
  hwAccel Pfrom("Hw Accel unit"),  
  FTP Pfrom("FTP Backup")
};


Punion MsgPayload_t(:MsgTy_t ty:){
  Pswitch (ty){ 
    Pcase CSFW       :  CSFW_t        csfw;
    Pcase ISAKMP     :  ISAKMP_t      isakmp;
    Pcase Session    :  Session_t     session;
    Pcase hwAccel    :  HWAccel_t     HWAccel;
    Pcase InboundESP :  InboundESP_t  inboundESP;
    Pcase Security   :  Security_t    security;
    Pcase failed     :  FailedLogIn_t failedLogIn;
    Pcase failedRemoteLogin:
                        FailedLogIn_t failedRemoteLogIn;
    Pcase SNMPAuth   :  SNMPAuth_t    snmpAuth;
    Pcase SNMPTraps  :  SNMPTraps_t   snmpTraps;
    Pcase NTP        :  NTP_t         ntp;
    Pcase FTP        :  FTP_t         ftp;
  }
};

Pstruct main_t{
  ' '; Puint32 int1;
  ' '; Pdate(:' ':) date2;
  ' '; Ptime(:' ':) time2;
  ' '; Pstring(:' ':) mgr;
  ' '; Puint8 id;
  " : "; MsgTy_t msgTy;
       MsgPayload_t(:msgTy:) msg;
};

Punion Body_t{
  main_t   main_exp;
  repeat_t repeat;
};

Precord Pstruct entry_t{
       Pdate_explicit_ME(:"/[a-zA-Z]+\\s[0-9]+/", "%b %d", P_cstr2timezone("-0400"):) date;
  ' '; Ptime(:' ':) time;
  ' '; SysName_t    systemName;
  ' '; Body_t       body;
}
