      ****************************************************************  
      *         T I F T - TRAILER RECORD FOR TIF                     *  
      *                                                              *  
      * MEMBER NAME: IJNLTIFT                                        *  
      *                                                              *  
      * REL   DATE      INI  CHANGES HISTORY                         *  
      * ----  --------  ---  --------------------------------------- *  
      * 97.3  06/03/97  WBL  ADDED SUB FIELDS FOR SMS AREA (NO CHANGE*  
      *                      IN RECORD LENGTH OR OTHER AREAS         *  
      * 95.7  04/24/95  JLS  CHANGED  TO STANDARD NAMES.             *  
      * 95.7  03/10/95  WYH  NEW TIF TRAILER RECORD LAYOUT.          *  
      *                                                              *  
      ****************************************************************  
               05  XX-TRL-RC.                                           
                   07  XX-TRL-RC-TYPE-CD       PIC X(01).               
                       88  XX-TRL-RC-TYPE-VLD     VALUE HIGH-VALUES.    
                   07  XX-TRL-FILE-RC-CNT      PIC S9(09)    COMP-3.    
                   07  XX-TRL-FILE-AMT         PIC S9(09)V99 COMP-3.    
                   07  XX-TRL-SMS.                                      
                       09  XX-TRL-SMS-REC-TYP PIC X(05).                
                       09  XX-TRL-SMS-CNT      PIC 9(09).               
                       09  XX-TRL-SMS-COMMON PIC X(34).                 
                       09  FILLER              PIC X(12).               
      ****************************************************************  
      *    END OF COPY MEMBER IJNLTIFT                               *  
      ****************************************************************  
