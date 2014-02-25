      *-------------------------------------------------------------,   
      *                           I J N L T I F H                   :   
      *  COPYLIB IJNLTIFH                               LENGTH=0194 :   
      *-------------------------------------------------------------.   
      * WARNING !! ANY CHANGES TO THIS COPYBOOK MUST BE ACCOMPANIED :   
      * BY CHANGES ON THE COPYBOOK  IJNLTIFH                        :   
      *-------------------------------------------------------------.   
      *                                                               * 
      *         T I F H - HEADER RECORD FOR TIF                       * 
      *                                                               * 
      * MEMBER NAME: IJNLTIFH                                         * 
      *                                                               * 
      * REL   DATE      INI  CHANGES HISTORY                          * 
      * ----  --------  ---  ---------------------------------------- * 
      * 95.7  03/10/95  WYH  ADDED 'V' TO VALID FILE TYPE.            * 
      *                      ADDED 'R' TO VALID ACCRUAL IND.          * 
      *                      ADDED 'CIA' 'SBS' 'TA' THR' TO VALID     * 
      *                             SOURCE SYSTEM.                    * 
      *                      CHANGED PROCESS ENTITY FROM 3 BYTE TO 2. * 
      * 95.7  04/24/95  JLS  CHANGED TO STANDARD NAMES.               * 
      * 95.7  05/03/95  MDH  ADDED A NEW SRCE SYS 'LION'              * 
      * 96.2  02/13/96  CAS  ADDED FIELD HDR-BUNDLER-FILE-SEQ-NUM     * 
      *                      ADDED 'IIC ' TO VALID SOURCE SYSTEM      * 
      * 96.2  02/23/96  LJZ  DELETED FIELD HDR-BUNDLER-FILE-SEQ-NUM   * 
      * 96.4  07/22/96  BAB  ADDED 'CUEM TO VALID SOURCE SYSTEM       * 
      * 98.04 08/22/97  RSS  ADDED VALUES TO FILE TYPE                * 
      *                      ADDED VALUES FOR 98.04 RELEASE           * 
      * 98.07 03/24/98  WBL  ADDED UB CYCLE CODE AND PARTITION        * 
      *                      ADDED NEW VALUES FOR COPY-ID, SOURCE     * 
      *                      SYSTEM, COMPLEX-ID                       * 
      *                      CORRECTED SIZE AND ADDED SUB-FIELD       * 
      *                      FORMAT FOR SMS DATA AREAS                * 
      * 98.07 04/22/98  TDM  REDEFINED THE HDR-PRCSS-ENTY-CD FIELD FOR* 
      *                      UB HDR-UB-BL-CYC-NB FIELD.               * 
      ***************************************************************** 
         05  XX-HDR-RC.                                           
                                                                  
             07  XX-HDR-REC-TYPE-CD           PIC X(01).          
                 88  XX-HDR-REC-TYPE-VLD      VALUE LOW-VALUES.   
             07  XX-HDR-CPY-CD                PIC X(02).          
                 88  XX-HDR-COPY-CD-VLD          VALUE            
                     'A ' 'B ' 'C ' 'D ' 'E ' 'F '                
                     'AA' 'BA' 'CA' 'DA' 'EA' 'FA'                
                     'AB' 'BB' 'CB' 'DB' 'EB' 'FB'                
                     'AC' 'BC' 'CC' 'DC' 'EC' 'FC'                
                     'A1' 'B1' 'C1' 'D1' 'E1' 'F1'                
                     'A2' 'B2' 'C2' 'D2' 'E2' 'F2'                
                     'A3' 'B3' 'C3' 'D3' 'E3' 'F3'                
                     'A4' 'B4' 'C4' 'D4' 'E4' 'F4'                
                     'DA' '  '.                                   
      ***************************************************************** 
      * VALUE OF '9' INDICATES A VTNS BACKOUT                         * 
      ***************************************************************** 
             07  XX-HDR-FL-TYPE-CD            PIC X(01).          
                 88  XX-HDR-FILE-TYPE-VLD     VALUE               
                           'A' 'B' 'C' 'E' 'F' 'M' 'O' 'R'        
                           'S' 'U' 'V' 'W' '9'.                   
                                                                  
             07  XX-HDR-SRCE-SYS-CD           PIC X(04).          
                 88  XX-HDR-SRCE-SYS-VALID    VALUE               
                           'BAR ' 'LION' 'MAR ' 'NRJ ' 'CUEM'     
                           'CIA ' 'SBS ' 'TA  ' 'THR ' 'IIC '     
                           'CBSV' 'UBLR' 'FIS'.                   
                                                                  
             07  XX-HDR-DLM-DT.                                   
                  09  XX-HDR-MO               PIC 9(02).          
                  09  XX-DT-DLM-1             PIC X(01).          
                  09  XX-HDR-DY               PIC 9(02).          
                  09  XX-DT-DLM-2             PIC X(01).          
                  09  XX-HDR-YR               PIC 9(04).          
             07  XX-HDR-DT         REDEFINES                      
                 XX-HDR-DLM-DT                PIC X(10).          
             07  XX-HDR-DLM-TM.                                   
                  09  XX-HDR-HR               PIC 9(02).          
                  09  XX-TM-DLM-1             PIC X(01).          
                  09  XX-HDR-MI               PIC 9(02).          
                  09  XX-TM-DLM-2             PIC X(01).          
                  09  XX-HDR-SC               PIC 9(02).          
             07  XX-HDR-TM         REDEFINES                      
                 XX-HDR-DLM-TM                PIC X(08).          
             07  XX-HDR-COM-DLM-DT.                               
                  09  XX-HDR-COM-MO              PIC 9(02).       
                  09  XX-HDR-COM-DLM-1           PIC X(01).       
                  09  XX-HDR-COM-DY              PIC 9(02).       
                  09  XX-HDR-COM-DLM-2           PIC X(01).       
                  09  XX-HDR-COM-YEAR.                            
                      11  XX-HDR-COM-CC          PIC 9(02).       
                      11  XX-HDR-COM-YR          PIC 9(02).       
             07  XX-HDR-COM-DT     REDEFINES                      
                 XX-HDR-COM-DLM-DT               PIC X(10).       
             07  XX-HDR-ACRL-CD                  PIC X(01).       
                 88  XX-HDR-ACRL-CD-VLD          VALUE            
                           'A' 'J' 'P' 'R'.                       
             07  XX-HDR-BL-CYC-CD                PIC X(02).       
                 88  XX-HDR-BL-CYC-CD-VLD        VALUE            
                     '01' '04' '07' '10' '13' '16' '20' '22'.     
                 88  XX-HDR-BL-CYC-CD-VLD-VT     VALUE            
                     '10' '31'.                                   
             07  XX-HDR-COMPLX-CD                PIC X(01).       
                 88  XX-HDR-COMPLX-CD-VLD-VT     VALUE 'C'.       
             07  XX-HDR-PRCSS-ENTY-CD            PIC X(02).       
             07  XX-HDR-VTNS-PARTITION REDEFINES                  
                 XX-HDR-PRCSS-ENTY-CD.                            
                 09 XX-HDR-VTNS-PRTN             PIC X(01).       
                    88  XX-HDR-VTNS-PRTN-VLD     VALUE            
                        '1' '2' '3' '4' '5' ' '.                  
                 09 FILLER                       PIC X(01).       
             07  XX-HDR-UB-BL-CYC-NB REDEFINES                    
                 XX-HDR-PRCSS-ENTY-CD            PIC X(02).       
                    88  XX-HDR-UB-BL-CYC-NB-VLD VALUE             
                        '  ' '01' THRU '99'.                      
             07  XX-HDR-SMS.                                      
                 09 XX-HDR-SMS-REC-TYPE-CD       PIC X(05).       
                 09 XX-HDR-SMS-RPC-CD            PIC X(02).       
                 09 XX-HDR-SMS-PE-CD             PIC X(02).       
                 09 FILLER                       PIC X(01).       
                 09 XX-HDR-SMS-JOBNAME           PIC X(08).       
                 09 XX-HDR-SMS-SEQ-NM            PIC X(06).       
                 09 XX-HDR-SMS-DATE.                              
                    11 XX-HDR-SMS-YY             PIC X(02).       
                    11 XX-HDR-SMS-DDD            PIC X(03).       
                 09 XX-HDR-SMS-TIME.                              
                    11 XX-HDR-SMS-HH             PIC X(02).       
                    11 XX-HDR-SMS-MM             PIC X(02).       
                    11 XX-HDR-SMS-SS             PIC X(02).       
                 09 FILLER                       PIC X(13).       
             07  FILLER                          PIC X(12).       
      *-------------------------------------------------------------.   
      *                     E N D   O F  I J N L T I F H            :   
      *-------------------------------------------------------------'   
