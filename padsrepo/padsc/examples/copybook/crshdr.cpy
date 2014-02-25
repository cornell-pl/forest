      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  
      *    RELEASE 9.4.6  CRS DISCOUNT FILE HEADER RECORD           *  
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  
       01  XX-CRS-HEADER.                                              
           05 FILLER               PIC X(06)     VALUE LOW-VALUES.     
           05 XX-BILL-CYCLE-DATE.                                      
              10 XX-BILL-CYC-YY    PIC 9(02)     VALUE ZEROES.         
              10 XX-BILL-CYC-MM    PIC 9(02)     VALUE ZEROES.         
              10 XX-BILL-CYC-DD    PIC 9(02)     VALUE ZEROES.         
           05 FILLER               PIC X(06)     VALUE LOW-VALUES.     
           05 XX-COPY-ID           PIC X(02)     VALUE SPACES.         
           05 XX-RPC-CD            PIC X(01)     VALUE SPACES.         
           05 FILLER               PIC X(06)     VALUE LOW-VALUES.     
           05 XX-CREATE-DATE.                                          
              10 XX-CREATE-YY      PIC 9(02)     VALUE ZEROES.         
              10 XX-CREATE-MM      PIC 9(02)     VALUE ZEROES.         
              10 XX-CREATE-DD      PIC 9(02)     VALUE ZEROES.         
           05 FILLER               PIC X(53)     VALUE LOW-VALUES.     
      * * * * * * * * * * *  END OF RECORD  * * * * * * * * * * * * *
