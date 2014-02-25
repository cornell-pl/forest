      *Appendix A - Copybook ijnltif
      *--------------------------------------------------------------.  00000010
      *                           I J N L T I F                      :  00000020
      *  COPYLIB IJNLTIF                                LENGTH=....  :  00000030
      *--------------------------------------------------------------:  00000040
      *                                                              :  00000050
      *          T I F - TAILORED JOURNALS INTERFACT DETAIL RECORD   :  00000060
      *                                                              :  00000070
      * MEMBER NAME:  IJNLTIF                                        :  00000080
      *                                                              :  00000090
      * THE RECORD IS COMPOSED OF THREE PARTS. THE FIRST 350 BYTES   :  00000100
      * IS 'EXTERNAL' DATA POPULATED BY THE BILLERS, THE SECOND PART :  00000110
      * IS 'INTERNAL' DATA WHICH IS DERIVED IN THE PROGRAM AND IT'S  :  00000120
      * 350 BYTES. THE THIRD PORTION OF THE RECORD IS THE 'EXTERNAL' :  00000130
      * TAX TRANSACTION (?? BYTES) WHICH OCCURS UP TO 30 TIMES.      :  00000140
      *                                                              :  00000150
      * SO THE SHORTEST RECORD IS 752 BYTES (WITHOUT TAX DATA) AND   :  00000160
      * THE LONGEST 3092 (WITH 30 TAX TRANSACTIONS).                 :  00000170
      *                                                              :  00000180
      * REL   DATE      INI  CHANGES HISTORY                         :  00000190
      * ----  --------  ---  --------------------------------------  :  00000200
      * 2000.01 10/12/99 TD - ADDED 'NT' & 'TX' TO TAX-CALC-ND-VLD   :  00000210
      *                     - ADDED AN 88-LEVEL TO TARIFF-12-ND WITH :  00000220
      *                       VALID VALUES OF 'Y' 'N' AND ' '.       :  00000230
      *                                                              :  00000240
      * 00.01 07/27/99  JAO  EXTERNAL PORTION OF RECORD:             :  00000250
      *                        REMOVED: CALL-900-CT                  :  00000260
      *                        ADDED:   TYPE-OF-VOLUME               :  00000270
      *                                 BUNDLED-FEAT-ND              :  00000280
      *                                 JRNL-ADJMT-CD                :  00000290
      *                                 BTN-LEC-REF-NB               :  00000300
      *                                 VALUES 7 & 8 TO              :  00000310
      *                                    MSG-RT-CLS-CD-VLD         :  00000320
      *                                 VALUE 6 TO MSG-TYPE-CD-VLD   :  00000330
      *                 INTERNAL PORTION OF RECORD:                  :  00000340
      *                        REMOVED: TYPE-OF-VOLUME               :  00000350
      *                                                              :  00000360
      * 99.10 04/16/99 ADDED XX-VOLUME-QTY TO EXTERNAL TIF           :  00000370
      *                    (PIC CLAUSE CHANGED 6/25/99)              :  00000380
      *                ADDED XX-PRICING-PLAN TO EXTERNAL TIF         :  00000390
      *                ADDED XX-RVPP-CD TO INTERNAL TIF              :  00000400
      *                ADDED XX-CHANNEL-ID TO THE INTERNAL TIF       :  00000410
      *                ADDED XX-PRICING-PLAN TO INTERNAL TIF         :  00000420
      *                JRNL-ENTRY-IET-TRANS MOVED TO INTERNAL TIF    :  00000430
      *       06/01/99 ADDED XX-PLATFORM-ID TO INTERNAL TIF          :  00000440
      *                INCREASED TIF FROM 750 BYTES TO 900           :  00000450
      *                INCREASED INTERNAL TIF FROM 576 900           :  00000460
      *                                                              :  00000470
      * 99.07 01/27/99 ADDED CUST-SEG-CD TO INTERNAL PART OF TIF     :  00000480
      *       02/10/99 REMOVED XX-MISC-RVNU-TYPE-CD-VLD VALUES       :  00000490
      *                                                              :  00000500
      * 99.04 11/17/98 ADDED NEW FIELD RVNU-XFER-CD AND UNREGU-SVC-ND:  00000510
      *                      FOR METRO ALL IN ONE PROJECT            :  00000520
      *                      ADDED NEW VALUES FOR BILLER TAX ID      :  00000530
      *       01/27/99       ADDED TAX-CAT-CODE TO INTERNAL          :  00000540
      *                      ADDED RVNU-XFER-CC TO INTERNAL          :  00000550
      *                                                              :  00000560
      * 99.01 09/02/98  SG   ADDED TAX CATEGORY CODE AFTER GEO-SRCE- :  00000570
      *                      ND AND A 88 LEVEL                       :  00000580
      *                      ADDED BUNDLE NAME AFTER ORGN-SRCE-COM-  :  00000590
      *                      DLM-DT                                  :  00000600
      *                      ADDED 'SERB' 'WHLB' 'RNTB' 'PSAB' 'I/SB':  00000610
      *                      'I/NB' 'I/IB' 'I/LB' 'FEAB' 'LSVB'      :  00000620
      *                      '1WAB' 'MOBB' '2WAB' TO SALE-TYPE VALUES:  00000630
      *                      ADDED 'V' TO TAX SERVICE GROUP CODE     :  00000640
      *                      VALUE                                   :  00000650
      *--------------------------------------------------------------:  00000660
      *--------------------------------------------------------------:  00000670
                                                                        00000680
           03 XX-DTL-RC.                                                00000690
      *--------------------------------------------------------------.  00000700
      * THE FOLLOWING LEVEL DEFINES THE FIXED LENGTH PORTION         :  00000710
      * OF THE TIF RECORD (BYTES 001-900), INCLUDING:                :  00000720
      *     A) THE EXTERNAL TIF (001-575)                            :  00000730
      *     B) THE INTERNAL TIF (576-900)                            :  00000740
      *--------------------------------------------------------------'  00000750
              05 XX-EXTRN-INTRN-INFO.                                   00000760
      *--------------------------------------------------------------.  00000770
      * THE FOLLOWING LEVEL DEFINES THE EXTERNAL TIF                 :  00000780
      * (BYTES 1-575) WHICH IS POPULATED BY THE FEEDER SYSTEMS       :  00000790
      *--------------------------------------------------------------'  00000800
                 07 XX-EXTRN-INFO.                                      00000810
                    09 XX-EDIT-GRP1.                                    00000820
                       10 XX-SUMM-KEY1.                                 00000830
                          11 XX-DTL-RC-TYPE-CD    PIC X(01).            00000840
                             88 XX-DTL-RC-TYPE-CD-VLD                   00000850
                                   VALUE 'R'.                           00000860
                          11 XX-SVC-CLS-CD        PIC X(05).            00000870
                          11 XX-SVC-KIND-CD       PIC X(02).            00000880
                          11 XX-MKT-SEG-CD        PIC X(02).            00000890
                             88 XX-MKT-SEG-CD-VLD                       00000900
                                   VALUE '00' '01' '03' '04' '05'.      00000910
                          11 XX-USOC-CD           PIC X(05).            00000920
                             88 XX-CAP-USOC-CD-VLD                      00000930
                                   VALUE 'CAPML' 'CAPAL' 'CAPMT'        00000940
                                         'CAPAT' 'CAPDT' 'CAPNG'        00000950
                                         'CAPNA'.                       00000960
                          11 XX-CHRG-TYPE-CD      PIC X(02).            00000970
                             88 XX-CHRG-TYPE-CD-VLD                     00000980
                                   VALUE '  ' '99'                      00000990
                                         '01' THRU '60'.                00001000
                          11 XX-MISC-RVNU-TYPE-CD PIC X(05).            00001010
                             88 XX-MISC-RVNU-FIDS-VLD                   00001020
                                   VALUE 'CCA  ' 'MCAN ' 'NRC  ' 'ROC  '00001030
                                         'SCCN'.                        00001040
                             88 XX-MISC-RVNU-FICT-VLD                   00001050
                                   VALUE 'RCU  ' 'SCU  '.               00001060
                             88 XX-MISC-MAINT-SITE-VISIT                00001070
                                   VALUE '00056'.                       00001080
                             88 XX-MISC-MAINT-TIME-LABOR                00001090
                                   VALUE '00057'.                       00001100
                             88 XX-MISC-INSTL-SITE-VISIT                00001110
                                   VALUE '00058'.                       00001120
                             88 XX-MISC-INSTL-TIME-LABOR                00001130
                                   VALUE '00059'.                       00001140
                             88 XX-MISC-MATERIALS                       00001150
                                   VALUE '00060'.                       00001160
                             88 XX-MISC-WIRELESS                        00001170
                                   VALUE '00061'.                       00001180
                          11 XX-FEAT-CD           PIC X(03).            00001190
                             88 XX-FEAT-ND-VLD                          00001200
                                   VALUE '   '                          00001210
                                         '001' THRU '007'               00001220
                                         '009' THRU '011'               00001230
                                         '013' THRU '034'.              00001240
                          11 XX-GEO-CD            PIC X(13).            00001250
                          11 XX-ACCT-NB           PIC X(13).            00001260
                          11 XX-VTNS-ACCT-NB REDEFINES                  00001270
                             XX-ACCT-NB.                                00001280
                             13 XX-VTNS-SBU       PIC X(11).            00001290
                             13 XX-FILLER         PIC X(02).            00001300
      *--------------------------------------------------------------.  00001310
      * FOR VTNS, WHEN THE ACCOUNT IS NON SUMMARY BILLED             :  00001320
      * (STAND ALONE ACCOUNT) THE XX-VTNS-ACCT-NB AND THE VTNS-      :  00001330
      * MSTR-ACCT-NB WILL BE THE SAME (THEY WILL CONTAIN THE         :  00001340
      * CBS BU NUMBER).                                              :  00001350
      *--------------------------------------------------------------'  00001360
                          11 XX-RC-LVL-CD         PIC X(01).            00001370
                             88 XX-RC-LVL-CD-VLD                        00001380
                                   VALUE ' ' 'L' 'T' 'F'.               00001390
                          11 XX-LEAD-ACCT-NB       PIC X(13).           00001400
                          11 XX-VTNS-MSTR-ACCT-NB REDEFINES             00001410
                             XX-LEAD-ACCT-NB.                           00001420
                             13 XX-VTNS-MBU       PIC X(11).            00001430
                             13 XX-FILLER         PIC X(02).            00001440
                          11 XX-BL-ACCT-NB        PIC X(13).            00001450
                          11 XX-VTNS-ACCT-ID REDEFINES                  00001460
                             XX-BL-ACCT-NB.                             00001470
                             13 XX-VTNS-SALES-OFF-CD                    00001480
                                                   PIC X(02).           00001490
                             13 XX-VTNS-MCN        PIC X(06).           00001500
                             13 XX-VTNS-MCN-SFX    PIC X(03).           00001510
                             13 XX-FILLER          PIC X(02).           00001520
                    09 XX-JRNL-GRS-AT      COMP-3  PIC S9(9)V99.        00001530
                    09 XX-EDIT-GRP2.                                    00001540
                       10 XX-SUMM-KEY2.                                 00001550
                          11 XX-ACCT-STAT-CD       PIC X(01).           00001560
                             88 XX-ACCT-STAT-CD-VLD                     00001570
                                   VALUE '1' THRU '4'.                  00001580
                             88 XX-ACCT-STAT-CD-VLD-VT                  00001590
                                   VALUE '1' '4'.                       00001600
                          11 XX-JRNL-CHRG-CAT-CD PIC X(01).             00001610
                             88 XX-JRNL-CHRG-CAT-CD-VLD                 00001620
                                   VALUE '0' THRU '8'.                  00001630
                          11 XX-ADJMT-SRCE-CD      PIC X(01).           00001640
                             88 XX-ADJMT-SRCE-CD-VLD                    00001650
                                   VALUE ' ' '1' THRU '7'.              00001660
                          11 XX-JRNL-ADJMT-CAT-CD PIC X(01).            00001670
                             88 XX-JRNL-ADJMT-CAT-CD-VLD                00001680
                                   VALUE ' ' '1' '2'                    00001690
                                         '5' THRU '9'.                  00001700
                          11 XX-CASH-TRNSCTN-CD    PIC X(01).           00001710
                             88 XX-CASH-TRNSCTN-CD-VLD                  00001720
                                   VALUE ' ' '0' '1' '2' 'A' 'B' 'C'.   00001730
                          11 XX-TRNSCTN-CD         PIC X(02).           00001740
                          11 XX-VTNS-ADJMT-RSN-CD REDEFINES             00001750
                             XX-TRNSCTN-CD         PIC X(02).           00001760
                          11 XX-ACCT-QLTY-CNTL-ND PIC X(01).            00001770
                             88 XX-ACCT-QLTY-CNTL-ND-VLD                00001780
                                   VALUE ' ' 'Y'.                       00001790
                       10 XX-MSG-CT        COMP-3  PIC S9(11).          00001800
                       10 XX-MSG-MN-CT     COMP-3  PIC S9(11).          00001810
                       10 XX-MSG-SC-CT     COMP-3  PIC S9(11).          00001820
                       10 XX-SUMM-KEY3.                                 00001830
                          11 XX-RCRG-NRCRG-ND      PIC X.               00001840
                             88 XX-RCRG-NRCRG-ND-VLD                    00001850
                                VALUE ' ' 'N' 'U' 'R'.                  00001860
                          11 XX-PROD-ELEM-CD       PIC X(10).           00001870
                       10 XX-JRNL-TAXBL-AT                              00001880
                                           COMP-3  PIC S9(9)V99.        00001890
                       10 XX-SUMM-KEY4.                                 00001900
                          11 XX-CASH-TRNSCTN-TO-CD PIC X(01).           00001910
                             88 XX-CASH-TRNSCTN-TO-CD-VLD               00001920
                                   VALUE ' ' '0' '1' '2' 'A' 'B' 'C'.   00001930
                          11 XX-JRNL-ADJMT-TO-CD PIC X(01).             00001940
                             88 XX-JRNL-ADJMT-TO-CD-VLD                 00001950
                                   VALUE ' ' '8' '9'.                   00001960
                          11 XX-ACCT-STAT-TO-CD    PIC X(01).           00001970
                             88 XX-ACCT-STAT-TO-CD-VLD                  00001980
                                   VALUE ' ' '1' THRU '4'.              00001990
                          11 XX-STUDY-CD           PIC X(04).           00002000
                             88 XX-STUDY-CD-VLD                         00002010
                                   VALUE '    ' '1NET' 'UPLN'           00002020
                                         'THRI' 'CNET' 'DNS '           00002030
                                         'VTNS' 'WSRC'.                 00002040
                          11 XX-ACCT-TYPE-CD       PIC X(02).           00002050
                          11 XX-EXEMPT-RSN-CD    REDEFINES              00002060
                             XX-ACCT-TYPE-CD       PIC X(02).           00002070
                          11 XX-HOME-ST-TAX-ND     PIC X(01).           00002080
                             88 XX-HOME-ST-TAX-ND-VLD                   00002090
                                   VALUE ' '  '0' 'Y' 'F' 'L'.          00002100
                          11 XX-MSG-RT-CLS-CD      PIC X(01).           00002110
                             88 XX-MSG-RT-CLS-CD-VLD                    00002120
                                   VALUE ' ' '0' THRU '8'.              00002130
                          11 XX-MSG-TYPE-CD        PIC X(01).           00002140
                             88 XX-MSG-TYPE-CD-VLD                      00002150
                                   VALUE ' ' '0' THRU '6'.              00002160
                          11 XX-MSG-SVC-TYPE-CD    PIC X(02).           00002170
                             88 XX-MSG-SVC-TYPE-CD-VLD                  00002180
                                   VALUE '  ' 'J1' 'J2'                 00002190
                                         '00' THRU '23'                 00002200
                                         '25' THRU '31'                 00002210
                                         '40' THRU '42'                 00002220
                                         '50' THRU '51'                 00002230
                                         '60' THRU '68'                 00002240
                                         '71' THRU '72'                 00002250
                                         '91' THRU '92'.                00002260
                          11 XX-BL-CAT-CD           PIC X(02).          00002270
                             88 XX-BL-CAT-CD-VLD                        00002280
                                   VALUE '  ' 'AA' 'BB' 'CC'            00002290
                                         'DD' 'EE' 'EX'                 00002300
                                         'FF' 'GG' 'GN'                 00002310
                                         'GR' 'GU' 'II'                 00002320
                                         'KK' 'LL' 'MM'.                00002330
                          11 XX-CAP-ID             PIC X(03).           00002340
                             88 XX-CAP-ID-VLD                           00002350
                                   VALUE '   '                          00002360
                                         '000' THRU '999'.              00002370
                          11 XX-TAX-NON-CMPLNC-ND PIC X(1).             00002380
                             88 XX-TAX-NON-CMPLNC-ND-VLD                00002390
                                   VALUE 'Y' 'N' ' '.                   00002400
                          11 XX-CUST-DEF-GP-CD     PIC X(6).            00002410
                          11 XX-TIF-JIF-ND         PIC X(1).            00002420
                             88 XX-TIF-JIF-ND-VLD                       00002430
                                   VALUE 'T' 'J' 'H' 'U'.               00002440
                          11 XX-BL-DT.                                  00002450
                             13 XX-BL-CENTRY       PIC X(2).            00002460
                             13 XX-BL-YR           PIC X(2).            00002470
                             13 XX-BL-MO           PIC X(2).            00002480
                          11 XX-SBS-BLNG-PRD-CD    PIC X(2).            00002490
                          11 XX-SPLT-BL-PULL-ND    PIC X(1).            00002500
                          11 XX-OFFR-CMPNT-ID      PIC X(8).            00002510
                          11 XX-ACCT-CMPNT-ID      PIC X(8).            00002520
                          11 XX-CMPNT-ID           PIC X(8).            00002530
                          11 XX-CMPNT-SECNDRY-ID PIC X(8).              00002540
                          11 XX-JRNL-CD            PIC X(8).            00002550
                          11 XX-JRNL-MSG-ID        PIC X(4).            00002560
                          11 XX-USG-CR-CARD-ND     PIC X(1).            00002570
                             88 XX-USG-CR-CARD-ND-VLD                   00002580
                                   VALUE ' ' '0' '1'.                   00002590
                          11 XX-BUS-RESDNT-ND      PIC X(1).            00002600
                             88 XX-BUS-RESDNT-ND-VLD                    00002610
                                   VALUE ' ' 'B' 'R' 'A'.               00002620
                          11 XX-ACSS-MTHD-CD       PIC X(1).            00002630
                             88 XX-ACSS-MTHD-CD-VLD                     00002640
                                   VALUE ' ' '0' '1' '2'.               00002650
                          11 XX-ADV-AREAR-ND       PIC X(1).            00002660
                             88 XX-ADV-AREAR-ND-VLD                     00002670
                                   VALUE ' ' 'R' 'V'.                   00002680
                          11 XX-RVNU-TYPE-CD       PIC X(1).            00002690
                             88 XX-RVNU-TYPE-CD-VLD                     00002700
                                   VALUE ' ' 'O' 'S' 'U'.               00002710
                          11 XX-HOLD-BL-ND         PIC X(1).            00002720
                             88 XX-HOLD-BL-ND-VLD                       00002730
                                   VALUE ' ' 'R' 'Y'.                   00002740
                             88 XX-HELD-BILL                            00002750
                                   VALUE 'Y'.                           00002760
                          11 XX-CNTRY-CD           PIC X(3).            00002770
                          11 XX-SLS-LOC-CD         PIC X(11).           00002780
                          11 XX-GEO-SHIP-TO-CD     PIC X(13).           00002790
                          11 XX-GEO-SHIP-FROM-CD PIC X(13).             00002800
                          11 XX-TAX-SVC-GP-CD      PIC X(1).            00002810
                             88 XX-TAX-SVC-GP-CD-VLD                    00002820
                                   VALUE 'A' 'M' 'P' 'W' 'Z'            00002830
                                         '8' 'S' 'C' 'D' 'V'.           00002840
                          11 XX-TAX-EXMPT-CD       PIC X(5).            00002850
                          11 XX-TAX-REASONS REDEFINES                   00002860
                             XX-TAX-EXMPT-CD.                           00002870
                             13 XX-TAX-RSN-FEDERAL PIC X(1).            00002880
                             13 XX-TAX-RSN-STATE PIC X(1).              00002890
                             13 XX-TAX-RSN-COUNTY PIC X(1).             00002900
                             13 XX-TAX-RSN-CITY    PIC X(1).            00002910
                             13 XX-TAX-RSN-SUBCITY PIC X(1).            00002920
                          11 XX-JRNL-JURIS-CD      PIC X(1).            00002930
                             88 XX-DOMN-NB-VLD                          00002940
                                   VALUE '0' THRU '8'.                  00002950
                          11 XX-MPS-JURIS-CD       PIC X(1).            00002960
                             88 XX-MPS-JURIS-NB-VLD                     00002970
                                   VALUE '1' '2' '3' '6'                00002980
                                         '7' '8' '9' ' '.               00002990
                          11 XX-SBS-CIA-SRCE-ND    PIC X(3).            00003000
                             88 XX-EVENT                                00003010
                                   VALUE 'BE '.                         00003020
                             88 XX-CCHARGE                              00003030
                                   VALUE 'CCS'.                         00003040
                             88 XX-CREDIT                               00003050
                                   VALUE 'CPC'.                         00003060
                       10 XX-LOCAL-ACSS-LINE-CT                         00003070
                                            COMP-3 PIC S9(07).          00003080
                       10 XX-EX-TYP-DIS-PL.                             00003090
                          11 XX-EXTRACT-TYPE           PIC X(03).       00003100
                             88 XX-EXTRACT-TYPE-VLD                     00003110
                                   VALUE 'IFX' 'IGR' 'IIN' 'IIP'        00003120
                                         'IIR' 'IIU' 'IJC' 'IJD'        00003130
                                         'IMC' 'IVD' 'IVC' 'IMD'        00003140
                                         'IPD' 'IPI' 'IPO' 'IPR'        00003150
                                         'IPS' 'IPX' 'ISI' 'ISU'        00003160
                                         'ITX' 'MIU' '   '.             00003170
                          11 XX-EXTRACT-TYPE-RED REDEFINES              00003180
                             XX-EXTRACT-TYPE.                           00003190
                             13  XX-FILLER             PIC X(01).       00003200
                             13  XX-MEMO-JRN-CD        PIC X(01).       00003210
                             13  XX-FILLER             PIC X(01).       00003220
                          11 XX-DISCOUNT-PLAN          PIC X(01).       00003230
                       10 XX-SUMM-KEY5.                                 00003240
                          11 XX-ORGN-SRCE-COMPLX-CD    PIC X(01).       00003250
                          11 XX-ORGN-SRCE-PRCSS-ENTY-CD                 00003260
                                                       PIC X(02).       00003270
                          11 XX-ORGN-SRCE-BL-CYC-CD    PIC X(02).       00003280
                          11 XX-ORGN-SRCE-COM-DLM-DT PIC X(10).         00003290
                          11 XX-BUNDLE-NAME REDEFINES                   00003300
                                XX-ORGN-SRCE-COM-DLM-DT                 00003310
                                                       PIC X(10).       00003320
                          11 XX-SRCE-DEST-CD           PIC X(02).       00003330
                             88 XX-SRCE-DEST-CD-VLD                     00003340
                                   VALUES 'BU' 'B1' 'SB' 'VT'           00003350
                                          'SY' 'WB' 'WE' 'WY' 'UL'.     00003360
                             88 XX-SRCE-DEST-IS-BUNDLER                 00003370
                                   VALUES 'BU' 'B1' 'SY'                00003380
                                          'WB' 'WY'.                    00003390
                             88 XX-SBS-TO-BUNDLER                       00003400
                                   VALUE 'BU'.                          00003410
                             88 XX-CREATED-BY-BUNDLER                   00003420
                                   VALUE 'B1'.                          00003430
                             88 XX-LION-TO-SBS                          00003440
                                   VALUE 'SB'.                          00003450
                             88 XX-LION-TO-BNDLR-SBS                    00003460
                                   VALUE 'SY'.                          00003470
                             88 XX-WEFOS-TO-BUNDLER                     00003480
                                   VALUE 'WB'.                          00003490
                             88 XX-LION-TO-WEFOS                        00003500
                                   VALUE 'WE'.                          00003510
                             88 XX-LION-TO-BNDLR-WEFOS                  00003520
                                   VALUE 'WY'.                          00003530
                             88 XX-VTNS-TO-TJ                           00003540
                                   VALUE 'VT'.                          00003550
                             88 XX-SRCE-DEST-UB                         00003560
                                   VALUE 'UL'.                          00003570
                          11 XX-SVC-PRVD-ID        PIC X(05).           00003580
                          11 XX-WRLS-CYC-END-DT    PIC X(08).           00003590
                          11 XX-BLR-TAX-ID         PIC X(05).           00003600
                                88 XX-BLR-TAX-ID-VLD                    00003610
                                      VALUE 'BAR  ' 'LION ' 'SBS  '     00003620
                                            'SBSJ ' 'THRF ' 'WATS '     00003630
                                            'WADJ ' 'AWC  ' 'VTNS '     00003640
                                            'VADJ ' 'DLSB ' 'DLWE '     00003650
                                            'UJSB ' 'UJWE ' 'UBLR '     00003660
                                            'UBLJ ' 'SWL  ' 'SWLJ '     00003670
                                            'TJSW '.                    00003680
                          11 XX-TYPE-OF-CHG        PIC X(02).           00003690
                                88 XX-TYPE-OF-CHG-VLD                   00003700
                                      VALUE 'BD' 'BL' 'BO' 'CA' 'CE'    00003710
                                            'CL' 'CN' 'DA' 'DO' 'RF'    00003720
                                            'FC' 'IN' 'IT' 'NF' 'LP'    00003730
                                            'MI' 'OT' 'RP' 'TR' 'US'    00003740
                                            'EL' 'JK' 'NR'.             00003750
                          11 XX-SRVC-STATE         PIC X(02).           00003760
                          11 XX-TRANS-TYPE         PIC X(04).           00003770
                             88 XX-TRANS-TYPE-VLD                       00003780
                                   VALUE 'SALE' 'BDBT' 'TRAD' 'REFD'    00003790
                                         'INTR' 'BADJ' 'LATE' 'RETN'    00003800
                                         'TRNS' 'SFTP' 'TXAJ' 'MEMO'    00003810
                                         'XCTM' 'UTEC' 'ILES' 'DISC'.   00003820
                          11 XX-SALE-TYPE          PIC X(04).           00003830
                             88 XX-SALE-TYPE-VLD                        00003840
                                   VALUE 'LSVC' 'FEAT' 'I/LT' 'I/IT'    00003850
                                         'I/NT' 'INFO' '1WAY' '2WAY'    00003860
                                         'MOBL' 'RNTL' 'PSAL' 'WHLS'    00003870
                                         'SERV' 'I/ST' 'XCTM' 'MEMO'    00003880
                                         'SERB' 'WHLB' 'RNTB' 'PSAB'    00003890
                                         'I/SB' 'I/NB' 'I/IB' 'I/LB'    00003900
                                         'FEAB' 'LSVB' '1WAB' 'MOBB'    00003910
                                         '2WAB'.                        00003920
                          11 XX-OCL-ND             PIC X(01).           00003930
                             88 XX-OC-ND-VLD                            00003940
                                   VALUE 'Y' 'N' ' '.                   00003950
                          11 XX-PRIM-SUPPL-ND      PIC X(01).           00003960
                             88 XX-PRIM-SUPPL-ND-VLD                    00003970
                                   VALUE 'P' 'S'.                       00003980
                          11 XX-CHG-EFF-DT         PIC X(08).           00003990
                          11 XX-LEASE-EFF-DT       PIC X(08).           00004000
                          11 XX-TERMS-CD           PIC X(04).           00004010
                          11 XX-PRICE-ELEM-GP      PIC X(04).           00004020
                          11 XX-CHG-FREQ           PIC X(01).           00004030
                             88 XX-CHG-FREQ-VLD                         00004040
                                   VALUE 'N' 'O' 'R'.                   00004050
                          11 XX-TAX-CALC-ND        PIC X(02).           00004060
                             88 XX-TAX-CALC-ND-VLD                      00004070
                                   VALUE 'RB' 'TF' '  '                 00004080
                                         'GR' 'AX' 'NR' 'FE'            00004090
                                         'NT' 'TX'.                     00004100
                          11 XX-CUST-SRVC-CTR-CD PIC X(11).             00004110
                          11 XX-BR-OFFC-CODE-REF REDEFINES              00004120
                             XX-CUST-SRVC-CTR-CD.                       00004130
                             13  XX-BR-OFFC-CD     PIC X(08).           00004140
                             13  FILLER            PIC X(03).           00004150
                          11 XX-INV-NB             PIC X(20).           00004160
                          11 XX-BL-DT-BL-PRD                            00004170
                                   REDEFINES XX-INV-NB.                 00004180
                             13 XX-BL-DT-REF.                           00004190
                                15 XX-BL-CC        PIC X(02).           00004200
                                15 XX-BL-YY        PIC X(02).           00004210
                                15 XX-BL-MM        PIC X(02).           00004220
                                15 XX-BL-DD        PIC X(02).           00004230
                             13 FILLER             PIC X(12).           00004240
                          11 XX-LINE-NB            PIC X(10).           00004250
                          11 XX-TYPE-OF-SVC        PIC X(03).           00004260
                          11 XX-III-ND             PIC X(01).           00004270
                             88 XX-III-ND-VLD                           00004280
                                   VALUE '0' '1' '2'                    00004290
                                         '3' '4' '5'                    00004300
                                         '7' '8' '9'                    00004310
                                         'L' ' '.                       00004320
                          11 XX-CKT-NUM            PIC X(24).           00004330
                          11 XX-BL-GRP             PIC X(03).           00004340
                          11 XX-PROD-BLNG-ID       PIC X(08).           00004350
                          11 XX-CALL-OCCUR-DT      PIC X(08).           00004360
                          11 XX-FILLER             PIC X(01).           00004370
                          11 XX-APPRTNMT-ND        PIC X(01).           00004380
                             88 XX-APPRTNMT-ND-VLD                      00004390
                                   VALUE 'C' 'F' 'U' ' ' 'Y'.           00004400
                          11 XX-DR-CR-CD           PIC X(02).           00004410
                             88 XX-DR-CR-CD-VLD                         00004420
                                   VALUE '10' '11' '60' '61' '  '.      00004430
                          11 XX-HYBRID-TAX-ND      PIC X(01).           00004440
                             88 XX-HYBRID-TAX-ND-VLD                    00004450
                                   VALUE '3' ' '.                       00004460
                          11 XX-TARIFF-12-ND       PIC X(01).           00004470
                             88 XX-TARIFF-12-ND-VLD                     00004480
                                    VALUE 'Y' 'N' ' '.                  00004490
                          11 XX-JURIS-SRCE-ND      PIC X(01).           00004500
                          11 XX-SVC-GP-SRCE-ND     PIC X(01).           00004510
                          11 XX-GEO-SRCE-ND        PIC X(01).           00004520
                          11 XX-TAX-CAT-CD         PIC X(02).           00004530
                             88 XX-TAX-CAT-CD-VLD                       00004540
                                    VALUE '01' '02' '03' '04' '05'      00004550
                                          '06' '07' '08' '09' '10'      00004560
                                          '11' '12' '13' '  '.          00004570
                          11 XX-RVNU-XFER-CD       PIC X(02).           00004580
                             88 XX-RVNU-XFER-CD-VLD                     00004590
                                    VALUE 'MN' '  '.                    00004600
                          11 XX-RVNU-XFER-FROM-TO-CD                    00004610
                                REDEFINES XX-RVNU-XFER-CD.              00004620
                                13  XX-RVNU-XFER-FROM-CD                00004630
                                                   PIC X(01).           00004640
                                13  XX-RVNU-XFER-TO-CD                  00004650
                                                   PIC X(01).           00004660
                          11 XX-FILLER             PIC X(01).           00004670
                          11 XX-VOLUME-QTY         PIC S9(07)V99        00004680
                                                       COMP-3.          00004690
                          11 XX-PRICING-PLAN       PIC X(08).           00004700
                          11 XX-PLATFORM-ID        PIC X(02).           00004710
                          11 XX-TYPE-OF-VOLUME     PIC X(04).           00004720
                          11 XX-BUNDLED-FEAT-ND    PIC X(01).           00004730
                             88 XX-BUNDLED-PBI     VALUE 'Y'.           00004740
                          11 XX-JRNL-ADJMT-CD      PIC X(03).           00004750
                             88 XX-JRNL-ADJMT-CD-VLD                    00004760
                                      VALUE 'COR' 'EFS' 'FRD' 'NBD'     00004770
                                            'BNK' 'CCC' 'PRC' 'LPF'     00004780
                                            'PLP' 'REC' '   '.          00004790
                          11 XX-BTN-LEC-REF-NB     PIC X(10).           00004800
                          11 XX-FILLER             PIC X(92).           00004810
                                                                        00004820
      *--------------------------------------------------------------.  00004830
      * THIS LEVEL DEFINES BYTE RANGE 576-900. THIS IS INTERNAL      :  00004840
      * AREA DESIGNED TO HOLD THE DERIVED DATA. IN ANOTHER WORD,     :  00004850
      * THIS IS OUTPUT AREA.                                         :  00004860
      *--------------------------------------------------------------'  00004870
                 07 XX-INTRN-INFO.                                      00004880
                    09 XX-FACCT-CD                 PIC X(10).           00004890
                    09 XX-PROD-SVC-CD              PIC X(10).           00004900
                    09 XX-CSUB-ACCT-CD             PIC X(05).           00004910
                    09 XX-TRNSCTN-EDIT-DT.                              00004920
                       11 XX-TRNSCTN-EDIT-DT-CENTRY                     00004930
                                                   PIC 9(02).           00004940
                       11 XX-TRNSCTN-EDIT-DT-YR    PIC 9(02).           00004950
                       11 XX-TRNSCTN-EDIT-DT-1     PIC X(01).           00004960
                       11 XX-TRNSCTN-EDIT-DT-MO    PIC 9(02).           00004970
                       11 XX-TRNSCTN-EDIT-DT-2     PIC X(01).           00004980
                       11 XX-TRNSCTN-EDIT-DT-DY    PIC 9(02).           00004990
                    09 XX-TRNSCTN-EDIT-DT-A REDEFINES                   00005000
                          XX-TRNSCTN-EDIT-DT       PIC X(10).           00005010
                    09 XX-TRNSCTN-EDIT-TM.                              00005020
                       11 XX-TRNSCTN-EDIT-TM-HR    PIC 9(02).           00005030
                       11 XX-TRNSCTN-EDIT-TM-1     PIC X(01).           00005040
                       11 XX-TRNSCTN-EDIT-TM-MN    PIC 9(02).           00005050
                       11 XX-TRNSCTN-EDIT-TM-2     PIC X(01).           00005060
                       11 XX-TRNSCTN-EDIT-TM-SC    PIC 9(02).           00005070
                    09 XX-TRNSCTN-EDIT-TM-A REDEFINES                   00005080
                          XX-TRNSCTN-EDIT-TM       PIC X(08).           00005090
                    09 XX-TRNSCTN-GP-NB      COMP  PIC S9(09).          00005100
                    09 XX-TRNSCTN-DERVD-NB COMP    PIC S9(09).          00005110
                    09 XX-TRNSCTN-OFFST-NB COMP    PIC S9(09).          00005120
                    09 XX-SOBP.                                         00005130
                       11 XX-SOBP-MNG-ENTY-CD      PIC X(04).           00005140
                       11 XX-SOBP-LGL-ENTY-CD      PIC X(03).           00005150
                       11 XX-SOBP-ACCT-ENTY-CD     PIC X(03).           00005160
                    09 XX-IET.                                          00005170
                       11 XX-IET-MNG-ENTY-CD       PIC X(04).           00005180
                       11 XX-IET-LGL-ENTY-CD       PIC X(03).           00005190
                       11 XX-IET-ACCT-ENTY-CD      PIC X(03).           00005200
                    09 XX-ORG-ORGNTG-CD            PIC X(09).           00005210
                    09 XX-ORG-CHRG-CD              PIC X(09).           00005220
                    09 XX-ST-CD                    PIC X(02).           00005230
                    09 XX-JRNL-ST-CD               PIC X(02).           00005240
                    09 XX-ALLOC-ND                 PIC X(01).           00005250
                    09 XX-TAX-CLSFCTN-CD           PIC X(05).           00005260
                    09 XX-TAX-CLS-CD               PIC X(01).           00005270
                    09 XX-LDS-ACSS-MTHD-CD         PIC X(01).           00005280
                    09 XX-FILE-TYPE-CD             PIC X(01).           00005290
                       88 XX-ADJMT-TYPE            VALUE 'A'.           00005300
                       88 XX-BBUE-TYPE             VALUE 'B'.           00005310
                       88 XX-CASH-TYPE             VALUE 'C'.           00005320
                       88 XX-BBUE-RVNU-TYPE        VALUE 'D'.           00005330
                       88 XX-EBUB1-TYPE            VALUE 'E'.           00005340
                       88 XX-EBUB2-TYPE            VALUE 'F'.           00005350
                       88 XX-EBUB1-RVNU-TYPE       VALUE 'G'.           00005360
                       88 XX-EBUB2-RVNU-TYPE       VALUE 'H'.           00005370
                       88 XX-MIU-RCYC-RVNU-TYPE    VALUE 'I'.           00005380
                       88 XX-MIU-WO-TYPE           VALUE 'M'.           00005390
                       88 XX-MCC-TYPE              VALUE 'O'.           00005400
                       88 XX-MIU-RCYC-TYPE         VALUE 'R'.           00005410
                       88 XX-MS-TYPE               VALUE 'S'.           00005420
                       88 XX-USG-TYPE              VALUE 'U'.           00005430
                       88 XX-RVNU-TYPE             VALUE 'V'.           00005440
                       88 XX-WO-RVNU-TYPE          VALUE 'W'.           00005450
                    09 XX-SRCE-SYS-ID              PIC X(04).           00005460
                    09 XX-JRNL-DT.                                      00005470
                       11 XX-JRNL-DT-CENTRY        PIC 9(02).           00005480
                       11 XX-JRNL-DT-YR            PIC 9(02).           00005490
                       11 XX-JRNL-DT-1             PIC X(01).           00005500
                       11 XX-JRNL-DT-MO            PIC 9(02).           00005510
                       11 XX-JRNL-DT-2             PIC X(01).           00005520
                       11 XX-JRNL-DT-DY            PIC 9(02).           00005530
                    09 XX-JRNL-DT-A REDEFINES                           00005540
                          XX-JRNL-DT               PIC X(10).           00005550
                    09 XX-JRNL-TM.                                      00005560
                       11 XX-JRNL-TM-HR            PIC 9(02).           00005570
                       11 XX-JRNL-TM-1             PIC X(01).           00005580
                       11 XX-JRNL-TM-MN            PIC 9(02).           00005590
                       11 XX-JRNL-TM-2             PIC X(01).           00005600
                       11 XX-JRNL-TM-SC            PIC 9(02).           00005610
                    09 XX-JRNL-TM-A REDEFINES                           00005620
                          XX-JRNL-TM               PIC X(08).           00005630
                    09 XX-ACRL-CD                  PIC X(01).           00005640
                    09 XX-PRCSS-TYPE-CD            PIC X(03).           00005650
                       88 XX-C-IET-TYPE                                 00005660
                             VALUE 'BC1' 'BC2' 'BC3' 'BC4'              00005670
                                   'BC5' 'BC6' 'BC7' 'PAY'.             00005680
                       88 XX-0-IET-TYPE                                 00005690
                             VALUE 'MIU' 'EB1' 'EB2' 'RCL'              00005700
                                   'BBE' 'RV1' 'RV2' 'RV3' 'RV4'.       00005710
                    09 XX-BVSJ-RVNU-CAT-CD         PIC X(01).           00005720
                    09 XX-DERVD-DFLT-CD            PIC X(01).           00005730
                    09 XX-LOC-CD                   PIC X(06).           00005740
                    09 XX-DERVD-SEQ-NB             PIC X(11).           00005750
                    09 XX-OFFSET-DRVR-ND           PIC X(01).           00005760
                    09 XX-FCE-ERR-CD               PIC X(01).           00005770
                    09 XX-DERVD-TB-ID              PIC X(03).           00005780
                    09 XX-LD-NRC-CD                PIC X(04).           00005790
                    09 XX-TAX-ONLY-ND              PIC X(01).           00005800
                       88 XX-TAX-ONLY-ND-VLD                            00005810
                             VALUE 'Y' 'N'.                             00005820
                    09 XX-CAP-OWN-CD               PIC X(01).           00005830
                    09 XX-CMPLX-ID                 PIC X(01).           00005840
                    09 XX-PRCSS-ENTY-CPY-ID        PIC X(02).           00005850
                    09 XX-TAX-JURIS-CD             PIC X(02).           00005860
                    09 XX-TAX-RT                   PIC SV9(5).          00005870
                    09 XX-TAX-RSN-CD               PIC X(04).           00005880
                    09 XX-ALT-MSG-ID               PIC X(04).           00005890
                    09 XX-CIA-FML-DESC             PIC X(20).           00005900
                    09 XX-CIA-FML-DESC-SUB         PIC X(20).           00005910
                    09 XX-MAXTAX-AT                                     00005920
                                           COMP-3  PIC S9(9)V9(4).      00005930
                    09 XX-TAX-NTAX-AT                                   00005940
                                           COMP-3  PIC S9(9)V9(4).      00005950
                    09 XX-ACCS-LN-TRNK-TYPE        PIC X(01).           00005960
                    09 XX-TAX-RT-TYPE              PIC X(01).           00005970
                    09 XX-SURCHG-AO-AT                                  00005980
                                           COMP-3  PIC S9(9)V9(4).      00005990
                    09 XX-MAXTAX-ND                PIC X(01).           00006000
                    09 XX-GRS-TAX-AT                                    00006010
                                           COMP-3  PIC S9(9)V9(4).      00006020
                    09 XX-RTRANS-DIST-RT                                00006030
                                           COMP-3  PIC SV9(5).          00006040
                    09 XX-RTRANS-DIST-TAX                               00006050
                                           COMP-3  PIC S9(9)V9(4).      00006060
                    09 XX-CUST-CODE-REQD           PIC X(01).           00006070
                    09 XX-DR-CR-ND                 PIC X(01).           00006080
                    09 XX-VTNS-PARTITION-NUM       PIC X(01).           00006090
                    09 XX-TAX-CAT-CD-FILL          PIC X(02).           00006100
                    09 XX-RVNU-XFER-CD-FILL        PIC X(02).           00006110
                    09 XX-UNREGU-SVC-ND            PIC X(01).           00006120
                    09 XX-CUST-SEG-GRP             PIC X(10).           00006130
                    09 XX-CUST-SEG-CD REDEFINES                         00006140
                       XX-CUST-SEG-GRP.                                 00006150
                       11 XX-CUST-SEG-ID           PIC X(07).           00006160
                       11 XX-CUST-REV-BAND         PIC X(03).           00006170
                    09 XX-RVPP-CD                  PIC X(06).           00006180
                    09 XX-CHANNEL-ID               PIC X(01).           00006190
                    09 XX-JRNL-ENTRY-IET-TRANS     PIC X(01).           00006200
                    09  FILLER                     PIC X(38).           00006210
                                                                        00006220
      *--------------------------------------------------------------.  00006230
      * THIS LEVEL REDEFINES AN INTERNAL PORTION AS AN EXTERNAL      :  00006240
      * PORTION FOR FIS. FROM 576 TO 900. THE OCC INFO WILL NOT      :  00006250
      * BE MAINTAINED AFTER THE FML CODE DERIVATION.                 :  00006260
      *--------------------------------------------------------------'  00006270
                 07 XX-EXTRN-MCC-INFO-2                                 00006280
                       REDEFINES XX-INTRN-INFO.                         00006290
                    09 XX-EXTRN-INFO-2.                                 00006300
                       11 XX-RMC-BKT-NB     PIC X(03).                  00006310
                          88 XX-RMC-BKT-NB-VLD                          00006320
                                VALUE '   ' '000' THRU '999'.           00006330
                       11 XX-MCC-CAPTN-CD PIC X(04).                    00006340
                          88 XX-OCC-CAPTN-CD-VLD                        00006350
                                VALUE '    '                            00006360
                                      '0000' THRU '9999'.               00006370
                       11 XX-MCC-PHRS-1     PIC X(51).                  00006380
                       11 XX-MCC-PHRS-2     PIC X(51).                  00006390
                       11 XX-NREV-TAX-ND    PIC X(3).                   00006400
                       11 FILLER            PIC X(213).                 00006410
                                                                        00006420
      *--------------------------------------------------------------.  00006430
      * THIS LEVEL DEFINES BYTE RANGE 901 TO 3092.                   :  00006440
      * THIS IS THE EXTERNAL TAX INFO AREA POPULATED BY BILLERS.     :  00006450
      *--------------------------------------------------------------'  00006460
              05 XX-TAX-SURCHG-INFO.                                    00006470
                 07 XX-CTM-ND              COMP    PIC S9(04).          00006480
                    88 XX-CTM-ND-VLD                                    00006490
                          VALUE 0 THRU 30.                              00006500
                 07 XX-TAX-TB                                           00006510
                       OCCURS 0 TO 30 TIMES                             00006520
                          DEPENDING ON  XX-CTM-ND.                      00006530
                    09 XX-TB-TAX-CLS-CD            PIC X(01).           00006540
                       88 XX-TB-TAX-CLS-CD-VLD                          00006550
                             VALUE 'R' 'P'.                             00006560
                    09 XX-TB-TAX-CLSFCTN-CD        PIC X(05).           00006570
                    09 XX-TB-TAX-JURIS-CD          PIC X(02).           00006580
                       88 XX-TB-TAX-JURIS-CD-VLD                        00006590
                             VALUE 'FE' 'ST' 'CN' 'CI' 'SC'.            00006600
                    09 XX-TB-TAX-RT                PIC SV9(5).          00006610
                    09 XX-TB-TAX-RT4                                    00006620
                          REDEFINES XX-TB-TAX-RT                        00006630
                                                   PIC S9V9(4).         00006640
                    09 XX-TB-TAX-RT3                                    00006650
                              REDEFINES XX-TB-TAX-RT                    00006660
                                                   PIC S99V9(3).        00006670
                    09 XX-TB-TAX-RSN-CD            PIC X(04).           00006680
                       88 XX-TB-TAX-RSN-CD-VLD                          00006690
                             VALUE 'STAT' 'RSAL' 'DRPP' 'FDGV'          00006700
                                   'OFSV' 'NGOV' 'TAXB' 'LCGV'.         00006710
                    09 XX-TB-TAX-AT                                     00006720
                                           COMP-3  PIC S9(9)V9(7).      00006730
                    09 XX-TB-MAXTAX-AT                                  00006740
                                           COMP-3  PIC S9(9)V9(4).      00006750
                    09 XX-TB-TAX-RT-TYPE           PIC X(1).            00006760
                       88 XX-TB-TAX-RT-TYPE-VLD                         00006770
                             VALUE 'R' 'B' 'E' 'C' 'S' ' '.             00006780
                    09 XX-TB-TAXABLE-AT                                 00006790
                                           COMP-3  PIC S9(9)V9(4).      00006800
                    09 XX-TB-NTAXABLE-AT                                00006810
                                           COMP-3  PIC S9(9)V9(4).      00006820
                    09 XX-TB-SRCHG-AO-AT                                00006830
                                           COMP-3  PIC S9(9)V9(4).      00006840
                    09 XX-TB-MAXTAX-ND             PIC X(01).           00006850
                       88 XX-TB-MAXTAX-ND-VLD                           00006860
                             VALUE 'L' 'I' 'T' 'B' ' '.                 00006870
                    09 XX-TB-GRS-TAX-AT                                 00006880
                                           COMP-3  PIC S9(9)V9(4).      00006890
                    09 XX-TB-RTRANS-DIST-RT                             00006900
                                           COMP-3  PIC SV9(5).          00006910
                    09 XX-TB-RTRANS-DIST-TAX                            00006920
                                           COMP-3  PIC S9(9)V9(4).      00006930
      *-------------------------------------------------------------.   00006940
      *                     E N D   O F  I J N L T I F              :   00006950
      *-------------------------------------------------------------'   00006960
