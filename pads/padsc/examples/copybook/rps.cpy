      ******************************************************************
      * COPY MEMBER  RP519                CREATED: 12/03/91 T01044 CJJ *
      *                                   CHANGED: 05/07/01 T10830 BEW *
      *                                                                *
      * DESC: SDN / ONENET RPS INTERFACE FILE              LRECL = 450 *
      *                                                                *
      ***  NOTE: ANY CHANGES TO RP519 MUST BE MADE TO COPYBOOK RP519A  *
      *********** CHANGE HISTORY ***************************************
      * 05/07/01     T10830     ADDED RP519-HE-ACCT-IND                *
      * 09/26/00     T10230 CR  CHANGED RP519-GLOB-VEN-CODE TO         *
      *              CR0926     RP519-GV-IND-CODE                      *
      * 05/09/00     T01030 PG  ADDED RP519-GLOB-VEN-CODE,             *
      *              PG0509     RP519-GV-NOVATED-IND AND CHANGED       *
      *                         FILLER TO 8 BYTES. CHANGED             *
      *                         PLUSS-CKT-NUM TO BREAK OUT CHANNEL-ID  *
      *                         AND MCN-IND.                           *
      * 09/17/99     T00130 CAR ADDED NEW FIELDS- VOL-QTY AND VOL-TYPE *00000090
      *                                                                *
      * 06/04/99     T91030 CAR RENAMED RP519-TO-CNTRY-CD              *00000090
      *                         NOW     RP519-FROM-CNTRY-CD            *00000090
      *                         RP519-CNTRY-CD                         *
      *                         (TO) REFLECTS "DESTINATION"            *
      *                                                                *
      * 01/07/99     30716      LGH  ADDED CUST SEG DB AND CR FIELDS   *00000090
      * 12/23/98     30716      LGH  ADDED FIELDS AT END OF RECORD     *00000090
      *                                                                *
      * 08/30/96  T61230  PG                                           *
      * -  ADDED NEW SALES CHANNEL FIELD                               *
      * -  CHANGED NEW WIRELESS EXTRACT TYPE 'ORX' FIELD NAME          *
      *                                                                *
      * 08/12/96  T61230  ALW                                          *
      * -  ADDED NEW WIRELESS INDICATOR CODE 88 LEVEL 'W'              *
      * -  ADDED NEW WIRELESS REASON CODE 88 LEVEL 'RX'                *
      * -  ADDED NEW WIRELESS EXTRACT TYPE 'ORX' ALL CHANGES ARE FOR   *
      *    ONENET WIRELESS BUNDLE/RELEASE 96.4.                        *
      *                                                                *
      * 06/04/96  T60830  GH                                           *
      * -  ADDED NEW TYPE III INDICATOR 88 LEVEL 'LOCAL' AND ADDED     *
      *    NEW SOURCE OF CHARGE 88 LEVEL 'KRP'. BOTH CHANGES ARE FOR   *
      *    GLOBAL/LOCAL RELEASE 96.3.                                  *
      *                                                                *
      * 06/03/96  T60830  JMW                                          *
      * -  ADDED 88 LEVEL FOR REASON CODES LM, LN, LO, LP              *
      *                                                                *
      * 11/09/94  T50330  WYH                                          *00001100
      * -  ADDED 2 BYTE TP-ID AND 1 BYTE INBOUND-IND AT END OF RECORD. *00001100
      *                                                                *00001100
      * 03/22/94 KAT                                                   *00001000
      * -  ADDED NEW FACCTS, AND 88 LEVELS FOR BOTH        T40620 KAT  *00001200
      *    CR & DB FACCTS.                                             *00001300
      *                                                                *
      * -  ADDED ONE NET EXTRACT TYPES AND REASON          T40330 HTH  *
      *    CODES.                                                      *
      *                                                                *
      * -  REMOVED SVC-TYPE, ACC-TYPE-CALL-CNT,            T30430 CM   *
      *    FEATR-SUB-TYPE-CD, FEATR-TYPE-CD, AND                       *
      *    ACC-TYPE-MINUTE-CNT, ORIG-SEG-TYPE-CD                       *
      *                                                                *
      * -  ALSO REMOVED CKT AND CKL RECORDS                            *
      *                                                                *
      * -  ADDED SVC-TYPE, ACC-TYPE-CALL-CNT,               T30430 CM  *
      *    FEATR-SUB-TYPE-CD, FEATR-TYPE-CD, AND                       *
      *    ACC-TYPE-MINUTE-CNT, ORIG-SEG-TYPE-CD                       *
      *                                                                *
      * -  CORRECTED LENGTH OF FILLER TO X(76)              T30430 DL  *
      *                                                                *
      * -  PLACED TERMNT-LOCTN-TXT INTO TBL                 T30430 DL  *
      *                                                                *
      * -  ADDED REC-TYPES & TTACS DATA                     T30430 DL  *
      *                                                                *
      * -  REMOVED RP519-VPP-FID                            T01132 JLS *
      *    ADDED ADJMT-PG-CD                                           *
      *    ADDED GOVT-ACCT-MGR-CD                                      *
      *                                                                *
      * -  CHANGED ACC-TYPE-CD FROM X(3) TO X(2)            T01132 JLS *
      *                                                                *
      * 06/10/99  T91030  GS                                           *
      * -  ADDED   RP519-TAX-TYPE   X(5)                    T91030 GS  *
      * -  CHANGED FILLER FROM PIC X(24) TO PIC X(19)       T91030 GS  *
      ******************************************************************

       01  RP519-RECORD.                                                      00
         03  RP519-RPS-REC.
           05  RP519-REC-TYPE-CD             PIC X(01).
               88  RP519-RPS-TYPE                VALUE '1'.
           05  RP519-GDG-NO                  PIC 9(03) VALUE ZERO.
           05  RP519-EXTRACT-GDG-NO          PIC 9(04) VALUE ZERO.            00
           05  RP519-CBS-COPY-NUM            PIC X(02) VALUE SPACES.          00
           05  RP519-EXTRACT-TYPE.                                            00
               88  RP519-UNBILLED-FILE       VALUE 'UR ' 'UN '                00
                                                   'OR ' 'ON '.               00
               88  RP519-REVERSAL-FILE       VALUE 'IRR' 'URR' 'UNR'          00
                                                   'OAR' 'ORR' 'ONR'.         00
               88  RP519-UNBILLED            VALUE 'UN ' 'UR '                00
                                                   'ON ' 'OR '.               00
               88  RP519-UNB-RECURR          VALUE 'UR ' 'OR'.                00
               88  RP519-UNB-NONRECUR        VALUE 'UN ' 'ON'.                00
               88  RP519-INVC                VALUE 'IIN' 'IIR' 'IIP'          00
                                                   'IIU' 'IFX' 'ITX'          00
                                                   'ISU' 'IGR'                00
                                                   'OIN' 'OIR' 'OIP'          00
                                                   'OIU' 'OFX' 'OTX'          00
                                                   'OSU' 'OGR'.               00
               88  RP519-EXT-PSC-TYPE        VALUE 'IIN' 'IIR' 'IIP'          00
                                                   'IIU' 'IRR' 'UN '          00
                                                   'UNR' 'UR ' 'URR'          00
                                                   'MIU' 'IMC' 'IMD'          00
                                                   'IVC' 'IVD'                00
                                                   'OIN' 'OIR' 'OIP'          00
                                                   'OIU' 'OAR' 'ON '          00
                                                   'ONR' 'OR ' 'ORR'          00
                                                   'OIU' 'OMC' 'OMD'          00
                                                   'OVC' 'OVD'.               00
               88  RP519-INVC-RECURR         VALUE 'IIR' 'OIR'.               00
               88  RP519-WEFOS               VALUE 'OXT'.                     00
               88  RP519-INV-ROAM-TAX-ORX    VALUE 'ORX'.                     00
               88  RP519-INVC-NONRECUR       VALUE 'IIN' 'OIN'.               00
               88  RP519-INV-USAGE           VALUE 'IIU' 'OIU'.               00
               88  RP519-INV-PART-PERIOD-CHRGS                                00
                                             VALUE 'IIP' 'OIP'.               00
               88  RP519-INV-FET-TAX-IFX VALUE 'IFX' 'OFX'.                   00
               88  RP519-INV-LOC-TAX-ITX VALUE 'ITX' 'OTX'.                   00
               88  RP519-INV-SUR-TAX-ISU VALUE 'ISU' 'OSU'.                   00
               88  RP519-INV-GRS-TAX-IGR VALUE 'IGR' 'OGR'.                   00
               88  RP519-EXT-ALL-TAXES    VALUE 'IGR' 'IFX' 'ITX' 'ISU'       00
                                                'OGR' 'OFX' 'OTX' 'OSU'.      00
               88  RP519-MSG-WRT-OFF-MIU VALUE 'MIU' 'OIU'.                   00
               88  RP519-CR-ADJMT            VALUE 'IMC' 'IVD'                00
                                                   'OMC' 'OVD'.               00
               88  RP519-DB-ADJMT            VALUE 'IMD' 'IVC'                00
                                                   'OMD' 'OVC'.               00
               88  RP519-CR-JRNL             VALUE 'IJC' 'OJC'.               00
               88  RP519-DB-JRNL             VALUE 'IJD' 'OJD'.               00
               88  RP519-EXT-REJECT          VALUE 'TRL'.                     00
               88  RP519-REVERSAL            VALUE 'IRR' 'URR' 'UNR'          00
                                                   'OAR' 'ORR' 'ONR'.         00
               88  RP519-RVRS-UNBILLED       VALUE 'URR' 'UNR'                00
                                                   'ORR' 'ONR'.               00
               88  RP519-RVRS-UNB-RECURR     VALUE 'URR' 'ORR'.               00
               88  RP519-RVRS-UNB-NONRECUR   VALUE 'UNR' 'ONR'.               00
               88  RP519-RVRS-UNEARNED       VALUE 'IRR' 'OAR'.               00
               88  RP519-ADVANCE-PAYMENTS    VALUE 'IPD' 'OPD'.               00
               88  RP519-SECURITY-DEPOSIT    VALUE 'IPS' 'OPS'.               00
               88  RP519-POST-SUSP-PAYMT     VALUE 'ISI' 'OSI'.               00
               88  RP519-UNB-OR-RVRS-UNB     VALUE 'UR ' 'URR' 'UN '          00
                                                   'UNR'                      00
                                                   'OR ' 'ORR' 'ON '          00
                                                   'ONR'.                     00
               10  FILLER                    PIC X(01) VALUE SPACES.          00
               10  RP519-EXTR-TYPE-POS23     PIC X(02) VALUE SPACES.          00
           05  RP519-EXTR-TYPE REDEFINES RP519-EXTRACT-TYPE.                  00
               10  RP519-EXTR-TYPE-POS12     PIC X(02).                       00
                   88  RP519-PAYMENT         VALUE 'IP' 'OP'.                 00
                   88  RP519-ADJUST          VALUE 'IM' 'IV' 'IJ'             00
                                                   'OM' 'OV' 'OJ'.            00
                   88  RP519-MEMO            VALUE 'IM' 'OM'.                 00
                   88  RP519-JOURNAL         VALUE 'IJ' 'OJ'.                 00
                   88  RP519-VOID            VALUE 'IV' 'OV'.                 00
                   88  RP519-MEMO-OR-VOID    VALUE 'IM' 'IV' 'OM' 'OV'.       00
               10  FILLER                    PIC X(01).                       00
           05  RP519-EXTRACT-TYPE1 REDEFINES RP519-EXTRACT-TYPE.              00
               10  RP519-EXTRACT-POS11       PIC X(01).                       00
                   88  RP519-ONENET          VALUE 'O'.                       00
                   88  RP519-SDN-REVENUE     VALUE 'I'.                       00
                   88  RP519-SDN-UNBILLED    VALUE 'U'.                       00
               10  FILLER                    PIC X(02).                       00
           05  RP519-EFFECTIVE-DATE          PIC X(06) VALUE SPACES.          00
           05  RP519-BL-TO-CD                PIC X(11) VALUE SPACES.          00
           05  RP519-INVC-NUM                PIC X(10) VALUE SPACES.          00
           05  RP519-ORIGNL-INVC-NUM         PIC X(10) VALUE SPACES.          00
               88  RP519-DUMMY               VALUE '7777777777'               00
                                                   '88888888@@'               00
                                                   '9999999INV'.              00
               88  RP519-SEVENS-INVC         VALUE '7777777777'.              00
               88  RP519-EIGHTS-INVC         VALUE '88888888@@'.              00
               88  RP519-NINES-INVC          VALUE '9999999INV'.              00
           05  RP519-ADJ-RSN-CD              PIC X(02) VALUE SPACES.          00
               88  RP519-FED-EXC-TAXES       VALUE 'FX'.                      00
               88  RP519-TAX                 VALUE 'TX'.                      00
               88  RP519-ROAM-TAX            VALUE 'RX'.                      00
               88  RP519-TAX-ADJMT           VALUE 'FX' 'TX' 'ZF' 'ZT'.       00
               88  RP519-CO-OFF-JOURNAL      VALUE 'ZN' 'ZR' 'ZT'             00
                                                   'ZF' 'QS' 'QU'             00
                                                   'QP'.                      00
               88  RP519-UNCLCTBL-UN1        VALUE 'W1' 'W2' 'W3' 'W4'        00
                                                   'W5' 'W6' 'W7' 'W8'.       00
               88  RP519-UNCLCTBL-UN3        VALUE 'B1' 'B2' 'B3' 'B4'        00
                                                   'B6' 'B7' 'B8' 'B9'.       00
               88  RP519-UNCLCTBL-UN5        VALUE 'E1' 'E2' 'E3' 'E4'        00
                                                   'E5' 'E6' 'E7' 'E8'        00
                                                   'E9' 'E0' 'OB' 'IB'.       00
               88  RP519-UNCLCTBL-UN7        VALUE 'SF' 'FS'.                 00
               88  RP519-OUTBOUND-PSC-RSN    VALUE 'BE' 'CA' 'RR' 'SA'        00
                                                   'S1' 'S2' 'S3' 'ZN'        00
                                                   'ZR' '02' '04' '08'        00
                                                   '11' '12' '13' '14'        00
                                                   '31' '70' '71' '72'        00
                                                   '73' '74' '75' '76'        00
                                                   '77' '78'.                 00
               88  RP519-OUTBOUND-PSC-TAX1   VALUE 'FE' 'ZE'.           01531002
               88  RP519-OUTBOUND-PSC-TAX2   VALUE 'SU'.                01532003
               88  RP519-INBOUND-PSC-RSN     VALUE 'B5' 'RB' 'SI' '1S'        00
                                                   '2S' '3S' '22' '23'        00
                                                   '24' '80' '81' '82'        00
                                                   '83' '84' '85' '86'        00
                                                   '87' '88' '91' '92'        00
                                                   '93' '94' '95'.            00
               88  RP519-INBOUND-PSC-TAX     VALUE 'EF' 'S8'.           01591002
               88  RP519-SPEC-PSC-RSN        VALUE 'BE' 'CA' 'RR' 'SA'        00
                                                   'S1' 'S2' 'S3' 'ZN'        00
                                                   'ZR' '02' '04' '08'        00
                                                   '11' '12' '13' '14'        00
                                                   '31' '70' '71' '72'        00
                                                   '73' '74' '75' '76'        00
                                                   '77' '78'                  00
                                                   'FE' 'SU' 'ZE'       01661000
                                                   'B5' 'RB' 'SI' '1S'        00
                                                   '2S' '3S' '22' '23'        00
                                                   '24' '80' '81' '82'        00
                                                   '83' '84' '85' '86'        00
                                                   '87' '88' '91' '92'        00
                                                   '93' '94' '95'       01720001
                                                   'EF' 'S8'.           01721001
               88  RP519-UNCLTBL-PSC-RSN     VALUE 'B6' 'B7' 'B8' 'B9'        00
                                                   'E0' 'E6' 'E7' 'E8'        00
                                                   'E9' 'IB' 'W5' 'W6'        00
                                                   'W7' 'W8'.                 00
               88  RP519-LOCAL-RSN           VALUE 'LM' 'LN' 'LO' 'LP'.       00
           05  RP519-DBCR-CD                 PIC X(02) VALUE SPACES.          00
               88  RP519-CR-CODE             VALUE '60' '61'.                 00
               88  RP519-DB-CODE             VALUE '10' '11'.                 00
           05  RP519-CUR-AMT                 PIC 9(11)V99 VALUE ZEROES.       00
           05  RP519-UNER-AMT                PIC 9(11)V99 VALUE ZEROES.       00
           05  RP519-PE-REV-AMT              PIC S9(12)V99 VALUE ZERO.        00
           05  RP519-CR-FACCT.                                                00
               10  RP519-CR-FACCT-8-BYTE.                                     00
                   88  RP519-ACCT-RCVBL-FACCT-CR                        01890007
                               VALUE '11111010', '11111020', '11111030'.01900007
                   88  RP519-UNER-FACCT-CR             VALUE '22202111'.01910007
                   88  RP519-EXPENSE-FACCT-CR          VALUE 'EXPENSE '.01920007
                   88  RP519-DFLT-EXP-FACCT-CR         VALUE '55114435'.01930007
                   88  RP519-LCKBX-FACCT-CR            VALUE '82201120'.01940007
                   88  RP519-INT-RCVBL-FACCT-CR        VALUE '11211000'.01941007
                   88  RP519-CASH-CLRG-FACCT-CR        VALUE '10106000'.01942007
                   15  RP519-CR-FACCT-1      PIC X(03) VALUE SPACES.          00
                   15  RP519-CR-FACCT-2      PIC X(02) VALUE SPACES.          00
                   15  RP519-CR-FACCT-3      PIC X(02) VALUE SPACES.          00
                   15  RP519-CR-FACCT-4      PIC X(01) VALUE SPACES.          00
               10  RP519-CR-FACCT-SPACES     PIC X(02) VALUE SPACES.          00
           05  RP519-CR-CSUB.                                                 00
               10  RP519-CR-CSUB-1           PIC X(01) VALUE SPACES.          00
               10  RP519-CR-CSUB-2           PIC X(02) VALUE SPACES.          00
               10  RP519-CR-CSUB-3           PIC X(02) VALUE SPACES.          00
           05  RP519-DB-FACCT.                                                00
               10  RP519-DB-FACCT-8-BYTE.                                     00
                   88  RP519-ACCT-RCVBL-FACCT-DB                        02101007
                               VALUE '11111010', '11111020', '11111030'.02102007
                   88  RP519-UNER-FACCT-DB             VALUE '22202111'.02103007
                   88  RP519-EXPENSE-FACCT-DB          VALUE 'EXPENSE '.02104007
                   88  RP519-DFLT-EXP-FACCT-DB         VALUE '55114435'.02105007
                   88  RP519-LCKBX-FACCT-DB            VALUE '82201120'.02106007
                   88  RP519-INT-RCVBL-FACCT-DB        VALUE '11211000'.02107007
                   88  RP519-CASH-CLRG-FACCT-DB        VALUE '10106000'.02108007
                   15  RP519-DB-FACCT-1      PIC X(03) VALUE SPACES.          00
                   15  RP519-DB-FACCT-2      PIC X(02) VALUE SPACES.          00
                   15  RP519-DB-FACCT-3      PIC X(02) VALUE SPACES.          00
                   15  RP519-DB-FACCT-4      PIC X(01) VALUE SPACES.          00
               10  RP519-DB-FACCT-SPACES     PIC X(02) VALUE SPACES.          00
           05  RP519-DB-CSUB.                                                 00
               10  RP519-DB-CSUB-1           PIC X(01) VALUE SPACES.          00
               10  RP519-DB-CSUB-2           PIC X(02) VALUE SPACES.          00
               10  RP519-DB-CSUB-3           PIC X(02) VALUE SPACES.          00
           05  RP519-UNER-FACCT.                                              00
               10  RP519-UN-FACCT-8-BYTE.                                     00
                   15  RP519-UN-FACCT-1      PIC X(03) VALUE SPACES.          00
                   15  RP519-UN-FACCT-2      PIC X(02) VALUE SPACES.          00
                   15  RP519-UN-FACCT-3      PIC X(02) VALUE SPACES.          00
                   15  RP519-UN-FACCT-4      PIC X(01) VALUE SPACES.          00
               10  RP519-UN-FACCT-SPACES     PIC X(02) VALUE SPACES.          00
           05  RP519-UNER-CSUB.                                               00
               10  RP519-UN-CSUB-1           PIC X(01) VALUE SPACES.          00
               10  RP519-UN-CSUB-2           PIC X(02) VALUE SPACES.          00
               10  RP519-UN-CSUB-3           PIC X(02) VALUE SPACES.          00
           05  RP519-BILLER-ID               PIC X(01) VALUE SPACES.          00
               88  RP519-SDN                 VALUE 'S'.                       00
               88  RP519-UNKNOWN             VALUE 'U'.                       00
           05  RP519-BILLER-STATE-CD         PIC X(02) VALUE SPACES.          00
           05  RP519-SOURCE-OF-CHRG          PIC X(01) VALUE SPACES.          00
               88  RP519-BILLER-GSDN         VALUE '0'.                       00
               88  RP519-BILLER-HNS          VALUE '4'.                       00
               88  RP519-BILLER-SDN          VALUE '5'.                       00
               88  RP519-BILLER-ISDN         VALUE 'J'.                       00
               88  RP519-BILLER-SCS          VALUE 'K'.                       00
               88  RP519-BILLER-SOC          VALUE 'L'.                       00
               88  RP519-BILLER-SDS          VALUE 'M'.                       00
               88  RP519-BILLER-GIN          VALUE 'N'.                       00
               88  RP519-BILLER-KRP          VALUE 'R'.
               88  RP519-BILLER-CCS          VALUE 'S'.
               88  RP519-BILLER-IS-USAGE     VALUE '0' '4' '5' 'J' 'K'        00
                                                   'L' 'M' 'N' 'S' 'R'.
           05  RP519-USER-ENTERED-STATE      PIC X(02) VALUE SPACES.          00
           05  RP519-III-IND                 PIC X(01) VALUE SPACES.          00
               88  RP519-III-INTRASTATE      VALUE '0'.                       00
               88  RP519-III-INTERSTATE      VALUE '1'.                       00
               88  RP519-III-OFFSHORE        VALUE '2'.                       00
               88  RP519-III-OVERSEAS        VALUE '3'.                       00
               88  RP519-III-LOCAL           VALUE '6'.
           05  RP519-TYPE-SVC                PIC X(03) VALUE SPACES.          00
           05  RP519-CLASS-SVC               PIC X(05) VALUE SPACES.          00
           05  RP519-USOC-CD                 PIC X(05) VALUE SPACES.          00
           05  RP519-ARREARS-BILLING-IND     PIC X(01) VALUE SPACES.          00
               88  RP519-ADVANCE-BILLING     VALUE 'F'.                       00
               88  RP519-ARREARS-BILLING     VALUE 'A'.                       00
           05  RP519-DISC-PLAN               PIC X(01) VALUE SPACES.          00
           05  RP519-MIU-IND                 PIC X(01) VALUE SPACES.          00
               88  RP519-MIU-WRITE-OFF       VALUE 'W'.                       00
           05  RP519-ADJ-IND                 PIC X(01) VALUE SPACES.          00
               88  RP519-ADJ                 VALUE 'Y'.                       00
               88  RP519-NON-ADJ             VALUE 'N'.                       00
           05  RP519-BRANCH-OFFICE-CDE.                                 00025300
               10  RP519-BR-CD-POS-1-5       PIC X(05).                 00025310
                   88  RP519-WRLDSRCE   VALUE 'USV01' 'US222' 'US233'.  02650008
               10  FILLER                    PIC X(03).                 00027410
           05  RP519-ORIG-START-DATE.                                         00
               10  RP519-ORIG-START-YR-DTE   PIC X(02) VALUE SPACES.          00
               10  RP519-ORIG-START-MO-DTE   PIC X(02) VALUE SPACES.          00
               10  RP519-ORIG-START-DAY-DTE  PIC X(02) VALUE SPACES.          00
           05  RP519-ORIG-INVC-DT            PIC X(06) VALUE SPACES.          00
           05  RP519-PBS-GENERATED           PIC X(01) VALUE SPACES.          00
               88  RP519-PBS-GEN-TRANS       VALUE 'Y'.                       00
               88  RP519-NORMAL-TRANS        VALUE 'N'.                       00
           05  RP519-JRNLZD-FLG              PIC X(01) VALUE SPACES.          00
               88  RP519-JRNLZD              VALUE 'Y'.                       00
           05  RP519-OMIT-BILL-FLG           PIC X(01) VALUE SPACES.          00
               88  RP519-OMIT-BILL           VALUE 'Y'.                       00
               88  RP519-INTRNL-CUST         VALUE 'I'.
           05  RP519-TP-FROM-CD              PIC X(01) VALUE SPACES.          00
           05  RP519-DB-STUDY-CD             PIC X(04) VALUE SPACES.          00
           05  RP519-CR-STUDY-CD             PIC X(04) VALUE SPACES.          00
           05  RP519-UNER-STUDY-CD           PIC X(04) VALUE SPACES.          00
           05  RP519-ADJMT-PG-CD             PIC X(01) VALUE SPACES.          00
           05  RP519-SALES-CHANNEL           PIC X(08) VALUE SPACES.          00
           05  RP519-TRM-AGRE-PLN-TYPE       PIC X(08) VALUE SPACES.          00
           05  RP519-TRM-AGRE-STRT-DTE.                                       00
               10  RP519-TRM-AGRE-STRT-YY    PIC X(02) VALUE SPACES.          00
               10  RP519-TRM-AGRE-STRT-MM    PIC X(02) VALUE SPACES.          00
               10  RP519-TRM-AGRE-STRT-DD    PIC X(02) VALUE SPACES.          00
           05  RP519-PRIVATE-LINE-ID.                                         00
               10  RP519-SALES-OFFC-CD       PIC X(02) VALUE SPACES.          00
               10  RP519-MCN-BASE            PIC X(06) VALUE SPACES.          00
               10  RP519-MCN-SUFFIX          PIC X(03) VALUE SPACES.          00
               10  RP519-BILL-GRP-NBR        PIC X(03) VALUE SPACES.          00
               10  RP519-SALES-GRP-CD        PIC X(02) VALUE SPACES.          00
           05  RP519-PLUSS-CKT-NUM.
               10  FILLER                    PIC X(22) VALUE SPACES.
               10  RP519-CKT-CHANNEL-ID      PIC X(01) VALUE SPACE.
               10  RP519-CKT-MCN-IND         PIC X(01) VALUE SPACE.
           05  RP519-JE.                                                      00
               10  RP519-JE-FIRST            PIC X(04) VALUE SPACES.          00
               10  RP519-JE-MIDDLE           PIC X(01) VALUE SPACES.          00
               10  RP519-JE-LAST             PIC X(01) VALUE SPACES.          00
           05  RP519-ME                      PIC X(04) VALUE SPACES.          00
           05  RP519-DB-IET.                                                  00
               10  RP519-DB-IET-ME           PIC X(04) VALUE SPACES.          00
               10  RP519-DB-IET-LE           PIC X(03) VALUE SPACES.          00
               10  RP519-DB-IET-AE           PIC X(03) VALUE SPACES.          00
           05  RP519-CR-IET.                                                  00
               10  RP519-CR-IET-ME           PIC X(04) VALUE SPACES.          00
               10  RP519-CR-IET-LE           PIC X(03) VALUE SPACES.          00
               10  RP519-CR-IET-AE           PIC X(03) VALUE SPACES.          00
           05  RP519-CNTRY-CD                PIC X(03) VALUE SPACES.          00
           05  RP519-DB-PROD-CD              PIC X(10) VALUE SPACES.          00
           05  RP519-CR-PROD-CD              PIC X(10) VALUE SPACES.          00
           05  RP519-DB-CUSTOMER-CD          PIC X(11) VALUE SPACES.          00
           05  RP519-CR-CUSTOMER-CD          PIC X(11) VALUE SPACES.          00
           05  RP519-ORG-CD-CHARGED          PIC X(09) VALUE SPACES.          00
           05  RP519-DB-ORG-CD-ORIG          PIC X(09) VALUE SPACES.          00
           05  RP519-CR-ORG-CD-ORIG          PIC X(09) VALUE SPACES.          00
           05  RP519-HOME-STE-TAX-IND        PIC X(01) VALUE SPACES.          00
           05  RP519-ACC-TYPE-CD             PIC X(02) VALUE SPACES.
           05  RP519-FILE-IND                PIC X(01) VALUE SPACES.          00
               88  RP519-SELECT-FILE         VALUE 'S'.                       00
               88  RP519-INTERFACE-FILE      VALUE 'I'.                       00
           05  RP519-ERROR-CODE              PIC X(02) VALUE SPACES.          00
           05  RP519-BILL-CYCLE              PIC X(02) VALUE SPACES.          00
           05  RP519-INBOUND-IND             PIC X(01) VALUE SPACES.          00
               88  RP519-INBOUND             VALUE 'Y'.
               88  RP519-NOT-INBOUND         VALUE 'N' ' '.
           05  RP519-TP-ID                   PIC X(02) VALUE SPACES.          00
           05  RP519-SRCE-SVC-CD             PIC X(01) VALUE SPACES.          00
               88  RP519-WIRELESS-SVC           VALUE 'W'.
           05  RP519-CUST-SEG-CD-CR          PIC X(10)  VALUE SPACES.         00
           05  RP519-CALL-TYPE-CD            PIC X(03)  VALUE SPACES.         00
           05  RP519-CALL-CNT                PIC S9(09) VALUE +0 COMP-3.      00
           05  RP519-ELPSD-TM                PIC S9(12) VALUE +0 COMP-3.      00
           05  RP519-FROM-CNTRY-CD           PIC X(02)  VALUE SPACES.         00
           05  RP519-CUST-SEG-CD-DB          PIC X(10)  VALUE SPACES.         00
           05  RP519-LEAD-ACCOUNT            PIC X(13)  VALUE SPACES.
           05  RP519-TAX-TYPE                PIC X(05)  VALUE SPACES.
           05  RP519-VOL-QTY                 PIC S9(7)V99 VALUE +0
                                                          COMP-3.
           05  RP519-VOL-TYPE                PIC X(04)  VALUE SPACES.
           05  RP519-GV-IND-CODE             PIC X(01)  VALUE SPACES.
               88  RP519-88-GV-NOVATED           VALUES 'K' 'N'.
               88  RP519-88-GV-QUALIFIED         VALUES 'I' 'J' 'L' 'M'.
               88  RP519-88-GV-VALID             VALUES 'I' 'J' 'K' 'L'
                                                        'M' 'N'.
           05  RP519-GV-NOVATED-IND          PIC X(01)  VALUE SPACES.
               88  RP519-88-NOVATED-ACCT         VALUE 'Y'.
           05  RP519-HE-ACCT-IND             PIC X(01)  VALUE SPACES.
               88  RP519-88-MBU-ACCOUNT   VALUE 'M'.
               88  RP519-88-SBU-ACCOUNT   VALUE 'S'.
               88  RP519-88-BU-ACCOUNT    VALUE 'B'.
           05  FILLER                        PIC X(07)  VALUE SPACES.
                                                                        00000200
      ***************************************************************** 00000100
      *  END OF RP519 COPY BOOK                                         00000200
      ***************************************************************** 00000100
