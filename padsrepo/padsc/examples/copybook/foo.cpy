000010* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
000020*    RELEASE 9.4.6 CRS DISCOUNT FILE DETAIL RECORD            *
000030*    MEMBER CRSDET   CREATED 09-09-93   RELEASE 9.3.12        *
000040*    LAST UPDATE DATE     PERSON        RELEASE     LENGTH    *
000050*       93/09/29          S.ACHARYA      9.3.12       147     *
000060*       94/03/01          S.ACHARYA      9.4.6        114     *
000070*       94/10/20          S.ACHARYA      9.4.12        74     *
000080*       96/02/14          F.VELASQUEZ    9.6.2         86     *
000090* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
000100 01  XX-CRS-RECORD.
000110     05 XX-LEAD-ACCT-NUM            PIC X(13)     VALUE SPACES.
000120     05 XX-SUB-ACCT-NUM             PIC X(13)     VALUE SPACES.
000130     05 XX-TOTALS-BY-JURISD.
000140        10 XX-DOM-GROSS-AND-DISCS.
000150           15 XX-DOM-GROSS-USAGE    PIC S9(9)V99 COMP-3
000160                                                 VALUE ZEROES.
000170           15 XX-DOM-DISC-AMT       PIC S9(9)V99 COMP-3
000180                                                 VALUE ZEROES.
000190        10 XX-CAN-GROSS-AND-DISCS.
000200           15 XX-CAN-GROSS-USAGE    PIC S9(9)V99 COMP-3
000210                                                 VALUE ZEROES.
000220           15 XX-CAN-DISC-AMT       PIC S9(9)V99 COMP-3
000230                                                 VALUE ZEROES.
000240        10 XX-OVS-GROSS-AND-DISCS.
000250           15 XX-OVS-GROSS-USAGE    PIC S9(9)V99 COMP-3
000260                                                 VALUE ZEROES.
000270           15 XX-OVS-DISC-AMT       PIC S9(9)V99 COMP-3
000280                                                 VALUE ZEROES.
000290        10 XX-MEX-GROSS-AND-DISCS.
000300           15 XX-MEX-GROSS-USAGE    PIC S9(9)V99 COMP-3
000310                                                 VALUE ZEROES.
000320           15 XX-MEX-DISC-AMT       PIC S9(9)V99 COMP-3
000330                                                 VALUE ZEROES.
000340        10 XX-LOC-GROSS-AND-DISCS.
000350           15 XX-LOC-GROSS-USAGE    PIC S9(9)V99 COMP-3
000360                                                 VALUE ZEROES.
000370           15 XX-LOC-DISC-AMT       PIC S9(9)V99 COMP-3
000380                                                 VALUE ZEROES.
000390     05 XX-TOTALS-BY-JURISD-RD REDEFINES
000400        XX-TOTALS-BY-JURISD.
000410        10 XX-GROSS-AND-DISCS OCCURS 2 TIMES.
000420           15 XX-GROSS-USAGE        PIC S9(9)V99 COMP-3.
000430           15 XX-DISC-AMT           PIC S9(9)V99 COMP-3.
000410        10 XX-GROSS-AND-DISCS OCCURS 3 TIMES.
000420           15 XX-GROSS-USAGE        PIC S9(9)V99 COMP-3.
000430           15 XX-DISC-AMT OCCURS 5 TIMES PIC S9(9)V99 COMP-3.
000440* * * * * * * * * * *  END OF RECORD  * * * * * * * * * * * * *
