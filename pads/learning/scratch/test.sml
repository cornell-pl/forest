structure Tests = struct
    open Types
    open Model

    (* Some AuxInfo structures to use *)
    val a1 : AuxInfo = { coverage = 7
                       , label    = NONE
                       , typeComp = zeroComplexity
                       , dataComp = zeroComplexity
                       }

    (* Some tokens to use *)
    val tint1  : Token = Pint 1
    val tint2  : Token = Pint 2
    val tint3  : Token = Pint 3
    val tip1   : Token = Pip "100.034.176.202"
    val tip2   : Token = Pip "101.034.176.102"
    val tbxml1 : Token = PbXML ("<tag>junk1</tag>", "attribute1")
    val tbxml2 : Token = PbXML ("<tag>bigger junk2</tag>", "attribute2")
    val texml1 : Token = PeXML ("<tag>junk1</tag>", "attribute1")
    val texml2 : Token = PeXML ("<tag>bigger junk2</tag>", "attribute2")
    val ttime1 : Token = Ptime "12:12:12"
    val ttime2 : Token = Ptime "01:23:45"
    val ttime3 : Token = Ptime "12:34:56"
    val tstr1  : Token = Pstring "12:12:12"
    val tstr2  : Token = Pstring "HIGH THEIR"
    val tstr3  : Token = Pstring "Gupe nga lata"
    val tstr4  : Token = Pstring "Cant parse this (mj hammer)"
    val tgrp1  : Token = Pgroup { left = (tip1, ln1)
                                , body = [(tip2, ln2)]
                                , right = (tip1, ln1)
                                }
    val twht1  : Token = Pwhite "   "
    val twht2  : Token = Pwhite "\t\t\t   "
    val toth1  : Token = Other #"a"
    val toth2  : Token = Other #"b"
    val toth3  : Token = Other #"c"
    val temp1  : Token = Pempty
    val terr1  : Token = Error

    (* Some locations to use *)
    val ln1 : location = { lineNo = 1, beginloc = 10, endloc = 20 }
    val ln2 : location = { lineNo = 2, beginloc = 12, endloc = 22 }
    val ln3 : location = { lineNo = 3, beginloc = 13, endloc = 23 }
    val ln4 : location = { lineNo = 4, beginloc = 14, endloc = 24 }

    (* Some token lists to use *)
    val ltl1     : LToken list = [ ( tint1, ln1 ) ]
    val ltl2     : LToken list = [ ( tint1, ln1 ), ( tint2, ln2 ) ]
    val ltlip1   : LToken list = [ ( tip1, ln1 ) ]
    val ltlbxml1 : LToken list = [ ( tbxml1, ln1 ) ]
    val ltlbxml2 : LToken list = [ ( tbxml2, ln1 ) ]
    val ltlbxml3 : LToken list = [ ( tbxml1, ln1 ), ( tbxml2, ln2 ) ]
    val ltlexml1 : LToken list = [ ( texml1, ln1 ) ]
    val ltlexml2 : LToken list = [ ( texml2, ln1 ) ]
    val ltlexml3 : LToken list = [ ( texml1, ln1 ), ( texml2, ln2 ) ]
    val ltltime1 : LToken list = [ ( ttime1, ln1 ) ]
    val ltltime2 : LToken list = [ ( ttime2, ln1 ) ]
    val ltltime3 : LToken list = [ ( ttime1, ln1 ), ( ttime2, ln2 ), ( ttime3, ln3 ) ]
    val ltlstr4  : LToken list = [ ( tstr1, ln1 ), ( tstr2, ln2 )
                                 , ( tstr3, ln3 ), ( tstr4, ln4 )
                                 ]
    val ltlgrp1  : LToken list = [ ( tgrp1, ln1 ) ]
    val ltlwht2  : LToken list = [ ( twht1, ln1 ), ( twht2, ln2 ) ]
    val ltloth3  : LToken list = [ ( toth1, ln1 ), ( toth2, ln2 ), ( toth3, ln3 ) ]
    val ltlemp1  : LToken list = [ ( temp1, ln1 ) ]
    val ltlerr1  : LToken list = [ ( terr1, ln1 ) ]

    (* Some refined base types to use *)
    val rsme1  : Refined = StringME "one"
    val rsme2  : Refined = StringME "two"
    val rsme3  : Refined = StringME "three"
    val rint1  : Refined = Int ( 4, 7 )
    val rint2  : Refined = Int ( 444, 777 )
    val rintc1 : Refined = IntConst 83838383838
    val rstr1  : Refined = StringConst "abcdefgh"
    val renum1 : Refined = Enum [ rsme1, rint1, rstr1 ]
    val rlbl1  : Refined = LabelRef (Atom.atom "label")

    (* Some Base Ty structures to use *)
    val ty1  : Ty = Base (a1, [])
    val ty2  : Ty = Base (a1, ltl1)
    val ty3  : Ty = Base (a1, ltlip1)
    val ty4  : Ty = Base (a1, ltlbxml3)
    val ty5  : Ty = Base (a1, ltlexml3)
    val ty6  : Ty = Base (a1, ltltime3)
    val ty7  : Ty = Base (a1, ltlstr4)
    val ty8  : Ty = Base (a1, ltlgrp1)
    val ty9  : Ty = Base (a1, ltlwht2)
    val ty10 : Ty = Base (a1, ltloth3)
    val ty11 : Ty = Base (a1, ltlemp1)
    val ty12 : Ty = Base (a1, ltlerr1)
    (* Some RefinedBase Ty structures to use *)
    val ty20 : Ty = RefinedBase (a1, rsme1, ltlstr4)
    val ty21 : Ty = RefinedBase (a1, rint1, ltl2)
    val ty22 : Ty = RefinedBase (a1, rintc1, ltl2)
    val ty23 : Ty = RefinedBase (a1, rstr1, ltlstr4)
    val ty24 : Ty = RefinedBase (a1, renum1, ltlstr4)
    val ty25 : Ty = RefinedBase (a1, rlbl1, [])
    (* Now to test structured, but unrefined types *)
    val ty30 : Ty = Pstruct (a1, [ ty3, ty9, ty21 ])
    val ty31 : Ty = Punion (a1, [ ty3, ty9, ty21, ty10 ])
    val ty32 : Ty = Parray ( a1, { tokens  = []
                                 , lengths = [ ( 6, 12 ) ]
                                 , first   = ty3
                                 , body    = ty9
                                 , last    = ty10
                                 }
                           )
    (* Test refined structures *)
    val ty40 : Ty = RArray (a1, NONE, NONE, ty9, NONE )

    (* Carry out the measurements on the Ty structures *)
    val m1  : Ty = measure ty1;
    val m2  : Ty = measure ty2;
    val m3  : Ty = measure ty3;
    val m4  : Ty = measure ty4;
    val m5  : Ty = measure ty5;
    val m6  : Ty = measure ty6;
    val m7  : Ty = measure ty7;
    val m8  : Ty = measure ty8;
    val m9  : Ty = measure ty9;
    val m10 : Ty = measure ty10;
    val m11 : Ty = measure ty11;
    val m12 : Ty = measure ty12;

    val m20 : Ty = measure ty20;
    val m21 : Ty = measure ty21;
    val m22 : Ty = measure ty22;
    val m23 : Ty = measure ty23;
    val m24 : Ty = measure ty24;
    val m25 : Ty = measure ty25;

    val m30 : Ty = measure ty30;
    val m31 : Ty = measure ty31;
    val m32 : Ty = measure ty32;
    val (Parray (a32, s32)) = m32;

    val m40 : Ty = measure ty40;
    val (RArray (a40, osep40, oterm40, body40, olen40)) = m40;

end
