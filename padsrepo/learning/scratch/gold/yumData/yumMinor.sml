structure YumMinor = struct
    open Tokens

    fun mkPhonyLoc ( ln : int ) : location = mkLoc 0 0 1 ln
    val minorTokens : LToken list =
    [
      ( Pstring "66.13", mkPhonyLoc 1)
    , ( Pstring "43.4", mkPhonyLoc 2)
    , ( Pstring "4", mkPhonyLoc 3)
    , ( Pstring "0.40E", mkPhonyLoc 4)
    , ( Pstring "16.EL4.12", mkPhonyLoc 5)
    , ( Pstring "22.0.1.EL", mkPhonyLoc 6)
    , ( Pstring "58.RHEL4", mkPhonyLoc 7)
    , ( Pstring "10", mkPhonyLoc 8)
    , ( Pstring "8", mkPhonyLoc 9)
    , ( Pstring "1.2", mkPhonyLoc 10)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 11)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 12)
    , ( Pstring "12.2.EL4", mkPhonyLoc 13)
    , ( Pstring "17", mkPhonyLoc 14)
    , ( Pstring "43.4", mkPhonyLoc 15)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 16)
    , ( Pstring "3.RHEL4.1", mkPhonyLoc 17)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 18)
    , ( Pstring "2", mkPhonyLoc 19)
    , ( Pstring "1.EL.13.20", mkPhonyLoc 20)
    , ( Pstring "13", mkPhonyLoc 21)
    , ( Pstring "4.RHEL4.1", mkPhonyLoc 22)
    , ( Pstring "2", mkPhonyLoc 23)
    , ( Pstring "3.1", mkPhonyLoc 24)
    , ( Pstring "2", mkPhonyLoc 25)
    , ( Pstring "12.2.EL4", mkPhonyLoc 26)
    , ( Pstring "17", mkPhonyLoc 27)
    , ( Pstring "1.2", mkPhonyLoc 28)
    , ( Pstring "43.4", mkPhonyLoc 29)
    , ( Pstring "19.EL.1", mkPhonyLoc 30)
    , ( Pstring "1.el4.2", mkPhonyLoc 31)
    , ( Pstring "24", mkPhonyLoc 32)
    , ( Pstring "4.EL4.1", mkPhonyLoc 33)
    , ( Pstring "1", mkPhonyLoc 34)
    , ( Pstring "3", mkPhonyLoc 35)
    , ( Pstring "14.1.EL", mkPhonyLoc 36)
    , ( Pstring "1.EL.13.20", mkPhonyLoc 37)
    , ( Pstring "2", mkPhonyLoc 38)
    , ( Pstring "1.EL.1", mkPhonyLoc 39)
    , ( Pstring "4.RHEL4.1", mkPhonyLoc 40)
    , ( Pstring "4.RHEL4.1", mkPhonyLoc 41)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 42)
    , ( Pstring "3.RHEL4.1", mkPhonyLoc 43)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 44)
    , ( Pstring "3.RHEL4.1", mkPhonyLoc 45)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 46)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 47)
    , ( Pstring "5", mkPhonyLoc 48)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 49)
    , ( Pstring "19.EL.1", mkPhonyLoc 50)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 51)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 52)
    , ( Pstring "8.RHEL4.1", mkPhonyLoc 53)
    , ( Pstring "3.RHEL4.1", mkPhonyLoc 54)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 55)
    , ( Pstring "3.RHEL4.1", mkPhonyLoc 56)
    , ( Pstring "24.5", mkPhonyLoc 57)
    , ( Pstring "21", mkPhonyLoc 58)
    , ( Pstring "4", mkPhonyLoc 59)
    , ( Pstring "4", mkPhonyLoc 60)
    , ( Pstring "2", mkPhonyLoc 61)
    , ( Pstring "35", mkPhonyLoc 62)
    , ( Pstring "14", mkPhonyLoc 63)
    , ( Pstring "6", mkPhonyLoc 64)
    , ( Pstring "9.1.98.EL", mkPhonyLoc 65)
    , ( Pstring "2.13", mkPhonyLoc 66)
    , ( Pstring "2.13", mkPhonyLoc 67)
    , ( Pstring "2", mkPhonyLoc 68)
    , ( Pstring "2.ent", mkPhonyLoc 69)
    , ( Pstring "18.2", mkPhonyLoc 70)
    , ( Pstring "1", mkPhonyLoc 71)
    , ( Pstring "2", mkPhonyLoc 72)
    , ( Pstring "2", mkPhonyLoc 73)
    , ( Pstring "2", mkPhonyLoc 74)
    , ( Pstring "2", mkPhonyLoc 75)
    , ( Pstring "1", mkPhonyLoc 76)
    , ( Pstring "6.rhel4", mkPhonyLoc 77)
    , ( Pstring "19.ent.centos4", mkPhonyLoc 78)
    , ( Pstring "3.9", mkPhonyLoc 79)
    , ( Pstring "3.9", mkPhonyLoc 80)
    , ( Pstring "3.9", mkPhonyLoc 81)
    , ( Pstring "19.ent.centos4", mkPhonyLoc 82)
    , ( Pstring "3.9", mkPhonyLoc 83)
    , ( Pstring "3.9", mkPhonyLoc 84)
    , ( Pstring "24.5", mkPhonyLoc 85)
    , ( Pstring "7.1", mkPhonyLoc 86)
    , ( Pstring "5.EL4", mkPhonyLoc 87)
    , ( Pstring "4", mkPhonyLoc 88)
    , ( Pstring "4", mkPhonyLoc 89)
    , ( Pstring "21", mkPhonyLoc 90)
    , ( Pstring "3.2.RHEL4", mkPhonyLoc 91)
    , ( Pstring "19.ent.centos4", mkPhonyLoc 92)
    , ( Pstring "19.ent.centos4", mkPhonyLoc 93)
    , ( Pstring "4.2.RHEL4", mkPhonyLoc 94)
    , ( Pstring "4", mkPhonyLoc 95)
    , ( Pstring "25", mkPhonyLoc 96)
    , ( Pstring "2.ent", mkPhonyLoc 97)
    , ( Pstring "2.EL4.1", mkPhonyLoc 98)
    , ( Pstring "12", mkPhonyLoc 99)
    , ( Pstring "3.9", mkPhonyLoc 100)
    , ( Pstring "2.13", mkPhonyLoc 101)
    , ( Pstring "2.13", mkPhonyLoc 102)
    , ( Pstring "2.13", mkPhonyLoc 103)
    , ( Pstring "7.EL4.2", mkPhonyLoc 104)
    , ( Pstring "7.1", mkPhonyLoc 105)
    , ( Pstring "13", mkPhonyLoc 106)
    , ( Pstring "2", mkPhonyLoc 107)
    , ( Pstring "39", mkPhonyLoc 108)
    , ( Pstring "24", mkPhonyLoc 109)
    , ( Pstring "7.EL4.2", mkPhonyLoc 110)
    , ( Pstring "7.EL4.2", mkPhonyLoc 111)
    , ( Pstring "7.EL4.2", mkPhonyLoc 112)
    , ( Pstring "7.EL4.2", mkPhonyLoc 113)
    , ( Pstring "7.EL4.2", mkPhonyLoc 114)
    , ( Pstring "7.EL4.2", mkPhonyLoc 115)
    , ( Pstring "98", mkPhonyLoc 116)
    , ( Pstring "14.1", mkPhonyLoc 117)
    , ( Pstring "14.1", mkPhonyLoc 118)
    , ( Pstring "2", mkPhonyLoc 119)
    , ( Pstring "14.1", mkPhonyLoc 120)
    , ( Pstring "14.1", mkPhonyLoc 121)
    , ( Pstring "0.rc1.9.9", mkPhonyLoc 122)
    , ( Pstring "24.RHEL4", mkPhonyLoc 123)
    , ( Pstring "8.rhel4", mkPhonyLoc 124)
    , ( Pstring "10.10.EL4.3", mkPhonyLoc 125)
    , ( Pstring "0.rc1.9.9", mkPhonyLoc 126)
    , ( Pstring "2.40.3", mkPhonyLoc 127)
    , ( Pstring "2.40.3", mkPhonyLoc 128)
    , ( Pstring "4.EL4.1.c4", mkPhonyLoc 129)
    , ( Pstring "5.1", mkPhonyLoc 130)
    , ( Pstring "2.centos4", mkPhonyLoc 131)
    , ( Pstring "2", mkPhonyLoc 132)
    , ( Pstring "2", mkPhonyLoc 133)
    , ( Pstring "1", mkPhonyLoc 134)
    , ( Pstring "2.EL4", mkPhonyLoc 135)
    , ( Pstring "2.19", mkPhonyLoc 136)
    , ( Pstring "2.19", mkPhonyLoc 137)
    , ( Pstring "2", mkPhonyLoc 138)
    , ( Pstring "12.3.EL4", mkPhonyLoc 139)
    , ( Pstring "13_nonptl", mkPhonyLoc 140)
    , ( Pstring "27", mkPhonyLoc 141)
    , ( Pstring "43.8", mkPhonyLoc 142)
    , ( Pstring "13.EL4.3", mkPhonyLoc 143)
    , ( Pstring "14.2", mkPhonyLoc 144)
    , ( Pstring "2.19", mkPhonyLoc 145)
    , ( Pstring "1.el4.2", mkPhonyLoc 146)
    , ( Pstring "1.EL4", mkPhonyLoc 147)
    , ( Pstring "60.RHEL4", mkPhonyLoc 148)
    , ( Pstring "66.14", mkPhonyLoc 149)
    , ( Pstring "7.rhel4", mkPhonyLoc 150)
    , ( Pstring "12.3.EL4", mkPhonyLoc 151)
    , ( Pstring "13_nonptl", mkPhonyLoc 152)
    , ( Pstring "8.3", mkPhonyLoc 153)
    , ( Pstring "2.EL4.3", mkPhonyLoc 154)
    , ( Pstring "2.19", mkPhonyLoc 155)
    , ( Pstring "13_nonptl", mkPhonyLoc 156)
    , ( Pstring "13_nonptl", mkPhonyLoc 157)
    , ( Pstring "27", mkPhonyLoc 158)
    , ( Pstring "43.8", mkPhonyLoc 159)
    , ( Pstring "0.pre5.3.2", mkPhonyLoc 160)
    , ( Pstring "1.4E.12", mkPhonyLoc 161)
    , ( Pstring "18", mkPhonyLoc 162)
    , ( Pstring "3.0.RHEL4", mkPhonyLoc 163)
    , ( Pstring "24.5.c4.1", mkPhonyLoc 164)
    , ( Pstring "7.centos4", mkPhonyLoc 165)
    , ( Pstring "3.RHEL4.3", mkPhonyLoc 166)
    , ( Pstring "2.19", mkPhonyLoc 167)
    , ( Pstring "4.9", mkPhonyLoc 168)
    , ( Pstring "3", mkPhonyLoc 169)
    , ( Pstring "4.4E.1", mkPhonyLoc 170)
    , ( Pstring "0.rc1.9.10", mkPhonyLoc 171)
    , ( Pstring "1.p23.1", mkPhonyLoc 172)
    , ( Pstring "3", mkPhonyLoc 173)
    , ( Pstring "12.3.EL4", mkPhonyLoc 174)
    , ( Pstring "27", mkPhonyLoc 175)
    , ( Pstring "2", mkPhonyLoc 176)
    , ( Pstring "18.EL4.2", mkPhonyLoc 177)
    , ( Pstring "3.2", mkPhonyLoc 178)
    , ( Pstring "1.el4.1", mkPhonyLoc 179)
    , ( Pstring "1.EL4", mkPhonyLoc 180)
    , ( Pstring "16.EL4.16", mkPhonyLoc 181)
    , ( Pstring "3.0.RHEL4", mkPhonyLoc 182)
    , ( Pstring "1.25", mkPhonyLoc 183)
    , ( Pstring "2", mkPhonyLoc 184)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 185)
    , ( Pstring "2", mkPhonyLoc 186)
    , ( Pstring "2", mkPhonyLoc 187)
    , ( Pstring "24.5.c4.1", mkPhonyLoc 188)
    , ( Pstring "4.EL4.1.c4.3", mkPhonyLoc 189)
    , ( Pstring "2", mkPhonyLoc 190)
    , ( Pstring "43.8", mkPhonyLoc 191)
    , ( Pstring "4.centos4", mkPhonyLoc 192)
    , ( Pstring "2.19", mkPhonyLoc 193)
    , ( Pstring "2.126", mkPhonyLoc 194)
    , ( Pstring "3", mkPhonyLoc 195)
    , ( Pstring "1.4E.12", mkPhonyLoc 196)
    , ( Pstring "2.19", mkPhonyLoc 197)
    , ( Pstring "169", mkPhonyLoc 198)
    , ( Pstring "2", mkPhonyLoc 199)
    , ( Pstring "8.RHEL4.12", mkPhonyLoc 200)
    , ( Pstring "2.19", mkPhonyLoc 201)
    , ( Pstring "3.12", mkPhonyLoc 202)
    , ( Pstring "2", mkPhonyLoc 203)
    , ( Pstring "14.2", mkPhonyLoc 204)
    , ( Pstring "2", mkPhonyLoc 205)
    , ( Pstring "14.2", mkPhonyLoc 206)
    , ( Pstring "14.2", mkPhonyLoc 207)
    , ( Pstring "13.EL4.3", mkPhonyLoc 208)
    , ( Pstring "8.RHEL4.12", mkPhonyLoc 209)
    , ( Pstring "27", mkPhonyLoc 210)
    , ( Pstring "8", mkPhonyLoc 211)
    , ( Pstring "3.12", mkPhonyLoc 212)
    , ( Pstring "3.12", mkPhonyLoc 213)
    , ( Pstring "9.RHEL4", mkPhonyLoc 214)
    , ( Pstring "22.ent.centos4", mkPhonyLoc 215)
    , ( Pstring "2.15", mkPhonyLoc 216)
    , ( Pstring "3.12", mkPhonyLoc 217)
    , ( Pstring "38.rhel4", mkPhonyLoc 218)
    , ( Pstring "22.ent.centos4", mkPhonyLoc 219)
    , ( Pstring "4.5", mkPhonyLoc 220)
    , ( Pstring "0.20060315", mkPhonyLoc 221)
    , ( Pstring "3.12", mkPhonyLoc 222)
    , ( Pstring "0.20060315", mkPhonyLoc 223)
    , ( Pstring "2.19", mkPhonyLoc 224)
    , ( Pstring "1.centos4.1", mkPhonyLoc 225)
    , ( Pstring "1.EL.13.25", mkPhonyLoc 226)
    , ( Pstring "22.ent.centos4", mkPhonyLoc 227)
    , ( Pstring "1.EL.13.25", mkPhonyLoc 228)
    , ( Pstring "1.3.RHEL4", mkPhonyLoc 229)
    , ( Pstring "10.12.EL4", mkPhonyLoc 230)
    , ( Pstring "1.EL.13.25", mkPhonyLoc 231)
    , ( Pstring "3.EL4", mkPhonyLoc 232)
    , ( Pstring "14.c4", mkPhonyLoc 233)
    , ( Pstring "14.2", mkPhonyLoc 234)
    , ( Pstring "1.EL", mkPhonyLoc 235)
    , ( Pstring "22.ent.centos4", mkPhonyLoc 236)
    , ( Pstring "1.1.centos4", mkPhonyLoc 237)
    , ( Pstring "34.EL", mkPhonyLoc 238)
    , ( Pstring "3.12", mkPhonyLoc 239)
    , ( Pstring "8.RHEL4.12", mkPhonyLoc 240)
    , ( Pstring "3.5", mkPhonyLoc 241)
    , ( Pstring "6.rhel4.1", mkPhonyLoc 242)
    , ( Pstring "1", mkPhonyLoc 243)
    , ( Pstring "14_EL4", mkPhonyLoc 244)
    , ( Pstring "0.rc1.9.10", mkPhonyLoc 245)
    , ( Pstring "4", mkPhonyLoc 246)
    , ( Pstring "13.1.80", mkPhonyLoc 247)
    , ( Pstring "1.EL.13.25", mkPhonyLoc 248)
    , ( Pstring "54.EL4", mkPhonyLoc 249)
    , ( Pstring "1.EL.13.25.1", mkPhonyLoc 250)
    , ( Pstring "1.EL.13.25.1", mkPhonyLoc 251)
    , ( Pstring "1.EL.13.25.1", mkPhonyLoc 252)
    , ( Pstring "1.EL.13.25.1", mkPhonyLoc 253)
    , ( Pstring "7.EL4.3", mkPhonyLoc 254)
    , ( Pstring "7.EL4.3", mkPhonyLoc 255)
    , ( Pstring "10", mkPhonyLoc 256)
    , ( Pstring "7.EL4.3", mkPhonyLoc 257)
    , ( Pstring "7.EL4.3", mkPhonyLoc 258)
    , ( Pstring "7.EL4.3", mkPhonyLoc 259)
    , ( Pstring "7.EL4.3", mkPhonyLoc 260)
    , ( Pstring "7.EL4.3", mkPhonyLoc 261)
    , ( Pstring "1", mkPhonyLoc 269)
    , ( Pstring "3", mkPhonyLoc 270)
    , ( Pstring "4.el4.kb", mkPhonyLoc 271)
    , ( Pstring "3", mkPhonyLoc 272)
    , ( Pstring "2", mkPhonyLoc 273)
    , ( Pstring "4", mkPhonyLoc 274)
    , ( Pstring "1.el4.kb", mkPhonyLoc 275)
    , ( Pstring "1", mkPhonyLoc 276)
    , ( Pstring "1", mkPhonyLoc 277)
    , ( Pstring "7.el4.kb", mkPhonyLoc 278)
    , ( Pstring "30", mkPhonyLoc 279)
    , ( Pstring "6", mkPhonyLoc 280)
    , ( Pstring "5", mkPhonyLoc 281)
    , ( Pstring "7.rhel4", mkPhonyLoc 282)
    , ( Pstring "7.el4.kb", mkPhonyLoc 283)
    , ( Pstring "33", mkPhonyLoc 284)
    , ( Pstring "1.el4.kb", mkPhonyLoc 285)
    , ( Pstring "28", mkPhonyLoc 286)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 287)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 288)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 289)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 290)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 291)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 292)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 293)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 294)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 295)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 296)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 297)
    , ( Pstring "34.0.1.EL", mkPhonyLoc 298)
    , ( Pstring "2.RHEL4.1", mkPhonyLoc 299)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 300)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 301)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 302)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 303)
    , ( Pstring "3.RHEL4.5", mkPhonyLoc 304)
    , ( Pstring "1.RHEL4.1", mkPhonyLoc 305)
    , ( Pstring "1.el4.kb", mkPhonyLoc 306)
    , ( Pstring "8.el4.kb", mkPhonyLoc 307)
    , ( Pstring "34.0.2.EL", mkPhonyLoc 308)
    , ( Pstring "8.el4.kb", mkPhonyLoc 309)
    , ( Pstring "1.9.EL", mkPhonyLoc 310)
    , ( Pstring "54.EL4", mkPhonyLoc 311)
    , ( Pstring "14_EL4", mkPhonyLoc 312)
    , ( Pstring "54.EL4", mkPhonyLoc 313)
    , ( Pstring "1", mkPhonyLoc 314)
    , ( Pstring "1", mkPhonyLoc 315)
    , ( Pstring "1.rhel4.4", mkPhonyLoc 320)
    , ( Pstring "3.15", mkPhonyLoc 321)
    , ( Pstring "44.EL4", mkPhonyLoc 322)
    , ( Pstring "3.15", mkPhonyLoc 323)
    , ( Pstring "3.15", mkPhonyLoc 324)
    , ( Pstring "3.15", mkPhonyLoc 325)
    , ( Pstring "3.15", mkPhonyLoc 326)
    , ( Pstring "5", mkPhonyLoc 327)
    , ( Pstring "3.1", mkPhonyLoc 328)
    ]
    val minorTokensFreq : int = 317

end