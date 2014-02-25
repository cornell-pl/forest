Penum arch_t {
  noarch Pfrom("noarch"), 
  my_i386 Pfrom("i386"), 
  my_i586 Pfrom("i586"), 
  my_i686 Pfrom("i686"), 
  my_alpha Pfrom("alpha"), 
  my_sparc Pfrom("sparc"), 
  my_mips Pfrom("mips"), 
  my_ppc Pfrom("ppc"), 
  my_m68k Pfrom("m68k"), 
  my_SGI Pfrom("SGI"),
  my_none Pfrom("(none)")
};

Punion release_t {
  Pstring_SE(:"/\\.i386/":) r1;
  Pstring_SE(:"/\\.i586/":) r2;
  Pstring_SE(:"/\\.i686/":) r3;
  Pstring_SE(:"/\\.noarch/":) r4;
  Pstring_SE(:"/\\.\\(none\\)/":) r5;
};

Precord Pstruct entry_t {
         Pstring_SE(:"/-\\d/":)       name;
         '-'; Pstring_ME(:"/\\d[\\w\\.]*/":)  version;
         '-'; release_t release;
   	 '.';  arch_t      arch;
   	 ".rpm";
};

Psource Parray rpm_t {
  entry_t [];
}
