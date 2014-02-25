
Pstruct position{
  Pfloat64 RA;
  Pre "/ +/"; Pfloat64 dec;
  Pre "/ +/"; Pfloat64 phi_offset;
};

Pstruct identity{
  Pint32 run;
  Pre "/ +/"; Pint32 rerun;
  Pre "/ +/"; Pint32 camcol;
  Pre "/ +/"; Pint32 field;
  Pre "/ +/"; Pint32 id;
  Pre "/ +/"; Pint32 x;
  Pre "/ +/"; Pint32 y;
};

Ptypedef Pstring_SE(:"/ +/":) longhex;

Pstruct PHOTO_flags{
  longhex flag1;
  Pre "/ +/"; longhex flag2;
};

Parray magnitudes_t{
  Pfloat32[10]: Psep(Pre "/ +/");
};

Pstruct likelihoods_t{
  Pint32 star; 
  Pre "/ +/"; Pint32 exp;
  Pre "/ +/"; Pint32 dev;
};

Pstruct band_shape{
  Pfloat32[4] f: Psep(Pre "/ +/");
  Pre "/ +/"; 
  Pint32[2] i: Psep(Pre "/ +/");
};

Pstruct averages{
  Pfloat32 e1;
  Pre "/ +/"; Pfloat32 e2;
  Pre "/ +/"; Pfloat32 sigma_e;
};

Punion photoz_opt{
  too_weak Pfrom("-1");
  Pfloat32 photoz;
};

Precord Pstruct galaxy{
  position pos; 
  Pre "/ +/"; identity id; 
  Pre "/ +/"; PHOTO_flags p_flags; 
  Pre "/ +/"; longhex flag3; 
  Pre "/ +/"; magnitudes_t magnitudes; 
  Pre "/ +/"; likelihoods_t   likelihoods; 
  Pre "/ +/"; Pfloat32 size;
  Pre "/ +/"; band_shape r_band_shape;
  Pre "/ +/"; band_shape i_band_shape;
  Pre "/ +/"; averages avgs;
  Pre "/ +/"; Pfloat32 r_band_extinction;
  Pre "/ +/"; photoz_opt photoz;
  Pre "/ +/"; Pfloat32 LRG_photoz;
};

Precord Pstruct header{
  Pint32 numGalaxies;
};

Psource Pstruct catalog{
  header hdr;
  galaxy[hdr.numGalaxies] galaxies;
};
