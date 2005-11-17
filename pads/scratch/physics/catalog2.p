#define WS Pre "/ +/"

Pstruct position{
  Pfloat64 RA;
  Pfloat64 dec;
  Pfloat64 phi_offset;
};

Pstruct identity{
  Pint32 run;
  Pint32 rerun;
  Pint32 camcol;
  Pint32 field;
  Pint32 id;
  Pint32 x;
  Pint32 y;
};

Ptypedef Pstring_SE(:WS:) longhex;

Pstruct PHOTO_flags{
  WS; longhex flag1;
  WS; longhex flag2;
};

Parray magnitudes_t{
  Pfloat32[10];
};

Pstruct likelihoods_t{
  Pint32 star; 
  Pint32 exp;
  Pint32 dev;
};

Pstruct band_shape{
  Pfloat32[4] f;
  Pint32[2] i;
};

Pstruct averages{
  Pfloat32 e1;
  Pfloat32 e2;
  Pfloat32 sigma_e;
};

Punion photoz_opt{
  too_weak Pfrom("-1");
  Pfloat32 photoz;
};

Precord Pstruct galaxy{
  position pos; 
  identity id; 
  PHOTO_flags p_flags; 
  WS; longhex flag3; 
  magnitudes_t magnitudes; 
  likelihoods_t   likelihoods; 
  Pfloat32 size;
  band_shape r_band_shape;
  band_shape i_band_shape;
  averages avgs;
  Pfloat32 r_band_extinction;
  WS; photoz_opt photoz;
  Pfloat32 LRG_photoz;
};

Precord Pstruct header{
  Pint32 numGalaxies;
};

Psource Pstruct catalog{
  header hdr;
  galaxy[hdr.numGalaxies] galaxies;
};
