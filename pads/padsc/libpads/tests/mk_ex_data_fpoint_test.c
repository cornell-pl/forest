#include "libpadsc-internal.h" /* for testing - normally do not include internal */

static PDC_uint64 PDCI_10toThe[] = {
  /* 10^0  = */                          1ULL,
  /* 10^1  = */                         10ULL,
  /* 10^2  = */                        100ULL,
  /* 10^3  = */                       1000ULL,
  /* 10^4  = */                      10000ULL,
  /* 10^5  = */                     100000ULL,
  /* 10^6  = */                    1000000ULL,
  /* 10^7  = */                   10000000ULL,
  /* 10^8  = */                  100000000ULL,
  /* 10^9  = */                 1000000000ULL,
  /* 10^10 = */                10000000000ULL,
  /* 10^11 = */               100000000000ULL,
  /* 10^12 = */              1000000000000ULL,
  /* 10^13 = */             10000000000000ULL,
  /* 10^14 = */            100000000000000ULL,
  /* 10^15 = */           1000000000000000ULL,
  /* 10^16 = */          10000000000000000ULL,
  /* 10^17 = */         100000000000000000ULL,
  /* 10^18 = */        1000000000000000000ULL,
  /* 10^19 = */       10000000000000000000ULL
};

PDC_byte ea_tab[256] =
{
  0x00, 0x01, 0x02, 0x03, '?',  0x09, '?',  0x7f,
  '?',  '?',  '?',  0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  0x10, 0x11, 0x12, 0x13, '?',  '?',  0x08,'?',
  0x18, 0x09, '?',  '?',  0x1c, 0x1d, 0x1e, 0x1f,
  '?',  '?',  '?',  '?',  '?',  0x0a, 0x17, 0x1b,
  '?',  '?',  '?',  '?',  '?',  0x05, 0x06, 0x07,
  '?',  '?',  0x16, '?',  '?',  '?',  '?',  0x04,
  '?',  '?',  '?',  '?',  0x14, 0x15, '?',  0x1a,
  0x20, '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x5b, 0x2e, 0x3c, 0x28, 0x2b, 0x21,
  0x26, '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x5d, 0x24, 0x2a, 0x29, 0x3b, 0x5e, /* not-sign 0xac -> circumflex 0x5e */
  0x2d, 0x2f, '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  0x7c, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,
  '?',  0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70,
  0x71, 0x72, '?',  '?',  '?',  '?',  '?', '?',
  '?',  0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, /* non-spacing macron 0xaf -> tilde 0x7e */
  0x79, 0x7a, '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  '?',  '?',  '?',  '?',  '?',  '?',  '?', '?',
  0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, '?',  '?',  '?',  '?',  '?', '?',
  0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,
  0x51, 0x52, '?',  '?',  '?',  '?',  '?', '?',
  0x5c, '?',  0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
  0x59, 0x5a, '?',  '?',  '?',  '?',  '?', '?',
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, '?',  '?',  '?',  '?',  '?', '?',
};
PDC_byte ae_tab[256] =
{
  0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f, 
  0x16, 0x19, 0x25, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
  0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26, 
  0x18, '?',  0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f, 
  0x40, 0x4f, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d, 
  0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61, 
  0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 
  0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f, 
  0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 
  0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 
  0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 
  0xe7, 0xe8, 0xe9, 0x4a, 0xe0, 0x5a, 0x5f, 0x6d, 
  0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 
  0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 
  0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 
  0xa7, 0xa8, 0xa9, 0xc0, 0x6a, 0xd0, 0xa1, 0x07, 
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
  '?',  '?',  '?',  '?',  '?',  '?',  '?',  '?',  
};

void
etoa(register PDC_byte *ebc, register PDC_byte *asc, int n)
{
  register PDC_byte *end;

  switch(n)
    {
    case 6:	asc[5] = ea_tab[ebc[5]];
    case 5:	asc[4] = ea_tab[ebc[4]];
    case 4:	asc[3] = ea_tab[ebc[3]];
    case 3:	asc[2] = ea_tab[ebc[2]];
    case 2:	asc[1] = ea_tab[ebc[1]];
    case 1:	asc[0] = ea_tab[ebc[0]];
      return;
    }

  end = ebc+n;
  while (ebc < end)
    *asc++ = ea_tab[*ebc++];
}

PDC_int64
etoi(PDC_byte *ebc, int n)
{
  PDC_int64 r;
  long rr;
  int neg;

  if (n >= 21) {
    error(0, "bad field size (%d) in etoi", n);
    return 0;
  }
  neg = ((ebc[n-1] & 0xF0) == 0xD0); /* look at sign nibble; C,F >=0; D < 0 */
  if (n < 10) {
    rr = 0;
    ebc += n;
    switch(n)
      {
      case 9:	rr += 100000000 * (0xF&ebc[-9]);
      case 8:	rr += 10000000 * (0xF&ebc[-8]);
      case 7:	rr += 1000000 * (0xF&ebc[-7]);
      case 6:	rr += 100000 * (0xF&ebc[-6]);
      case 5:	rr += 10000 * (0xF&ebc[-5]);
      case 4:	rr += 1000 * (0xF&ebc[-4]);
      case 3:	rr += 100 * (0xF&ebc[-3]);
      case 2:	rr += 10 * (0xF&ebc[-2]);
      case 1:	rr += 1 * (0xF&ebc[-1]);
      }
    r = rr;
  } else {
    r = 0;
    while (--n >= 0) {
      r = r*10 + (0xF & *ebc++);
    }
  }
  return neg ? - r : r;
}

void
PDC_int64_to_ebcdic(PDC_byte *ebc, PDC_int64 in, int len)
{
  unsigned long long r;
  int digit, idx = len-1;

  r = (in < 0) ? -in : in;
  printf("converting %lld\n", (long long)in);
  if (len >= 21) {
    error(0, "bad field size (%d) in PDC_int64_to_ebcdic", len);
    return;
  }
  printf("reverse digits: ");
  while (idx >= 0) {
    digit = r % 10;
    r /= 10;
    ebc[idx--] = 0xF0 | digit;
    printf("%d", digit);
  }
  if (r) {
    error(0, "PDC_int64_to_ebcdic: XXX warning, length %d not wide enough to encode value %lld",
	  len, (long long)in);
  }
  if (in < 0) {
    ebc[len-1] &= 0xDF; /* force sign nibble to negative */
    printf("-");
  }
  printf("\n\n");
}

void
PDC_uint64_to_ebcdic(PDC_byte *ebc, PDC_uint64 in, int len)
{
  unsigned long long r;
  int digit, idx = len-1;

  r = in;
  printf("converting %llu\n", (unsigned long long)in);
  if (len >= 21) {
    error(0, "bad field size (%d) in PDC_uint64_to_ebcdic", len);
    return;
  }
  printf("reverse digits: ");
  while (idx >= 0) {
    digit = r % 10;
    r /= 10;
    ebc[idx--] = 0xF0 | digit;
    printf("%d", digit);
  }
  if (r) {
    error(0, "PDC_uint64_to_ebcdic: XXX warning, length %d not wide enough to encode value %llu",
	  len, (unsigned long long)in);
  }
  printf("\n\n");
}

#define FUN_NUM  111111111111111111ULL

int main(int argc, char** argv) {
  PDC_uint64 unum;
  const char* fname = "../../data/ex_data.fpoint_test";
  char tmp[1000];
  Sfio_t* io;
  int w, n, d;

  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");

  /* write copy of unum once for each legal n/d combo making up each legal width */
  for (w = 1; w < 19; w++) {
    unum = FUN_NUM / (PDCI_10toThe[18-w]);
    printf("For width %d chose fun number %llu\n", w, unum);
    PDC_uint64_to_ebcdic((PDC_byte*)tmp, unum, w);
    for (n = w; n >= 0; n--) {
      d = w-n;
      sfwrite(io, (void*)tmp, w);
      sfputc(io, PDC_EBCDIC_NEWLINE);
    }
  }

  /* write 19 and 20 byte copies for illegal combo testing */
  sfwrite(io, (void*)tmp, 19);
  sfputc(io, PDC_EBCDIC_NEWLINE);
  sfwrite(io, (void*)tmp, 20);
  sfputc(io, PDC_EBCDIC_NEWLINE);

  sfclose(io);
}
