#include "libpadsc-internal.h" /* for testing - normally do not include internal */

int main(int argc, char** argv) {
  char fname[1000];
  char* h;
  Sfio_t* io;
  PDC_int8    data1 = -1;
  PDC_uint8   data2 = 1;
  PDC_int16   data3 = -1;
  PDC_uint16  data4 = 1;
  PDC_int32   data5 = -1;
  PDC_uint32  data6 = 1;
  PDC_int64   data7 = -1;
  PDC_uint64  data8 = 1;

  h = getenv("HOSTSHORT");
  sprintf(fname, "../ex_data.libtest2.%s", h);
  printf("fname = %s\n", fname);
  io = sfopen(0, fname, "w");
  sfwrite(io, (void*)&data1, sizeof(data1));
  sfwrite(io, (void*)&data2, sizeof(data2));
  sfwrite(io, (void*)&data3, sizeof(data3));
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfwrite(io, (void*)&data5, sizeof(data5));
  sfwrite(io, (void*)&data6, sizeof(data6));
  sfwrite(io, (void*)&data7, sizeof(data7));
  sfwrite(io, (void*)&data8, sizeof(data8));
  sfputc(io, '\n');
  sfwrite(io, (void*)&data1, sizeof(data1));
  sfwrite(io, (void*)&data2, sizeof(data2));
  sfwrite(io, (void*)&data3, sizeof(data3));
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfwrite(io, (void*)&data5, sizeof(data5));
  sfwrite(io, (void*)&data6, sizeof(data6));
  sfwrite(io, (void*)&data7, sizeof(data7));
  /*  sfwrite(io, (void*)&data8, sizeof(data8)); */
  sfwrite(io, (void*)&data4, sizeof(data4));
  sfputc(io, '\n');
  sfclose(io);
}
