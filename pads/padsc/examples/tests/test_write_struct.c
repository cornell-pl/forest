#include "libpadsc.h"
#include "struct.h"
/* #define FILENAME  "stdin" */
#define FILENAME  "../../data/ex_data.struct_write" 

PDC_error_t my_uint32_inv_val(PDC_t *pdc, void *ed_void, void *val_void, void **type_args) {
  PDC_base_ed *ed  = (PDC_base_ed*)ed_void;
  PDC_uint32  *val = (PDC_uint32*)val_void;
  if (ed->errCode == PDC_USER_CONSTRAINT_VIOLATION) {
    (*val) = 77777;
  } else {
    (*val) = 99999;
  }
  return PDC_OK;
}

int main(int argc, char** argv) {
  PDC_t*         pdc;
  testtwo        f1data;
  testtwo_ed     ed = {0};
  const char    *fname = FILENAME;

  if (argc == 2) {
    fname = argv[1];
  }
  if (PDC_ERR == PDC_open(&pdc,0,0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }

  pdc->disc->inv_valfn_map = PDC_inv_valfn_map_create(pdc); /* only needed if no map installed yet */ 
  PDC_set_inv_valfn(pdc, pdc->disc->inv_valfn_map, "PDC_uint32", my_uint32_inv_val);

  if (strcasecmp(fname, "stdin") == 0) {
    error(0, "Data file = standard in\n");
    if (PDC_ERR == PDC_IO_set(pdc, sfstdin)) {
      error(2, "*** PDC_IO_set(sfstdin) failed ***");
      exit(-1);
    }
  } else {
    error(0, "Data file = %s\n", fname);
    if (PDC_ERR == PDC_IO_fopen(pdc, (char*)fname)) {
      error(2, "*** PDC_IO_fopen failed ***");
      exit(-1);
    }
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    error(0, "\ncalling testtwo_read");
    if (PDC_OK == testtwo_read(pdc, 0, &ed, &f1data)) {
      /* do something with the data */
      error(2, "testtwo_read returned: id %d  ts %d  f %d ", f1data.header.id, f1data.header.ts, f1data.f);
      testtwo_write2io(pdc, sfstdout, &ed, &f1data);
    } else {
      error(2, "testtwo_read returned: error");
      testtwo_write2io(pdc, sfstdout, &ed, &f1data);
    }
  }
  error(0, "\nFound eof");

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2, "*** PDC_IO_close failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
