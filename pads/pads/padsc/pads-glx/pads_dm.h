#ifndef PADS_DM_HEADER
#define PADS_DM_HEADER
#include "galax.h"
#include "pads_c.h"
extern char *pads_error_string; 
extern galax_err padsDocument(processing_context pc, char *uri, char *psource_file, nodeRep nr, item *doc);
extern galax_err walkPadsDocument(item doc);

/* Functions for packing and unpacking atomicValues */
extern value c2ml_atomicValue(cAtomicValue *in);
extern void ml2c_atomicValue(value in, cAtomicValue *out);
#endif
