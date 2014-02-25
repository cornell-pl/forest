/* 
 * pads.i 
 *
 * SWIG Def'n of LIBPADS Interface (+ minor AST)
 *
 * - provide maps for PADS API output values, multiple return values, etc.
 * - explore catching Perror_t returns and creating exceptions
 */

%module pads

%include "typemaps.i"

%header %{

/* Must use the ckit-replace fake-out because PERL and AST have different
  definitions for regcomp_t */

#define FOR_CKIT	1
#define HAVE_SIZE	1
#define HAVE_VA_LIST	1
#include "pads-private.h"
#include "pads.h"

%}

%runtime %{
   /* ... code in runtime section ... */
   /* do nothing stub -- could be provided by PADSC generated code so watch out */
   void P_lib_init() { } 
%}

/* 
 * Copied from ast_common.h in order to permit certain pads decls to compile
 * under SWIG
 */
#define _ast_int1_t             char
#define _ast_int2_t             short
#define _ast_int4_t             int
#define _ast_int8_t             long long
#define _ast_intmax_t           _ast_int8_t

/***************
 * AST Interface
 ***************/

#define Sfoff_t                _ast_intmax_t
void error(int level, ...);

/****************
 * PADS Interface
 ****************/

/*
 * PADS-specific type-maps to alter certain function calling sequences
 */

/* 
 * Transform 
 *	Perror_t  P_open  (P_t **pads_out, Pdisc_t *disc, Pio_disc_t *io_disc);
 * to
 *	(Perror_t, P_t *) =  P_open  (Pdisc_t *disc, Pio_disc_t *io_disc);
 *
 * NB: one level of indirection is removed from P_t.
 */
#if defined(SWIGPERL)
%typemap(in, numinputs=0) P_t **pads_out (P_t *tempPadsPtr) {
 	/* Ignore a P_t ** */
	$1 = &tempPadsPtr;
}
%typemap(argout) P_t **pads_out {
 	/* Argout a P_t * (NOT a P_t **) */
	ST(argvi) = sv_newmortal();
	SWIG_MakePtr(ST(argvi++), (void *) *$1, SWIGTYPE_p_P_t, SWIG_SHADOW|0);
}
#elif defined(SWIGJAVA)
	#warning no typemaps for Java specified
#else
	#warning no typemaps for unsupported language specified
#endif

%inline %{

/* P_POS_EQ is implemented in PADS as a macro -- that won't do here because
   we cannot afford to include all header files (PERL v. AST regcomp_t def'ns)
   */

int P_POS_EQ(Ppos_t pos1, Ppos_t pos2) { return ((pos1).offset == (pos2).offset); }

%}

%ignore P_POS_Q;	// see comment above.

%include "rbuf.h"
%include "pads-private.h"
%include "pads.h"
%include "io_disc.h"
