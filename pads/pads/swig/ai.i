/* Places where file should be parameterized are marked by PARAMETER_HERE */
/* ai.i -- PARAMETER_HERE */
#define PADS_TY		http_clf_t	// PARAMETER_HERE
#define PADS_TY_MODULE	ai	// PARAMETER_HERE

%module PADS_TY_MODULE	// PARAMETER_HERE

%include "generators.i"

%{
#define FOR_CKIT	1
#define HAVE_SIZE	1
#define HAVE_VA_LIST	1
#include "ai.h"		// PARAMETER_HERE
%}

/*
 * %import is used to obtain type info but not generate wrappers (cf %include)
 * None of the warning supression options (cmd-line, #pragma, #warnfilter)
 * seems to work.  ast_common.h has a complex expression which the swig cpp
 * cannot parse but no ill effects seem to occur.
 */
	//#pragma SWIG nowarn=202
%import "ast_common.h"
	//#pragma SWIG warn=202
%import "pads-private.h"
%import "pads.h"

/*
 * Generate wrappers for this interface:
 */
%include "ai.h" // PARAMETER_HERE

/*
 * Augment interface with these additional functions:
 */

GENERATE_FNS(PADS_TY) // PARAMETER_HERE
