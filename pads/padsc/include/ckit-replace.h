// ckit-replace:  replaces ast and system includes
//
// ckit reads this file instead of the usual ast and system includes.
// See the top of pads.h file for the set of include files that
// get skipped over.
//
// The goal is to provide a minimum set of declarations which mimic
// those found in ast and system includes -- just enough to allow ckit
// to type pads include files and code generated by the padsc compiler.
//
// There are two advantages to this approach.
//   1. ckit does not have to parse complex system includes which
//        often use special-case compiler tricks that can confuse
//        it.  We used to fix problems related to gcc tricks
//        by undefining __GNUC__, but this only solved most of the
//        problem cases.  The ckit-replace approach is much simpler,
//        as we do not even attempt to parse actual system files.
//        It should be more portable than our previous approach.
//   2. ckit parses less stuff, so the padsc compiler will be faster.
//
// The main disadvantage of this approach is that if you use a new
// system/ast type or function in a pads header file, or in generated
// code, you need to add a declaration here.  Note that in the case
// of codegen using a function that is not declared here, if you
// use the pcgen-macros approach and declare a codegen macro that
// hides the use of the function, you do not need to declare the
// function here.
//
// Details:
//
// For most struct types we just forward declare a struct and never
// have to define it.  For types regex_t and regmatch_t, we need 
// fake storage declarations because they are inlined into Pregexp_t.
//

#ifndef __CKIT_REPLACE_H__
#define __CKIT_REPLACE_H__

// we need some _ast_foo types
#define _AST_STD_H
#include <ast_common.h>

// map some _ast_foo types to corresponding normal types
typedef _ast_va_list va_list;

// some common types
typedef int size_t;
typedef int ssize_t;

// types/vars that we use from sfio.h
struct _sfio_s; typedef struct _sfio_s Sfio_t;
typedef _ast_intmax_t Sfoff_t;
extern Sfio_t*		sfstdin;
extern Sfio_t*		sfstdout;
extern Sfio_t*		sfstderr;

// types that we use from vmalloc.h
struct _vmalloc_s; typedef struct _vmalloc_s Vmalloc_t;

// types that we use from tm.h
struct _tm_zone_s; typedef struct _tm_zone_s Tm_zone_t;

// types that we use from cdt.h
struct _dt_s; typedef struct _dt_s Dt_t;
struct _dtlink_s; typedef struct _dtlink_s Dtlink_t;

// types/funs that we use from regex.h
typedef _ast_int4_t regflags_t;
struct _regex_s { int _regex_foo; }; typedef struct _regex_s regex_t;
struct _regmatch_s { int _regmatch_foo; }; typedef struct _regmatch_s regmatch_t;
typedef int (*regclass_t)(int);
int	regaddclass (const char*, regclass_t);

// types/funs that we need from error.h
void error (int, ...);

// some standard functions
int isdigit(int C);
void *memcpy(void *out, const void *in, size_t n);
void *memset(const void *dst, int c, size_t length);
int sfprintf (Sfio_t*, const char*, ...);

#endif  /*  __CKIT_REPLACE_H__  */
