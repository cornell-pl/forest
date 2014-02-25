/* 
 * generators.i 
 *
 * Helper macros to generate helper functions that are better performed on 
 * the C-side of the scripting language/C interface.
 *
 * CAST_FN_GENERATOR(TYPE,SUFFIX) generates a function that casts a void * to
 * a ptr to the C type with the name "TYPE ## SUFFIX".
 *
 * SIZEOF_FN_GENERATOR(TYPE,SUFFIX) generates a function that returns the
 * sizeof the C type with the name "TYPE ## SUFFIX".
 *
 * CAST_GENERATORS(TYPE) calls CAST_FN_GENERATOR for TYPE with suffixes
 * "",_pd,_m,_acc.  Likewise SIZEOF_GENERATORS.
 *
 * GENERATE_FNS(TYPE) calls both CAST_GENERATORS and SIZEOF_GENERATORS within
 * an %inline section.
 *
 */

%define CAST_FN_GENERATOR(TYPE,SUFFIX) 	
TYPE##SUFFIX *
voidTo##TYPE##SUFFIX(void *p) { 
	return (TYPE##SUFFIX *)p; 
}
%enddef
%define SIZEOF_FN_GENERATOR(TYPE,SUFFIX)	
size_t 
sizeof_##TYPE##SUFFIX() { 
	return sizeof(TYPE##SUFFIX); 
}
%enddef

%define CAST_GENERATORS(TYPE)
CAST_FN_GENERATOR(TYPE,)
CAST_FN_GENERATOR(TYPE,_pd)
CAST_FN_GENERATOR(TYPE,_m)
CAST_FN_GENERATOR(TYPE,_acc)
%enddef

%define SIZEOF_GENERATORS(TYPE)
SIZEOF_FN_GENERATOR(TYPE,)
SIZEOF_FN_GENERATOR(TYPE,_pd)
SIZEOF_FN_GENERATOR(TYPE,_m)
SIZEOF_FN_GENERATOR(TYPE,_acc)
%enddef

%define GENERATE_FNS(TYPE)
%inline %{
CAST_GENERATORS(TYPE)
SIZEOF_GENERATORS(TYPE)
%}
%enddef
