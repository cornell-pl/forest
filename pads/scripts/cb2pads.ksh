#!/home/gsf/arch/linux.i386/bin/ksh
# Mnemonic:  cb2pads
# Abstract:  This script will cause a copy book to be parsed and converted into
#            various C and header files that can be used to process data that
#            is formatted using the COBOL copy book as the data layout.
#            It also outputs a .p file containing a PADS declaration for
#            the data format and a .p file containing path expressions.
#
#            The following is the flow of data through the script.
#
#            copybook ==> copybook.py ==> nawk2----------> cpy($name).h
#             +args                       |-------------> c($name).c
#                                         |-------------> p($name).c
#                                         |-------------> ($name).p
#                                         |-------------> ($name).q
#                                      
#                  ==> Pipe
#                  --> File creation
# Usage:     $0 copy-book-name [opt] [label [destpath]] [path-lines]
# Returns:   Nothing.
# Date:   
# Author:    Andrew Hume, Gus Maclellan, Bob Gruber (reg)
# Modified:
#	19 Aug 1997 (sd)  - To add fixup calls and to clean up a bit
#	22 Oct 1997 (sd)  - To allow redefines on the non-group level
#	25 Oct 1997 (sd)  - To correct length generation and offset problem
#	29 Oct 1997 (sd)  - To allow lower levels to be placed after higher levels. 
#	17 Dec 1997 (sd)  - To use blen generated len in fmt for binary 
#                               fields rather than pic value (I55)
#	29 Jan 1998 (sd)  - To handle new B type and to undefine COMP-1 and COMP-2
#	13 may 2002 (sd)  - Added spewing of #define of length as requested by Beth.
#	10 Jul 2002 (ah)  - add support for DEPEND stuff 
#	22 Mar 2003 (sd)  - Added call to the new python fixup (copybook.py)
#       08 May 2003 (reg) - Added .p file generation.
#       13 Aug 2003 (reg) - Added .q file generation.
#		
# ----------------------------------------------------------------------------

if [ "$PADS_HOME"x == x ]; then
  echo "env var PADS_HOME must be set"
  exit 1
fi


. $PADS_HOME/scripts/stdfun.ksh

case $# in
4)	input=$1; label=$2; dpath=$3; pathlines=$4;;
3)	input=$1; label=$2; dpath=$3; pathlines=0;;
2)	input=$1; label=$2; dpath=.; pathlines=0;;
1)	input=$1; label=$1; dpath=.; pathlines=0;;
*)	echo "Usage: $0 copy-book-name [label [destpath]] [path-lines]" >&2
	exit 1
esac

if [[ ! -r "$input" ]]
then
	echo "$0: Unable to find/read file: $input" >&2
	exit 2 
fi

name=${1%%.*}
name=${name##*/}		# reduce to the copybook name

# Preparse the copy book (fixup) and then work on the preparser output
# to generate C code and header files.
# -------------------------------------------------------------------------

#ng_cpy_fixup < $input | tee temp99 |  awk -v globalname=_$label -v cbname=$name -v dpath=$dpath '
$PADS_HOME/scripts/cb.py < $input | tee temp99 |  awk -v label=$label -v globalname=_$label -v cbname=$name -v dpath=$dpath -v pathlines=$pathlines '

function blen(type, p1, p2){
old = 0
  if(type == "X") return p1                 # chars
  if(type == "i") return p1                 # numbers         PIC 9999 DISPLAY
  if(type == "is") return p1                # signed numbers  PIC S999 DISPLAY
  if(type == "f") return p1+p2              # fixed point     PIC 9v99 DISPLAY
  if(type == "fs") return p1+p2             # signed fixed pt PIC S9V9 DISPLAY
  if(type == "I") return int((p1+2)/2)      # bcd + sign      PIC 9999 PACKED-
  if(type == "Is") return int((p1+2)/2)     # bcd + sign      PIC S999 PACKED-
  if(type == "F") return int((p1+p2+2)/2)   # signed bcd      PIC S9V9 PACKED-
  #if(type == "b" || type == "bs")          # binary          PIC 9999 BINARY
  if(match(type, "b") || match(type, "B"))    # some binary type 
   {                                                          PIC S9V9 BINARY
    p = (10 ^ (p1 + p2)) -1                 # largest number described by pic
    if(p < 65536)                         # half word if possible
     return 2
    word = 4294967295                       # largest unsigned # in 4 bytes
    for(rv = 4; p > word; p /= word)      # count # words needed for 
     rv += 4                                # the largest number
    return rv                               # and send it back as length
   }

  if(match(type, "b8"))    # not implemented in fixup - should not happen
   return 8
  if(match(type, "b4"))    # not implemented in fixup - should not happen
   return 4

  return 0                                  # unrecognized type - error
}

# multi is true if the things (ds) parent is an array and thus we need to 
# reference this with an array subscript at the end of the parents name
# if we recurse back to this routine we set it by looking at the length 
# for the thing (max_len), its an array if its > 1
function traverse(d, multi,	z1,z2,z3,z4,z5){
	#printf("traverse(%s)\n", d) >stderr
	for(z1 = 0; z1 < nfield[d]; z1++){	# for each element in the structure
		z2 = name[d,z1]			# get next element (field) name
		z3 = truename
		if( multi > 1 )
			truename = truename "[(idx)]." z2
		else
				truename = truename "." z2
		#printf("%s == %s vu=%s\n", z2, truename, var_used[z2]) >stderr
		z4 = z2; sub("_[0-9]*$", "", z4)
		z5 = ++def_decl[globalname "_" z4];
		if(z5 == 1)
			decl[++ndecl] = sprintf("#define %s_%s(base%s) ((base)%s)", globalname, z4, multi > 1 ? ",idx" : "", truename)
		else
			decl[++ndecl] = sprintf("#define %s_%s_%d(base%s) ((base)%s)", globalname, z4, z5,  multi > 1 ? ",idx" : "", truename)
		if(var_used[z2])
			var_used[z2] = substr(truename, 2);	# skip initial .
		if(dtype[z2]){			# if element is a structure, recurse
			traverse(dtype[z2], max_len[d,z1]-1)	# max len -1 sets multi if > 1 causing index 
		}
		truename = z3
	}
}
BEGIN {
  mod_cbname = cbname
  if (mod_cbname == ""){
    mod_cbname = label
  }
  cbname = toupper(cbname);
  stderr = "/dev/fd/2"
  fieldlev = 0                  # current level we are processing
  nstack = 0                    # next spot to place something on stack
  ndefn = 0                     # # of defined struct names
  nfield[fieldname] = 0         # # of fields in a structure

  #fieldname = "rootx"
  #lastname = "goo"
  #lastname = "cpy" globalname

  typeof["X"]  = "T_CHARS"       # types based on stuff from 1st pass
  typeof["i"]  = "T_NUM"
  typeof["is"] = "T_SNUM"
  typeof["f"]  = "T_FIXEDPOINT"
  typeof["fs"] = "T_SFIXEDPOINT"
  typeof["I"]  = "T_BCD"
  typeof["Is"] = "T_SBCD"
  typeof["F"]  = "T_SBCDFIXEDPOINT"
  typeof["bs"] = "T_BINARY"
  typeof["b"]  = "T_UBINARY"
  typeof["B"]  = "T_UBINFIXEDPOINT"
  typeof["Bs"] = "T_SBINFIXEDPOINT"
  #  typeof["Bs"] = "T_SBCDFIXEDPOINT"  # XXX wtf?
  typeof["b8"] = "T_UNIMPLEMENTED"
  typeof["b4"] = "T_UNIMPLEMENTED"

  padstype["T_CHARS"]          = "Pe_string_FW"
  padstype["T_NUM"]            = "Pebc_uint64"
  padstype["T_SNUM"]           = "Pebc_int64"
  padstype["T_BCD"]            = "Pbcd_uint64"
  padstype["T_SBCD"]           = "Pbcd_int64"
  padstype["T_FIXEDPOINT"]     = "Pebc_ufpoint64"
  padstype["T_SFIXEDPOINT"]    = "Pebc_fpoint64"
  padstype["T_SBCDFIXEDPOINT"] = "Pbcd_fpoint64"
  padstype["T_BINARY"]         = "Psbh_int64"
  padstype["T_UBINARY"]        = "Psbh_uint64"
  padstype["T_UBINFIXEDPOINT"] = "Psbh_ufpoint64"
  padstype["T_SBINFIXEDPOINT"] = "Psbh_fpoint64"

  padsargs["T_CHARS"]          = "digsum"
  padsargs["T_NUM"]            = "digsum"
  padsargs["T_SNUM"]           = "digsum"
  padsargs["T_BCD"]            = "digsum"
  padsargs["T_SBCD"]           = "digsum"
  padsargs["T_FIXEDPOINT"]     = "digsum_after_v"
  padsargs["T_SFIXEDPOINT"]    = "digsum_after_v"
  padsargs["T_SBCDFIXEDPOINT"] = "digsum_after_v"
  padsargs["T_BINARY"]         = "bytes"
  padsargs["T_UBINARY"]        = "bytes"
  padsargs["T_UBINFIXEDPOINT"] = "bytes_after_v"
  padsargs["T_SBINFIXEDPOINT"] = "bytes_after_v"

  # fieldname is current struct into which elements are being placed
  fieldname = "cpy" globalname    # base struct is first field name
  defn[ndefn++] = fieldname       # save root structure name
  nfield[fieldname] = 0           # number of elements in each struct

  max_tlen = 50; # max length of a type name

  printf("ng_comp_cpy v2.0/9A068\n")>stderr
}

# -------------------- MAIN LOOP -----------------------------------
$2 == "r"  {
   if (redef[$4]) {
     # printf("XXX redef[%s] = %s\n", $3, redef[$4])>stderr;
     redef[$3] = redef[$4]; # all redefs point to first def
   } else {
     # printf("XXX redef[%s] = %s\n", $3, $4)>stderr;
     redef[$3] = $4; # $4 is the first def
   }
   if (mkunion[redef[$3]]) {
     mkunion[redef[$3]] = sprintf("%s|%s", mkunion[redef[$3]], $3)
   } else {
     num_unions++
     mkunion[redef[$3]] = sprintf("%s|%s", redef[$3], $3)
     mkunion_t_nm[redef[$3]] = sprintf("gen_union_%d_t", num_unions)
     mkunion_f_nm[redef[$3]] = sprintf("GEN_UNION_%d", num_unions)
     # printf("XXX mkunion_f_nm[%s] = %s\n", redef[$3], mkunion_f_nm[redef[$3]])>stderr;
   }
   next
}

$2 < fieldlev  {                       # pop things off the stack 
    while($2 <= fieldlev)
     {
      nstack--
      fieldname = stack[nstack]
      fieldvarname = stackvar[nstack]
      fieldlev = flev[nstack]
    }
  }

$2 > fieldlev  {                    # start a new level
    stack[nstack] = fieldname       # save active struct name
    stackvar[nstack] = fieldvarname # save active struct var name
    flev[nstack] = fieldlev         # and the current level
    fieldlev = $2                   # level on stmt becomes current
    nstack++                       
  }

$2 == fieldlev  {       # add an element to current structure
	myname = $3
	outpathq = 0
	outpathnm = ""
	if(pathlines != 0){
	    lineno = $1
	    # printf("ZZZ checking line %d myname %s\n", lineno, myname)>stderr
	    npaths = split(pathlines, pl, ",")
	    for(i = 1; i <= npaths; i++){
		if(lineno == pl[i]){
		    outpathq = 1
		}
	    }
        }
	if(outpathq == 1){
	    # printf("ZZZ FOUND LINE %d myname %s\n", lineno, myname)>stderr
	    for (i = nstack-1; i > 0; i--){
		pathvar = stackvar[i]
		if (i == nstack-1){
		    outpathnm = pathvar
		}else{
		    outpathnm = pathvar "." outpathnm
		}
	    }
	    outpathnm = outpathnm "." myname
	    outpathlineno[outpathnm] = lineno
	    # printf("outpathnm = %s\n", outpathnm)>stderr
	}
#	vlen = 0 + $4
	for(i = 0; i < 1; i++){              # for each occurance
		x = myname
		name[fieldname,nfield[fieldname]] = x
		outpath[fieldname,nfield[fieldname]] = outpathnm
		# printf("ZZZ outpath[%s,%d] = %s\n", fieldname, nfield[fieldname], outpathnm)>stderr
		split($4, aa, "!")
		max_len[fieldname,nfield[fieldname]] = aa[1]
     		min_len[fieldname,nfield[fieldname]] = aa[2]
		var_len[fieldname,nfield[fieldname]] = aa[3]
		if(aa[3] != ""){
			var_used[aa[3]] = "x"
			globaldepend = 1
			#printf("var_used(%s)\n", aa[3])>stderr
		}

		if($5 == "none"){           # if group, this name has its own type
			dtype[x] = $3 globalname
		} else {                         # else it has type indicated by pass 1
			len[x] = blen($5, $6, $7)
		}
		digits_before_v[x] = $6
		digits_after_v[x] = $7
		digits_summed[x] = $6 + $7

					# (I55)
		if($5 == "b")		# binary format needs to hold # bytes required
			$6 = len[x]	# not pic size: 9v(4) yields 2 bytes in buffer
		x = $5 "," $6		# save format and length
		if(NF > 5)
			x = x  ", " $7	# fractional len if there
		fmt[fieldname,nfield[fieldname]] = x
		nfield[fieldname]++                  # incr field count of our parent 
	}                                      # end for each occurance

	if($5 == "none"){			# new structure defined by this item
		fieldname = $3 globalname	# we now are the "parent" struct
		fieldvarname = $3		# track the variable name as well
		nfield[fieldname] = 0		# no elements for this struct yet
		defn[ndefn++] = fieldname	# save it in struct def list
	}
}


# -------------------------- END PROCESSING ----------------------------------
END  {
hdr = dpath "/cpy" globalname ".h"
src = dpath "/c" globalname ".c"
prt = dpath "/p" globalname ".c"
pads = dpath "/cpygen" globalname ".p"
qfile = dpath "/cpygen" globalname ".q"

sys_incs = "#include <sfio.h>\n#include <string.h>\n"		# system include files needed
ng_incs = "#include \"ningaui.h\"\n#include \"ng_parse.h\"\n"	# gecko include files needed

# set up header area of each generated source file
printf("%s", sys_incs) >src
printf("%s", ng_incs) >src
printf("#include \"%s\/cpy%s.h\"\n\n", dpath, globalname) >src            # include file generated by this code

printf("%s", sys_incs) >prt
printf("%s", ng_incs) >prt
printf("#include \"%s\/cpy%s.h\"\n\n", dpath, globalname) >prt
printf("static char *tabs = \"\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\\t\";\n") >prt
printf("static ng_byte *origin;\n\n") >prt

# build true names for depends guys
truename = ""
traverse(defn[0], 0)

# dump out structures in reverse order
for(i = ndefn-1; i >= 0; i--){
	d = defn[i]			# get next structure name

	for(j = 0; j < nfield[d]; j++){			# for each element in the structure
		f = name[d,j]				# get next element (field) name
		vlen = max_len[d,j]
		if(dtype[f]){				# if element is a structure
                   if (vlen > 1 && !pads_ar_type[dtype[f]]) {
			cur_type = dtype[f]
			if (redef[f]) {
			    cur_type = mkunion_t_nm[redef[f]]
			}
			pads_ar_type[cur_type] = sprintf("gen_parray_of_%s", cur_type);
			printf("Parray %s (int len) {\n\t%s [len];\n};\n", pads_ar_type[cur_type], cur_type) >pads;
                   }
                } else {
			s = typeof[substr(fmt[d,j], 1, index(fmt[d,j], ",")-1)]
                        if (padstype[s] && vlen > 1) {
			    cur_type = padstype[s]
			    if (!pads_ar_type[cur_type]) {
				pads_ar_type[cur_type] = sprintf("gen_parray_of_%s", cur_type);
				if (padsargs[s] == "digsum") {
					printf("Parray %s (int num_digits, int len) {\n\t%s(:num_digits:) [len];\n};\n", pads_ar_type[cur_type], cur_type) >pads
				}
				if (padsargs[s] == "bytes"){
					printf("Parray %s (int num_bytes, int len) {\n\t%s(:num_bytes:) [len];\n};\n", pads_ar_type[cur_type], cur_type) >pads
				}
				if (padsargs[s] == "digsum_after_v"){
					printf("Parray %s (int num_digits, int d_exp, int len) {\n\t%s(:num_digits, d_exp:) [len];\n};\n", pads_ar_type[cur_type], cur_type) >pads
				}
				if (padsargs[s] == "bytes_after_v"){
					printf("Parray %s (int num_bytes, int d_exp, int len) {\n\t%s(:num_bytes, d_exp:) [len];\n};\n", pads_ar_type[cur_type], cur_type) >pads
				}
			    }
			}
                }
		if(mkunion[f]) {
		    printf("Punion %s {\n", mkunion_t_nm[f]) >pads;
		    num_uelts = split(mkunion[f], uelts , "|")
		    for(u = 1; u <= num_uelts; u++){
			arm_nm = sprintf("%s_arm", uelts[u])
			printf("\t%-*s %s;\n", max_tlen, dtype[uelts[u]], arm_nm) >pads;
		    }
		    printf("};\n") >pads;
		}
        }

	printf("struct %s {\n", d) >hdr
	printf("Pstruct %s {\n", d) >pads
	printf("static void\ndo_%s(ng_byte *buf, struct %s *t, struct cpy%s *tt)\n{\n", d, d, globalname) >src
	printf("static void\npr_%s(char *indent, ng_byte *buf, Sfio_t *out, struct cpy%s *tt)\n{\n", d, globalname) >prt
	printf("\tsfprintf(out, \"%%s%s >>>>>>aggr\\n\", indent--);\n", d) >prt

	charstart = ""			# starting "text" when moving sequential character buffers
	x = 0				# length of this structure
	zerolenok = 1

	for(j = 0; j < nfield[d]; j++){			# for each element in the structure
		f = name[d,j]				# get next element (field) name
		vlen = max_len[d,j]
		if(dtype[f]){				# if element is a structure
			mylen = len[dtype[f]]
			if(charstart){			# if command paritally written then terminate what was started 
				printf("\t%s, %d);\n", charstart, charlen) >src
				charstart = ""
			}
			if(vlen == 1) xx = ""; else xx = "[" vlen "]"
			printf("\tstruct %s %s%s;   /* %d */\n", dtype[f], f, xx, x) >hdr
			if (!redef[f]) {
			    cur_type = dtype[f]
			    cur_field = f
			    if (mkunion[f]) {
				cur_type = mkunion_t_nm[f]
				cur_field = mkunion_f_nm[f]
			    }
			    if(vlen == 1){
				printf("\t%-*s %s;\n", max_tlen, cur_type, cur_field) >pads
			    } else {
				# printf("\t//- XXX_CHECK Type here should be an array of %s with %d elts\n", cur_type, vlen) >pads
				ty_str = sprintf("%s(:%d:)", pads_ar_type[cur_type], vlen)
				printf("\t%-*s %s;\n", max_tlen, ty_str, cur_field) >pads
			    }
			}

			if(redef[f])			# if redefine of another field
				y = offset[redef[f]]	# offset is the offset of that field
			else
				y = x			# else offset is current offset

			if(vlen == 1){
				printf("\tdo_%s(buf+%d, &t->%s, tt);\n", dtype[f], y, f) >src
				printf("\tpr_%s(indent, buf+%d, out%s, tt);\n", dtype[f], y, xx) >prt
			} else {
				if(var_len[d,j])
					xx = "tt->" var_used[var_len[d,j]]
				else
					xx = max_len[d,j]
				printf("\t{int i; for(i = 0; i < %s; i++) do_%s(buf+%d+i*%d, &t->%s[i], tt);}\n", xx, dtype[f], y, mylen, f) >src
     			 	printf("\t{int i; for(i = 0; i < %s; i++) pr_%s(indent, buf+%d+i*%d, out, tt);}\n", xx, dtype[f], y, mylen) >prt
			}
			offset[f] = y			# save for possible redefine of this field
		} else {		# element is a primitive
			if(redef[f]){			# if it is a redefine of another element 
				fo = offset[redef[f]]	# get offset of real field
			} else {
				offset[f] = x		# save offset of the field 
				fo = x			# set field offset to current offset
			}

			s = typeof[substr(fmt[d,j], 1, index(fmt[d,j], ",")-1)]
			y = substr(fmt[d,j], index(fmt[d,j], ",")+1) 

			if(s == "T_CHARS"){		# build one fill call that copies adjacent character fields
				if(charstart == ""){
					charstart = sprintf("FILL_%s(%s, buf+%d", s, f, fo)
					charlen = 0
				}
				printf("\t  /*FILL_%s(%s, buf+%d, %s);*/\n", s, f, fo, y) >src
				printf("\tPR_%s(%s, buf+%d, %s);\n", s, f, fo, y) >prt
				if(vlen == 1) {
					printf("\tT_CHARS %s[%d];   /* %d */\n", f, len[f], fo) >hdr
				} else {
					printf("\tT_CHARS %s[%d][%d];   /* %d */\n", f, vlen, len[f], fo) >hdr
				}
			} else {
				if(charstart){		# need to terminate what previously written
					printf("\t%s, %d);\n", charstart, charlen) >src
					charstart = ""
				}
				if(vlen == 1){
					printf("\tFILL_%s(%s, buf+%d, %s);\n", s, f, fo, y) >src
					printf("\tPR_%s(%s, buf+%d, %s);\n", s, f, fo, y) >prt
					printf("\t%s %s;   /* %d */\n", s, f, fo) >hdr
				} else {
					printf("\t{int i; for(i=0;i<%d;i++)FILL_%s(%s[i], buf+%d+i*%d, %s);}\n", vlen, s, f, fo, len[f], y) >src
					printf("\t{int i; for(i=0;i<%d;i++)PR_%s(%s, buf+%d+i*%d, %s);}\n", vlen, s, f, fo, len[f], y) >prt
					printf("\t%s %s[%d];   /* %d */\n", s, f, vlen, fo) >hdr
				}
			}
			if (padstype[s]) {
			    cur_type = padstype[s]
			    if (padsargs[s] == "digsum"){
				if(vlen == 1) {
					ty_str = sprintf("%s(:%d:)", cur_type, digits_summed[f])
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				} else {
					# printf("\t//- XXX_CHECK Type here should be an array of %s(:%d:) with %d elts\n", cur_type, digits_summed[f], vlen) >pads
					ty_str = sprintf("%s(:%d,%d:)", pads_ar_type[cur_type], digits_summed[f], vlen)
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				}
			    }
			    if (padsargs[s] == "bytes"){
				if(vlen == 1) {
					ty_str = sprintf("%s(:%d:)", cur_type, len[f])
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				} else {
					# printf("\t//- XXX_CHECK Type here should be an array of %s(:%d:) with %d elts\n", cur_type, len[f], vlen) >pads
					ty_str = sprintf("%s(:%d,%d:)", pads_ar_type[cur_type], len[f], vlen)
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				}
			    }
			    if (padsargs[s] == "digsum_after_v"){
				if(vlen == 1) {
					ty_str = sprintf("%s(:%d,%d:)", cur_type, digits_summed[f], digits_after_v[f])
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				} else {
					# printf("\t//- XXX_CHECK Type here should be an array of %s(:%d,%d:) with %d elts\n", cur_type, digits_summed[f], digits_after_v[f], vlen) >pads
					ty_str = sprintf("%s(:%d,%d,%d:)", pads_ar_type[cur_type], digits_summed[f], digits_after_v[f], vlen)
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				}
			    }
			    if (padsargs[s] == "bytes_after_v"){
				if(vlen == 1) {
					ty_str = sprintf("%s(:%d,%d:)", cur_type, len[f], digits_after_v[f])
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				} else {
					# printf("\t//- XXX_CHECK Type here should be an array of %s(:%d,%d:) with %d elts\n", cur_type, len[f], digits_after_v[f], vlen) >pads
					ty_str = sprintf("%s(:%d,%d,%d:)", pads_ar_type[cur_type], len[f], digits_after_v[f], vlen)
					printf("\t%-*s %s;\n", max_tlen, ty_str, f) >pads
				}
			    }
			}
		}
		if(dtype[f])			# assign length of defined type
			len[f] = len[dtype[f]]

		if((len[f] < 0) || ((len[f] == 0) && !lenz[dtype[f]])){
			printf("comp_cpy pass2: Bad length for %s len=%d j=%d nfield[%s]=%d len[dtype=%s]=%s\n", f, len[f], j, d, nfield[d], dtype[f], len[dtype[f]]) >stderr
			printf("->>error<--\n") >src
		}
		if(!redef[f]){			# field not a redefinition
			zok = 0
			if(var_len[d,j]){
				vlen = min_len[d,j]
				if(vlen == 0) zok = 1
			}
			x += len[f]*vlen	# add element length to structure length
			if(x || !zok)
				zerolenok = 0
		}

		charlen += len[f]		# length for multiple char field copy
	}

	if(charstart){		# terminate block char buffer copy  call
		printf("\t%s, %d);\n", charstart, charlen) >src
		charstart = ""
	}

	printf("}\n\n") >src
	printf("}\n") >prt
	printf("};\t  /* %s len=%d */\n", d, x) >hdr
	printf("};\n", d) >pads

	len[d] = x		# keep the length of the structure
	lenz[d] = zerolenok
}

# go over defns again to compute paths, now that we have offsets
num_paths = 0
for(i = ndefn-1; i >= 0; i--){
	d = defn[i]			# get next structure name
	for(j = 0; j < nfield[d]; j++){			# for each element in the structure
		f = name[d,j]				# get next element (field) name
		vlen = max_len[d,j]
		opn = outpath[d,j]
		if (opn != ""){
		    lineno = outpathlineno[opn]
		    # printf("YYY MUNGING OUTPATH %s\n", opn)>stderr;
		    opn_off = 0
		    mod_opn = ""
		    num_opn_elts = split(opn, opn_elts, ".")
		    for(k = 1; k <= num_opn_elts; k++) {
			pathvar = opn_elts[k]
			mod_pathvar = pathvar
			# printf("YYY pathvar = %s\n", pathvar);
			mod_off = offset[pathvar]
			if (redef[pathvar]){
			    # printf("YYY redef = %s mkunion_f_nm[redef] = %s\n", redef[pathvar], mkunion_f_nm[redef[pathvar]])>stderr
			    mod_pathvar = mkunion_f_nm[redef[pathvar]] "." pathvar "_arm"
			    mod_off = offset[redef[pathvar]]
			}else{
			    if (mkunion_f_nm[pathvar]){
				# printf("YYY redef root = %s mkunion_f_nm[root] = %s\n", pathvar, mkunion_f_nm[pathvar])>stderr
				mod_pathvar = mkunion_f_nm[pathvar] "." pathvar "_arm"
			    }
			}
			if (k == 1){
			    mod_opn = mod_pathvar
			}else{
			    mod_opn = mod_opn "." mod_pathvar
			}
			opn_off += mod_off
		    }
		    # printf("YYY ==> MUNGED OUTPATH %s\n", mod_opn)>stderr;
		    if(dtype[f]){				# if element is a structure
			# no paths except at leaves
			num_paths++
			formatted_path[num_paths] = sprintf("/* ERROR: selected path %s from copybook %s line %d is not a base type [offset %d] */\n",
							    mod_opn, mod_cbname, lineno, opn_off)
			formatted_path_offset[num_paths] = opn_off
			printf("%s", formatted_path[num_paths])>stderr

		    } else {		# element is a primitive
			s = typeof[substr(fmt[d,j], 1, index(fmt[d,j], ",")-1)]
			if (padstype[s]) {
				cur_type = padstype[s]
				ty_str = ""
				if (padsargs[s] == "digsum"){
				    if(vlen == 1) {
					    ty_str = sprintf("%s(:%d:)", cur_type, digits_summed[f])
				    } else {
					    ty_str = sprintf("%s(:%d,%d:)", pads_ar_type[cur_type], digits_summed[f], vlen)
				    }
				}
				if (padsargs[s] == "bytes"){
				    if(vlen == 1) {
					    ty_str = sprintf("%s(:%d:)", cur_type, len[f])
				    } else {
					    ty_str = sprintf("%s(:%d,%d:)", pads_ar_type[cur_type], len[f], vlen)
				    }
				}
				if (padsargs[s] == "digsum_after_v"){
				    if(vlen == 1) {
					    ty_str = sprintf("%s(:%d,%d:)", cur_type, digits_summed[f], digits_after_v[f])
				    } else {
					    ty_str = sprintf("%s(:%d,%d,%d:)", pads_ar_type[cur_type], digits_summed[f], digits_after_v[f], vlen)
				    }
				}
				if (padsargs[s] == "bytes_after_v"){
				    if(vlen == 1) {
					    ty_str = sprintf("%s(:%d,%d:)", cur_type, len[f], digits_after_v[f])
				    } else {
					    ty_str = sprintf("%s(:%d,%d,%d:)", pads_ar_type[cur_type], len[f], digits_after_v[f], vlen)
				    }
				}
				if (ty_str != ""){
				    num_paths++
				    if(vlen == 1){
					formatted_path[num_paths] = sprintf("Poutpath %s; /- field %s type %s offset %d from copybook %s line %d\n", mod_opn, f, ty_str, opn_off, mod_cbname, lineno)
					formatted_path_offset[num_paths] = opn_off
				    }else{
					formatted_path[num_paths] = sprintf("Poutpath %s; /- **ERROR: ARRAY** field %s type %s offset %d from copybook %s line %d\n", mod_opn, f, ty_str, opn_off, mod_cbname, lineno)
					formatted_path_offset[num_paths] = opn_off
				    }
				}
			    }
			}
		}
	}
}
for(i = 1; i < num_paths; i++){
    for(j = i+1; j <= num_paths; j++){
	if (formatted_path_offset[i] > formatted_path_offset[j]){
	    tmp_path = formatted_path[i]
	    tmp_fpo  = formatted_path_offset[i]
	    formatted_path[i] = formatted_path[j]
	    formatted_path_offset[i] = formatted_path_offset[j]
	    formatted_path[j] = tmp_path
	    formatted_path_offset[j] = tmp_fpo
	}
    }
}
for(k = 1; k <= num_paths; k++){
    printf("\n%s", formatted_path[k]) >qfile
}
if (num_paths) printf("\n") >qfile

d = defn[0]                  # clean up the root structure and get out

printf("#define	%s_LENGTH	%d\n", cbname, len[d])>hdr;		# length asked for by bsr 
printf("extern int crack_%s(ng_byte *buf, int len, struct %s *t);\n", d, d) >hdr
printf("extern void print_%s(ng_byte *buf, int len, Sfio_t *out, struct %s *tt);\n", d, d) >hdr
printf("extern int depend_%s;\n", d) >hdr

printf("\nint\ncrack_%s(ng_byte *buf, int len, struct %s *t)\n", d, d) >src
printf("{\n\tif(len < %d)\n\t\treturn 0;\n", len[d]) >src
printf("\tdo_%s(buf, t, t);\n\treturn %d;\n}\n", d, len[d]) >src
printf("\nvoid\nprint_%s(ng_byte *buf, int len, FILE *out, struct %s *tt)\n", d, d) >prt
printf("{\n\torigin = buf;\n") >prt
printf("\tpr_%s(strchr(tabs, 0), buf, out, tt);\n}\n", d) >prt
printf("\nint depend_%s = %d;\n", d, globaldepend+0) >prt
#for(i in nfield){ printf("nfield(%s)=%d\n", i, nfield[i]) >stderr }

for(i = 1; i <= ndecl; i++) print decl[i] > hdr
}'

#rm /tmp/*$$ >/dev/null
