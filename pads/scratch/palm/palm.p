// Create new Pads binary library. Naming convention for binary library: Pbl.

typedef Puint32 Pbl_offset;
#define Pbl_eof_offset  0
#define Pbl_null_offset 0

Pstruct skipto(:Pbl_offset target:){
/*   Pcompute Ppos_t current = 0 : Pparsecheck((current = position,1)); */
  Pcompute Pbl_offset current = position.offset;
  Pcompute Puint32 len = target > current ? target - current : 0;
  Pb_int8[len] a; /* Ideally, we'd omit all elements. But then we can never reach len elements. Should think of fix. */
};

/* from start until, not including, end. */
Punion string_FP(:Pbl_offset start, Pbl_offset end:){
  Pswitch(end){
    Pcase Pbl_eof_offset : Pchar[] s1; /* Terminates at EOF. */
    Pdefault : Pstring_FW(:end - start:) s2;
  }
};

Pstruct string_until(:Pbl_offset end:){
  Pcompute Pbl_offset current = position.offset;
  string_FP(:current,end:) s;
};

/* from start until, not including, end. FP = fixed-position*/
Pstruct string_at(:Pbl_offset start, Pbl_offset end:){
    Pomit skipto(:start:)       sk;
    string_FP(:start,end:) str;
};

/* from start until, not including, end. 
   if start is Pbl_eof_offset, does not read anything. 
   if end is Pbl_eof_offset (and start != Pbl_null_offset), reads until end of file.
*/
Punion string_at_opt(:Pbl_offset start, Pbl_offset end:){
  Pswitch(start){
    Pcase Pbl_null_offset  : Pcompute Pint8           none = 0;
    Pdefault : string_at(:start,end:) some;
  } 
};

/* Hack. Really should be string terminated by \0 or maximum of len bytes.
use char array terminated by \- followed by optional padding*/
Ptypedef Pstring_FW(:len:) cstring(:int len:);
/* Ptypedef Pstring_ME(:"/.{4}|.*\\0/":) cstring(:int len:); */

Ptypedef Psbh_int32(:1:) card8;
Ptypedef Psbh_int32(:2:) card16;
Ptypedef Psbh_int32(:3:) card24;
Ptypedef Psbh_int32(:4:) card32;

/* needs to be modified so as to convert hi and lo to unix time */
Pstruct date{
  card16 hi;
  card16 lo;
};

Pstruct header{
  cstring(:32:) name;
  card16 flags;
  card16 version;
  date create_date;
  date backup_date;
  date modify_date;
  card32 mod_num;
  card32 app_info_pos;
  card32 sort_info_pos;
  cstring(:4:) db_type;
  cstring(:4:) creator;
  card32 uid_seed;
  card32 next_list;
};

Pstruct pointer{
  card32 rec_pos;
  card8  rec_attrs;
  card24 rec_uid;
};

Pstruct pointer_list{
  card16 count;
  pointer[count] pointers;
};

Pstruct record(:int i, Pbl_offset end, int attrs, int uid:){
  Pcompute Pint32 index = i;
  string_until(:end:) contents;
  Pcompute Pint32 attributes = attrs  & 0xF0;
  Pcompute Pint32 category = attrs & 0xF;
  Pcompute Pint32 unique_id = uid;
};

Parray records_t(:int* index, int nrec, pointer* ptrs:){
  record(:*index, 
	 *index < nrec - 1 ? ptrs[*index+1].rec_pos : Pbl_eof_offset,
	 ptrs[*index].rec_attrs,
	 ptrs[*index < nrec - 1 ? (*index)++ : (*index = 0, nrec -1)].rec_uid:) [nrec];
};

/* Parray records(:int i, int nrec, pointer* ptrs:){ */
/*   record(:current,  */
/* 	 current < nrec - 1 ? ptrs[current+1].rec_pos : Pbl_eof_offset, */
/* 	 ptrs[current].rec_attrs, */
/* 	 ptrs[current].rec_uid:) [nrec]; */
/* }; */

Psource Pstruct PalmDB{
  header hdr;
  pointer_list rec_pointers;
  Pcompute Pbl_offset sort_info_end = 
    rec_pointers.count == 0 ? Pbl_eof_offset : rec_pointers.pointers.elts[0].rec_pos;
  Pcompute Pbl_offset app_info_end = 
    hdr.sort_info_pos == Pbl_null_offset ? sort_info_end : hdr.sort_info_pos;
  string_at_opt(:hdr.app_info_pos,  app_info_end:) app_info;
  string_at_opt(:hdr.sort_info_pos, sort_info_end:) sort_info;
  Pcompute int index_state = 0;
  records_t(:&index_state,rec_pointers.count,rec_pointers.pointers.elts:) records;
};
