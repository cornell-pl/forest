#include "vanilla.p"

Pstruct pair_t {
  PPint v_proc_num;
  ' ';
  PPint v_node_num;
};

Pstruct pairs {
  Puint16  v_num_pairs;
  ' ';
  pair_t[v_num_pairs] v_elems : Psep (' ') && Plongest;
  ' ';
}

Pstruct shorthand {
  PPint v_num_procs;
  " * ";
  PPint v_node_num_short;
  Popt Pstring_ME(:"/\\+/":) v_plus;
  Popt PPint v_expo;
  ' ';
}

Ptypedef Pstring_SE(:Peor:) err_t;

Punion proc_assign_t {
  shorthand v_shorthand;
  pairs v_pairs;
  err_t v_error;
} 


Pstruct norm_entry_t {
        Puint32         v_job_end_tm;
        ' ';
        Puint32         v_num_rep_users;
        ' ';
        Puint16         v_procs;
        ' ';
        Puint32         v_job_sub_tm;
        ' ';
        Puint32         v_job_sug_start_tm;
        ' ';
        Puint32         v_job_deadline;
        ' ';
        Puint32         v_dispatch_tm;
        ' ';
        Pfloat64        v_user_tm;
        ' ';
        Pfloat64        v_sys_tm;
        ' ';
        Pfloat64        v_total_tm;
        ' ';
        proc_assign_t   v_proc_assign;
};

Pstruct global_err {
        PPwhite sp1;
        PPint int1;
        PPwhite sp2;
        PPint int2;
        PPwhite sp3;
}

Precord Punion entry_t {
        norm_entry_t v_norm_entry;
        global_err v_global_err;
}

Psource Parray table_t {
	entry_t[];
};
