#include <stdio.h> // for fprintf debugging use only

// On input ../data/dns:
//   1 question, 1496 answers, 0 auths, 0 others ?
//   The 1 question has domain_name research.att.com, length 9 + 4 + 4 + 1 = 18,
//   plus 4 bytes for qtype/qclass gives length 22.
//   It starts at 14 (0xe), plus 22 = 36 (0x24).
//   
//   Next thing is supposed to be domain name of first resource
//   in answers.  The c00c at that loc is not a legal label,
//   but it is legal pointer (ptr to data offset 0c == research.att.com label),
//   which ends the domain name.
//   The next 2+2+4+2+rdlength bytes are the rest of the resource.
//   These are: 0006 0001 00000e10 0027 (rdlength = 39)
//   total bytes = 12 + 39 = 51.  36 + 51 = 87 (0x57).
//
//   Another c00c (value 0c).
//   000f 0001 00000e10 000a (rdlength = 10)
//   total bytes = 12 + 10 = 22.  87 + 22 = 109 (0x6D)
//
//   etc.

/* PADS specification for DNS */

Pstruct label_t {
   Psbh_uint8(:1:)        length : length < 64; 
   Pa_string_FW(:length:) l;  
};

Pstruct ptr_t {
  Psbh_uint16(:2:)          praw : (praw>>14) == 3;
  Pcompute Puint16          pfix = praw - (praw & 49152); // 49152 == 0xC000
};

Punion label_or_ptr {
  label_t label;
  ptr_t   ptr;
};

int check256(size_t s, size_t offset) {
  if (s > 255) {
    error(ERROR_FATAL, "domain_name beginning at offset %lx has size %lu > 256",(long)offset, (unsigned long)s);
  };
  return 0;
}

// XXX_REMOVE
Puint32 ctr = 0;
int domain_name_dbg(label_or_ptr* elts, Puint32 length, size_t offset) {
  int i;
  fprintf(stderr, "domain_name begins at offset = %lx (ptr target offset == %lx)\n", (long)offset, (long)offset-2);
  fprintf(stderr, "  domain_name: ");
  if (length == 0) {
    fprintf(stderr, "(EMPTY)");
  } else {
    for (i = 0; i < length; i++) {
      if (i != 0) fprintf(stderr, ".");
      if (elts[i].tag == label) {
	fprintf(stderr, "%s", P_fmt_str(&(elts[i].val.label.l)));
      } else {
	fprintf(stderr, "[ptr:%x->%x]", elts[i].val.ptr.praw, elts[i].val.ptr.pfix);
      }
    }
  }
  fprintf(stderr, "\n\n");
  //  if (++ctr == 10) abort();
  return 1; 
}

Parray domain_name {
  label_or_ptr [] : Plast(check256(eltEnd.offset - arrayBegin.offset, arrayBegin.offset)
			      || elts[current].tag != label
			      || elts[current].val.label.length == 0) ;
} Pwhere {
  domain_name_dbg(elts, length, arrayBegin.offset);
};

Pstruct resource_record {
  domain_name              name;
  Psbh_uint16(:2:)         type;
  Psbh_uint16(:2:)         class;
  Psbh_uint32(:4:)         ttl;    /- should be limited to positive signed 32bit
  Psbh_uint16(:2:)         rdlength;
  Pa_string_FW(:rdlength:) rddata; /- structure determined by type, will fill in later
};

Parray resource_records(:unsigned int size:) {
   resource_record [size];
};

Pstruct question_t {
   domain_name qname;
   Psbh_uint16(:2:) qtype;
   Psbh_uint16(:2:) qclass;
};

Parray questions(:unsigned int size:) {
   question_t [size];
};

Pstruct header_t {
   Psbh_uint16(:2:) id;
   Psbh_uint16(:2:) blob;
   Psbh_uint16(:2:) qdcount; /- number of questions
   Psbh_uint16(:2:) ancount; /- number of answers
   Psbh_uint16(:2:) nscount; /- number of authorities
   Psbh_uint16(:2:) arcount; /- number of additionals
};

int cnt_dbg(const char *cnt_descr, unsigned int cnt) {
  fprintf(stderr, "# of %s = %u\n", cnt_descr, cnt);
  return 1;
}

#if 0
Pstruct dns_msg {
   Psbh_uint16(:2:)                   length;
   header_t                           header :   cnt_dbg("questions", header.qdcount);  /- not? OK to use "header" for both?
   questions(:header.qdcount:)        question:  cnt_dbg("answers",   header.ancount);
   resource_records(:header.ancount:) answer:    cnt_dbg("auths",     header.nscount);
   resource_records(:header.nscount:) authority: cnt_dbg("other",     header.arcount);
   resource_records(:header.arcount:) additional;
};
#else
Pstruct dns_msg {
   Psbh_uint16(:2:)                   length;
   header_t                           header
     :   cnt_dbg("questions", header.qdcount)
     &&  cnt_dbg("answers",   header.ancount)
     &&  cnt_dbg("auths",     header.nscount)
     &&  cnt_dbg("other",     header.arcount);
   questions(:header.qdcount:)        question;
   resource_records(:header.ancount:) answer;
   resource_records(:header.nscount:) authority;
   resource_records(:header.arcount:) additional;
};
#endif

