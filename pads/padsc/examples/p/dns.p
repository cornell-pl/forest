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

// For convenience to get the actual pointer from ptr_t field p
int p_offset(Puint16 p) {
  return (p & (0xC000-1));
}

Pstruct ptr_t {
  Psbh_uint16(:2:)          p : (p>>14) == 3;
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
	fprintf(stderr, "[ptr:%x->%x]", elts[i].val.ptr.p, p_offset(elts[i].val.ptr.p));
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
//} Pwhere { Pparsecheck(domain_name_dbg(elts, length, arrayBegin.offset));
};

Pstruct character_string {
  Psbh_uint8(:1:)        length;
  Pa_string_FW(:length:) bytes;
}

Pstruct A_t(:Puint16 rdlength:) {
  Psbh_uint32(:4:) address : rdlength == 4;
};

Pstruct CNAME_t {
  domain_name name;
};

Pstruct HINFO_t {
  character_string cpu;
  character_string os;
};

Pstruct MB_t {
  domain_name name;
};

Pstruct MD_t {
  domain_name name;
};

Pstruct MF_t {
  domain_name name;
};

Pstruct MG_t {
  domain_name name;
};

Pstruct MINFO_t {
  domain_name rmailbx;
  domain_name emailbx;
};

Pstruct MR_t {
  domain_name name;
};

Pstruct MX_t {
  Psbh_uint16(:2:) preference;
  domain_name exchange;
};

Pstruct NULL_t(:Puint16 rdlength:) {
  Pa_string_FW(:rdlength:) rdata;
};
  
Pstruct NS_t {
  domain_name name;
};

Pstruct PTR_t {
  domain_name name;
};

Pstruct SOA_t {
  domain_name mname;
  domain_name rname;
  Psbh_uint32(:4:) serial;
  Psbh_uint32(:4:) refresh;
  Psbh_uint32(:4:) retry;
  Psbh_uint32(:4:) expire;
  Psbh_uint32(:4:) minimum;
};

Pstruct WKS_t(:Puint16 rdlength:) {
  Pcompute size_t start = position.offset;
  Psbh_uint32(:4:) address;
  Psbh_uint8(:1:) protocol;
  Pcompute size_t middle = position.offset;
  // Need to make sure rdlength > middle - start
  Pa_string_FW(:rdlength-(middle-start):) bitmap;
};

Parray TXT_t(:Puint16 rdlength:) {
  character_string [] : Plast(eltEnd.offset - arrayBegin.offset >= rdlength);
};

Punion rr_spec (:Puint16 t,Puint16 rdlength:) {
  Pswitch (t) {
  Pcase 1 : A_t(:rdlength:) A;
  Pcase 2 : NS_t NS;
  Pcase 3 : MD_t MD;
  Pcase 4 : MF_t MF;
  Pcase 5 : CNAME_t CNAME;
  Pcase 6 : SOA_t SOA;
  Pcase 7 : MB_t MB;
  Pcase 8 : MG_t MG;
  Pcase 9 : MR_t MR;
  Pcase 10 : NULL_t(:rdlength:) RRNULL;
  Pcase 11 : WKS_t(:rdlength:) WKS;
  Pcase 12 : PTR_t PTR;
  Pcase 13 : HINFO_t HINFO;
  Pcase 14 : MINFO_t MINFO;
  Pcase 15 : MX_t MX;
  Pcase 16 : TXT_t(:rdlength:) TXT;
  Pdefault : Pa_string_FW(:rdlength:) unknown;
  }
  // Want to check here that we consumed exactly rdlength octets
};

Pstruct resource_record {
  domain_name              name;
  Psbh_uint16(:2:)         type;
  Psbh_uint16(:2:)         class;
  Psbh_uint32(:4:)         ttl;    /- should be limited to positive signed 32bit
  Psbh_uint16(:2:)         rdlength;
  rr_spec(:type,rdlength:) rdata;
};

Parray resource_records(:unsigned int size:) {
   resource_record [size];
};

Pstruct question_t {
   domain_name qname;
   Psbh_uint16(:2:) qtype : printf("Question type %d\n",qtype);
   Psbh_uint16(:2:) qclass;
};

Parray questions(:unsigned int size:) {
   question_t [size];
};

Pstruct header_t {
   Psbh_uint16(:2:) id;
   Psbh_uint16(:2:) blob : printf("Truncate is %d\n", (blob >> 9) & 1);
   Psbh_uint16(:2:) qdcount; /- number of questions
   Psbh_uint16(:2:) ancount; /- number of answers
   Psbh_uint16(:2:) nscount; /- number of authorities
   Psbh_uint16(:2:) arcount; /- number of additionals
};

int cnt_dbg(const char *cnt_descr, unsigned int cnt) {
  fprintf(stderr, "# of %s = %u\n", cnt_descr, cnt);
  return 1;
};

Pstruct dns_msg {
   Psbh_uint16(:2:)                   length;
   header_t                           header :   cnt_dbg("questions", header.qdcount);  /- not? OK to use "header" for both?
   questions(:header.qdcount:)        question:  cnt_dbg("answers",   header.ancount);
   resource_records(:header.ancount:) answer:    cnt_dbg("auths",     header.nscount);
   resource_records(:header.nscount:) authority: cnt_dbg("other",     header.arcount);
   resource_records(:header.arcount:) additional;
};
