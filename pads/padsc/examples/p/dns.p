/* PADS specification for DNS */

Pstruct label_t {
   Psbh_uint8(:1:)        length : length < 64; 
   Pa_string_FW(:length:) l;  
};

int chkPtr(Puint16 *ptr){
  *ptr  &= ( (1<<15) -1);
  return 1;
}

Pstruct ptr_t {
  Psbh_uint16(:2:)  ptr1 : ptr1>>14 == 3; 
} Pwhere {
  chkPtr(&ptr1);
};

Punion label_or_pointer{
  label_t label;
  ptr_t   ptr;
};

int check256(size_t s){
  if (s > 255) {
    abort();
  };
  return 1;
}

int pdebug(size_t s){
  error(0, "offset = %x", s);
  return 1; 
}

Parray domain_name {
  label_or_pointer [] : Plast(check256(eltEnd.offset - arrayBegin.offset) &&
			      (elts[current].tag != label ||
			       elts[current].val.label.length > 0)) ;
} Pwhere {
  Pgeneral(pdebug(arrayBegin.offset));
};

Pstruct resource_record {
   domain_name           name;
   Psbh_uint16(:2:)      type;
   Psbh_uint16(:2:)      class;
   Psbh_uint32(:4:)      ttl;    /- should be limited to positive signed 32bit
   Psbh_uint16(:2:)      rdlength;
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

Pstruct dns_msg {
   Psbh_uint16(:2:)                   length;
   header_t                           header; /- OK to use "header" for both?
   questions(:header.qdcount:)        question;
   resource_records(:header.ancount:) answer;
   resource_records(:header.nscount:) authority;
   resource_records(:header.arcount:) additional;
};

