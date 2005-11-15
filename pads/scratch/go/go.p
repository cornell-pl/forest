/* www.geneontology.org OBO flat file format. */

const char COLON = ':';

/* Punion OBO_tag_value_pair(:char *the_tag:){ */
/*   Pswitch(:the_tag:){ */
/*     NULL => Pstring(:COLON:) tag; */
/*     default => Pstring(:COLON:)  */
/*   } */
/* }; */

Pstruct OBO_tag_value_pair(:char term:){
  Pstring(:COLON:) tag; 
  Pre "/: */"; /* Skip optional spaces */
  Pstring(:term:) val;
  Pomit Pchar t : t == term;
};

int hasTag(OBO_tag_value_pair p,char *tag){
  return 1;
/*   return Pstring_eq_cstr(&p.tag,tag) == 0; */
};


/* Contains at least one t-v pair with specified tag. */
int containsTag(OBO_tag_value_pair *pairs, int numPairs, char *tag){
  return 1;
/*   int i; */
/*   for (i = 0; i < numPairs; i++){ */
/*     if (hasTag(pairs[i],tag)) return 1; */
/*   } */
/*   return 0; */
};

/* Contains exactly one t-v pair with specified tag. */
int containsTagOne(OBO_tag_value_pair *pairs, int numPairs, char *tag){
  return 1;
/*   int i; */
/*   int found = 0; */
/*   for (i = 0; i < numPairs; i++){ */
/*     if (hasTag(pairs[i],tag)){ */
/*       if (found) return 0; /\* Tag appears more than once.*\/ */
/*       else found = 1; /\* Tag appears at least once. Scan rest of array. *\/ */
/*     } */
/*   } */
/*   return found; */
};

Pstruct OBO_header{
  OBO_tag_value_pair(:'\n':)   format_version : hasTag(format_version,"format-version");
  OBO_tag_value_pair(:'\n':)[] tvpairs  : Pterm('\n');
  '\n';
} Pwhere {
  containsTag(tvpairs.elts, tvpairs.length, "typeref");
};

Penum OBO_stanza_type{
  Term,
  Typeref
};

Pstruct OBO_stanza{
  '['; OBO_stanza_type s_type; "]\n";
  OBO_tag_value_pair(:'\n':)   id     : hasTag(id,"id");
  OBO_tag_value_pair(:'\n':)[] tvpairs : Pterm('\n');
  '\n';
} Pwhere {
  containsTagOne(tvpairs.elts, tvpairs.length, "name");
};

/* Pstruct OBO_term_stanza{ */
/*   "[Term]\n"; */
/*   OBO_tag_value_pair(:'\n':)   id     : hasTag(id,"id"); */
/*   OBO_tag_value_pair(:'\n':)[] tvpairs : Pterm('\n'); */
/*   '\n'; */
/* } Pwhere { */
/*   containsTagOne(tvpairs.elts, tvpairs.length, "name"); */
/* }; */

/* Pstruct OBO_typeref_stanza{ */
/*   "[Typeref]\n"; */
/*   OBO_tag_value_pair(:'\n':)[] pairs : Pterm('\n'); */
/* }; */

/* Punion OBO_stanza{ */
/*   OBO_term_stanza term; */
/*   OBO_typeref_stanza typeref; */
/* }; */

Psource Pstruct OBO_file{
  OBO_header hdr;
  OBO_stanza[] stanzas : Plongest;
};
