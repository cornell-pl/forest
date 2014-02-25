/* www.geneontology.org OBO flat file format. */

const char COLON = ':';

/* Punion OBO_tag_value_pair(:char *the_tag:){ */
/*   Pswitch(:the_tag:){ */
/*     NULL => Pstring(:COLON:) tag; */
/*     default => Pstring(:COLON:)  */
/*   } */
/* }; */

Precord Pstruct OBO_tag_value_pair{
  Pstring(:COLON:) tag; 
  ": "; /* Skip optional spaces */
  Pstring_SE(:"/$/":) value;
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
  OBO_tag_value_pair format_version : hasTag(format_version,"format-version");
  OBO_tag_value_pair[] others  : Plongest;
} Pwhere {
  containsTag(others.elts, others.length, "typeref");
};

Pstruct OBO_term_stanza{
  "[Term]"; Peor;
  OBO_tag_value_pair id     : hasTag(id,"id");
  OBO_tag_value_pair[] others : Plongest;
} Pwhere {
  containsTagOne(others.elts, others.length, "name");
};

Pstruct OBO_typeref_stanza{
  "[Typeref]"; Peor;
  OBO_tag_value_pair[] pairs : Plongest;
};

Punion OBO_stanza{
  OBO_term_stanza term;
  OBO_typeref_stanza typeref;
};

Psource Pstruct OBO_file{
  OBO_header hdr; Peor;
  OBO_term_stanza[] stanzas : Psep(Peor);
};
