/* www.geneontology.org OBO flat file format. */
#define COLON ':'

Precord Pstruct OBO_tag_value_pair{
  Pstring(:COLON:) tag; 
  Pre "/: */"; 
  Pstring_ME(:Peor:) val;
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

Precord Pstruct OBO_header{
  OBO_tag_value_pair   format_version : hasTag(format_version,"format-version");
  OBO_tag_value_pair[] tvpairs  : Pterm(Peor);
} Pwhere {
  containsTag(tvpairs.elts, tvpairs.length, "typeref");
};

Penum OBO_stanza_type{
  Term,
  Typeref
};


Precord Pstruct OBO_stanza_tag {
    '['; OBO_stanza_type s_type; ']';
};

Precord Pstruct OBO_stanza{
  OBO_stanza_tag       tag;
  OBO_tag_value_pair   id      : hasTag(id,"id");
  OBO_tag_value_pair[] tvpairs : Pterm(Peor);
} Pwhere {
  containsTagOne(tvpairs.elts, tvpairs.length, "name");
};

Psource Pstruct OBO_file{
  OBO_header hdr;
  OBO_stanza[] stanzas;
};

