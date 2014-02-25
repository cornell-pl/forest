#define PgetLocal_extern(T) extern T getLocal_##T(char *uri)
#define PgetLocal(T,URI) getLocal_##T(URI) 
Ptypedef Pfloat32 type1;
Ptypedef Pint32 type2;

/* declare functions for getting data */
PgetLocal_extern(type1);
PgetLocal_extern(type2);

Ptypedef Pstring_SE(:Peor:) node_name_t;

/* ickiness to build file location */
char temp[100];
char *concat(char *s1, Pstring *p2, char *s3) {
  strcpy(temp, s1);
  strncat(temp,p2->str, p2->len);
  strcat(temp,s3);
  return temp;
};


Precord Pstruct node_info_t {
  node_name_t nodeName;
  Pcompute type1 data1 = PgetLocal(type1, concat("data/",&nodeName,"/data1.txt"));
  Pcompute type2 data2 = PgetLocal(type2, concat("data/",&nodeName,"/data2.txt"));
}

Parray node_list_t {
  node_info_t[]; 
};

Psource Pstruct source {
  node_list_t nodes;
}
