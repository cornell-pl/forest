#define Pget_extern(T) extern T get_##T(char *uri)
#define Pget(T,URI) get_##T(URI) 

Ptypedef Pfloat32 type1;
Ptypedef Pint32 type2;

/* declare functions for getting data */
Pget_extern(type1);
Pget_extern(type2);

Precord Pstruct node_info_t {
  "NumNodes: ";  Pint32 numNodes;
};


Psource Pstruct source {
  node_info_t nodeInfo;
  Pcompute type1 data11 = Pget(type1,"data/n1t1.data");
  Pcompute type1 data21 = Pget(type1,"data/n2t1.data");
  Pcompute type2 data12 = Pget(type2,"data/n1t2.data");
  Pcompute type2 data22 = Pget(type2,"data/n2t2.data");
}
