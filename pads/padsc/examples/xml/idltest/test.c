#include <stdio.h>
#include "test.h"

typedef struct nodeRepStruct { 
  char *name; 
  struct nodeRepStruct *parent; 
  struct nodeRepStruct **kids; 
  char *typed_value; 
} *nodeRepPtr;

void dfs(char *t, nodeRepPtr n) { 
  int i;
  nodeRepPtr k;

  printf("%s%s\n", t, n->name); 
  for (i = 0; n->kids && n->kids[i]; i++) {
    k = n->kids[i];
    dfs(t, k); 
  }
}
/*
No mainline when linking with Caml.  Just here for testing C side.
int main(int argc, char *argv[]) { 
  nodeRep x = root();
  dfs("", (nodeRepPtr)x);
  return 1;
}
*/
nodeRep root(void) { 
  nodeRepPtr root;
  nodeRepPtr k1, k11, k2, k21, k22, k3, k31, k32, k33; 

  root = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  root->name = "doc"; 
  root->parent = (nodeRepPtr)NULL; 
  root->typed_value = "";
  root->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 4); 

  k1 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k1->name = "k1";
  k1->parent = root; 
  k1->typed_value = "";
  k1->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 2); 

  k11 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k11->name = "k11";
  k11->parent = k1;
  k11->typed_value = "_k11_";
  k11->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k11->kids[0] = (nodeRepPtr)NULL;

  k1->kids[0] = k11;
  k1->kids[1] = (nodeRepPtr)NULL;

  k2 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k2->name = "k2";
  k2->parent = root; 
  k2->typed_value = "";
  k2->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 3); 

  k21 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k21->name = "k21";
  k21->parent = k2; 
  k21->typed_value = "_k21_";
  k21->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k21->kids[0] = (nodeRepPtr)NULL;

  k22 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k22->name = "k22";
  k22->parent = k2; 
  k22->typed_value = "_k22_";
  k22->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k22->kids[0] = (nodeRepPtr)NULL;

  k2->kids[0] = k21;
  k2->kids[1] = k22;
  k2->kids[2] = (nodeRepPtr)NULL;

  k3 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k3->name = "k3";
  k3->parent = root; 
  k3->typed_value = "";
  k3->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 4); 

  k31 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k31->name = "k31";
  k31->parent = k3; 
  k31->typed_value = "_k31_";
  k31->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k31->kids[0] = (nodeRepPtr)NULL;

  k32 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k32->name = "k32";
  k32->parent = k3; 
  k32->typed_value = "_k32_";
  k32->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k32->kids[0] = (nodeRepPtr)NULL;

  k33 = (nodeRepPtr)malloc(sizeof(struct nodeRepStruct)); 
  k33->name = "k33";
  k33->parent = k3; 
  k33->typed_value = "_k33_";
  k33->kids = (nodeRepPtr *)malloc(sizeof(nodeRepPtr) * 1); 
  k33->kids[0] = (nodeRepPtr)NULL;

  k3->kids[0] = k31;
  k3->kids[1] = k32;
  k3->kids[2] = k33;
  k3->kids[3] = (nodeRepPtr)NULL;

  root->kids[0] = k1;
  root->kids[1] = k2;
  root->kids[2] = k3;
  root->kids[3] = (nodeRepPtr)NULL;

  return (nodeRep)root; 
}

char *name(nodeRep x) { 
  nodeRepPtr n = (nodeRepPtr)x;
  return n->name; 
}

char *typed_value(nodeRep x) { 
  nodeRepPtr n = (nodeRepPtr)x;
  return (n->typed_value); 
}

nodeRepArray children(nodeRep x) { 
  nodeRepPtr n = (nodeRepPtr)x;
  return (nodeRepArray) (n->kids); 
}

nodeRep parent(nodeRep x) { 
  nodeRepPtr n = (nodeRepPtr)x;
  return (nodeRep) (n->parent);
}
