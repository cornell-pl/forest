#include <stdio.h>


#define NUM_ELTS  1024
#define INNER_ARR_SIZE  1024

int main(int argc,char** argv){
  int numElts = NUM_ELTS,
    innerArrSize = INNER_ARR_SIZE;

  if (argc > 1){
    sscanf(argv[1],"%d",&numElts);
    if (argc > 2)
      sscanf(argv[2],"%d",&innerArrSize);
  }

  fprintf(stderr,"Using %d elements with inner array size %d.\n",numElts,innerArrSize);

  int i,j;
  for (i=0; i<numElts;i++){
    for(j=0; j<innerArrSize-1;j++){
      printf ("10485768:");
    }
    printf("%d|2|A\n",i);
  }


  return 0;
}
