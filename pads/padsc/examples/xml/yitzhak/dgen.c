#include <stdio.h>


#define NUM_ELTS  50

/* 1 Mb */
#define INNER_ARR_SIZE  5

int main(){
  //  int numElts,innerArrSize;
  int i,j;
  for (i=0; i<NUM_ELTS;i++){
    for(j=0; j<INNER_ARR_SIZE-1;j++){
      printf ("1048576:");
    }
    printf("1234567|%d|2|A\n",j,i);
  }


  return 0;
}
