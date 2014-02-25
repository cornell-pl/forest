#include "data.h"

#define GENERATE_GET_FUNCTION(T) \
T getLocal_ ## T (char *uri) { \
  P_t *pads; \
  T d; \
  T##_pd pd; \
  T##_m mask; \
  P_open(&pads, 0, 0); \
  P_io_fopen(pads,uri); \
  T##_init(pads,&d); \
  T##_pd_init(pads,&pd); \
  T##_m_init(pads,&mask,P_CheckAndSet); \
  if (P_OK != T##_read(pads,&mask,&pd,&d)) \
    error(2,"Couldn't read data from %s\n\n", uri); \
  P_io_close(pads); \
  P_close(pads); \
  return d; \
}


GENERATE_GET_FUNCTION(type1);
GENERATE_GET_FUNCTION(type2);

/*
type2 getRemote_type2(char *uri) {

  P_t *pads;

  type2 d;
  type2_pd pd;
  type2_m mask;


  // get data and put in temp directory 
  char *tempdir = "temp/";
  system("wget -p %s %s", tempdir, uri);

  // read in data 
  P_open(&pads, 0, 0);
  P_io_fopen(pads,uri);

  type2_init(pads,&d);
  type2_pd_init(pads,&pd);
  type2_m_init(pads,&mask,P_CheckAndSet);
  
  if (P_OK != type2_read(pads,&mask,&pd,&d))
    error(2,"Couldn't read data from %s\n\n", uri);
  
  P_io_close(pads);
  P_close(pads);
  
  return d;
}

*/
