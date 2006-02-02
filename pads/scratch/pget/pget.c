#include "data.h"


type1 get_type1(char *uri) {

  P_t *pads;

  type1 d;
  type1_pd pd;
  type1_m mask;

  P_open(&pads, 0, 0);
  P_io_fopen(pads,uri);

  type1_init(pads,&d);
  type1_pd_init(pads,&pd);
  type1_m_init(pads,&mask,P_CheckAndSet);
  
  if (P_OK != type1_read(pads,&mask,&pd,&d))
    error(2,"Couldn't read data from %s\n\n", uri);
  
  P_io_close(pads);
  P_close(pads);
  
  return d;
}


type2 get_type2(char *uri) {

  P_t *pads;

  type2 d;
  type2_pd pd;
  type2_m mask;

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
