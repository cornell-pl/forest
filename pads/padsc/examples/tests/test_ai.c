#include "pads.h"
#include "ai.h"
#include <ast.h>
#include <error.h>


#define NO_PRINT 1

int main(int argc, char** argv) {
  P_t           *pads;
  http_clf_t_pd   pd;
  http_clf_t      ai;
  http_clf_t_acc  acc;
  http_clf_t_m    m;
  char            *fileName;
  
  if (argc == 2) {
    fileName = argv[1];
  } else {
    fileName = "../../data/ex_data.ai";
  }
  error(0, "Data file = %s\n", fileName);

  if (P_ERR == P_open(&pads, 0, 0)) {
    error(2, "*** P_open failed ***");
    exit(-1);
  }
  if (P_ERR == P_io_fopen(pads, fileName)) {
    error(2, "*** P_io_fopen failed ***");
    exit(-1);
  }
  if (P_ERR == http_clf_t_init(pads, &ai)) {
    error(2, "*** http_clt_t_init failed ***");
    exit(-1);
  }
  if (P_ERR == http_clf_t_pd_init(pads, &pd)) {
    error(2, "*** http_clt_t_pd_init failed ***");
    exit(-1);
  }
  if (P_ERR == http_clf_t_acc_init(pads, &acc)) {
    error(2, "*** http_clt_t_acc_init failed ***");
    exit(-1);
  }

  /* init mask -- must do this! */
  http_clf_t_m_init(pads, &m, P_CheckAndSet);

  /*
   * Try to read each line of data
   */
  while (!P_io_at_eof(pads)) {
    if (P_OK == http_clf_t_read(pads, &m, &pd, &ai)) {
      /* do something with the data */
#ifndef NO_PRINT
      if (ai.host.tag == resolved) {
	error(0, "host: %u.%u.%u.%u",
	      ai.host.val.resolved.nIP[0], 
	      ai.host.val.resolved.nIP[1], 
	      ai.host.val.resolved.nIP[2], 
	      ai.host.val.resolved.nIP[3]);
      } else {
	int i;
	error(0|ERROR_PROMPT, "host: ");
	for (i = 0; i < ai.host.val.symbolic.length; i++) {
	  if (i <  ai.host.val.symbolic.length-1) {
	    error(0|ERROR_PROMPT, "%-.*s.", ai.host.val.symbolic.sIP[i].len, ai.host.val.symbolic.sIP[i].str);
	  } else {
	    error(0, "%-.*s", ai.host.val.symbolic.sIP[i].len, ai.host.val.symbolic.sIP[i].str);
	  }
	}
      }
      if (ai.remoteID.tag == unauthorized) {
	error(0, "remoteID: **unauthorized**");
      } else {
	error(0, "remoteID: %-.*s", ai.remoteID.val.id.len, ai.remoteID.val.id.str);
      }
      if (ai.auth.tag == unauthorized) {
	error(0, "authid: **unauthorized**");
      } else {
	error(0, "authid: %-.*s", ai.auth.val.id.len, ai.auth.val.id.str);
      }
      error(0, "date: %-.*s", ai.date.len, ai.date.str);
      error(0, "request meth: %s  req_uri: %-.*s  version: %u.%u",
	    http_method_t2str(ai.request.meth),
	    ai.request.req_uri.len, ai.request.req_uri.str,
	    ai.request.version.major,
	    ai.request.version.minor);
      error(0, "response: %u   contentLength: %u", ai.response, ai.contentLength);
      error(0, "");
#endif
    } else {
      error(2, "read returned: error");
    }
    /* accum both good and bad vals */
    if (P_ERR == http_clf_t_acc_add(pads, &acc, &pd, &ai)) {
      error(2, "*** http_clt_t_acc_add failed ***");
      exit(-1);
    }	
  }
  http_clf_t_acc_report(pads, "", 0, 0, &acc);
  if (P_ERR == P_io_close(pads)) {
    error(2, "*** P_io_close failed ***");
    exit(-1);
  }

  if (P_ERR == P_close(pads)) {
    error(2, "*** P_close failed ***");
    exit(-1);
  }

  return 0;
}
