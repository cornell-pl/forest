#include "libpadsc.h"
#include "ai.h"
#include <ast.h>
#include <error.h>

int main(int argc, char** argv) {
  int             i;
  PDC_t           *pdc;
  http_clf_t_ed   ed;
  http_clf_t      ai;
  http_clf_t_acc  acc;
  char            *fileName;
  
  if (argc == 2) {
    fileName = argv[1];
  } else {
    fileName = "../data/ex_data.ai";
  }
  error(0, "Data file = %s\n", fileName);

  if (PDC_ERR == PDC_open(&pdc, 0, 0)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERR == PDC_IO_fopen(pdc, fileName)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }
  if (PDC_ERR == http_clf_t_init(pdc, &ai)) {
    error(2, "*** http_clt_t_init failed ***");
    exit(-1);
  }
  if (PDC_ERR == http_clf_t_ed_init(pdc, &ed)) {
    error(2, "*** http_clt_t_ed_init failed ***");
    exit(-1);
  }
  if (PDC_ERR == http_clf_t_acc_init(pdc, &acc)) {
    error(2, "*** http_clt_t_acc_init failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (PDC_OK == http_clf_t_read(pdc, 0, &ed, &ai)) {
      /* do something with the data */
      if (ai.host.tag == resolved) {
	error(0, "host: %u.%u.%u.%u",
	      ai.host.val.resolved.nIP[0], 
	      ai.host.val.resolved.nIP[1], 
	      ai.host.val.resolved.nIP[2], 
	      ai.host.val.resolved.nIP[3]);
      } else {
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

      if (PDC_ERR == http_clf_t_acc_add(pdc, &acc, &ed, &ai)) {
	error(2, "*** http_clt_t_acc_add failed ***");
	exit(-1);
      }	

    } else {
      error(2, "read returned: error");
    }
  }
  http_clf_t_acc_report(pdc, "", 0, 0, &acc);
  if (PDC_ERR == PDC_IO_fclose(pdc)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
