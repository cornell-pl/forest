#include "libpadsc.h"
#include "ai.h"

static const char* meth2str(http_method_t meth) {
  switch(meth) {
  case GET:
    return "GET";
  case PUT:
    return "PUT";
  case POST:
    return "POST";
  case HEAD:
    return "HEAD";
  case DELETE:
    return "DELETE";
  case LINK:
    return "LINK";
  case UNLINK:
    return "UNLINK";
  default:
    break;
  }
  return "* unknown meth *";
}

int main(int argc, char** argv) {
  int             i;
  PDC_t*          pdc;
  http_clf_t      ai;

  if (PDC_ERROR == PDC_open(0, &pdc)) {
    error(2, "*** PDC_open failed ***");
    exit(-1);
  }
  if (PDC_ERROR == PDC_IO_fopen(pdc, "../ex_data.ai", 0)) {
    error(2, "*** PDC_IO_fopen failed ***");
    exit(-1);
  }

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_peek_EOF(pdc, 0)) {
    if (PDC_OK == http_clf_t_read(pdc, 0, 0, &ai, 0)) {
      /* do something with the data */
      if (ai.host.tag == resolved) {
	error(0, "host: %u.%u.%u.%u",
	      ai.host.val.resolved.nIP[0], 
	      ai.host.val.resolved.nIP[1], 
	      ai.host.val.resolved.nIP[2], 
	      ai.host.val.resolved.nIP[3]);
      } else {
	printf("host: ");
	for (i = 0; i < ai.host.val.symbolic.length; i++) {
	  if (i <  ai.host.val.symbolic.length-1) {
	    printf("%-.*s.", ai.host.val.symbolic.sIP[i].len, ai.host.val.symbolic.sIP[i].str);
	  } else {
	    printf("%-.*s\n", ai.host.val.symbolic.sIP[i].len, ai.host.val.symbolic.sIP[i].str);
	  }
	}
      }
      if (ai.remoteID.tag == unauthorized) {
	error(0, "remoteID: -");
      } else {
	error(0, "remoteID: %-.*s", ai.remoteID.val.id.len, ai.remoteID.val.id.str);
      }
      if (ai.auth.tag == unauthorized) {
	error(0, "authid: -");
      } else {
	error(0, "authid: %-.*s", ai.auth.val.id.len, ai.auth.val.id.str);
      }
      error(0, "date: %-.*s", ai.date.len, ai.date.str);
      error(0, "request meth: %s  req_uri: %-.*s  version: %u.%u",
	    meth2str(ai.request.meth),
	    ai.request.req_uri.len, ai.request.req_uri.str,
	    ai.request.version.major,
	    ai.request.version.minor);
      error(0, "response: %u   contentLength: %u", ai.response, ai.contentLength);
      printf("\n");
    } else {
      error(2, "read returned: error");
    }
  }

  if (PDC_ERROR == PDC_IO_fclose(pdc, 0)) {
    error(2, "*** PDC_IO_fclose failed ***");
    exit(-1);
  }

  if (PDC_ERROR == PDC_close(pdc, 0)) {
    error(2, "*** PDC_close failed ***");
    exit(-1);
  }

  return 0;
}
