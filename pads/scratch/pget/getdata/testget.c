/* Test the get functionality implemented in get.h/get.c */

#include "get.h" 



/*--------------------------------------------------*/
/* GenerateRequests - generate various requests over time */
void *GenerateRequests() {
  int i;

  if (VERBOSE) printf("GenerateRequests() is running\n\n");

  MakeRequest("www.cs.princeton.edu");
  MakeRequest("www.princeton.edu");

  usleep(100000);

  MakeRequest("www.cnn.com");
  MakeRequest("www.nytimes.com");

  usleep(1000000);

  MakeRequest("www.google.com");
  MakeRequest("search.msn.com");

  usleep(100000);

  EndRequests();

}



/*--------------------------------------------------*/
/* test get.c/get.h by making various requests */
int main(int argc, char *argv[]) {

  pthread_t getdata, makerequests;

  InitGetData();

  pthread_create(&getdata, NULL, RunGetData, NULL);
  pthread_create(&makerequests, NULL, GenerateRequests, NULL);

  pthread_join(makerequests, NULL);
  pthread_join(getdata, NULL);

  CleanupGetData();
}
