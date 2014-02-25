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

  MakeRequest("http://www.cnnnnnn.com");
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

  pthread_t getdata;

  InitGetData("data");

  /* create a thread to get data in the background */
  pthread_create(&getdata, NULL, RunGetData, NULL);
  
  /* make requests for data */
  GenerateRequests();

  /* wait for the get data thread to complete */
  pthread_join(getdata, NULL);

  CleanupGetData();
}
