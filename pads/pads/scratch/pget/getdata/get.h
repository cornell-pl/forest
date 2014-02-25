/* Get remote data in the background as the requests become known
 * - thread 1: calls RunGetData() and loops requesting and processing data
 * - thread 2: make requests for data by calling AddToDoRequest(), call EndRequests() when done
 * Data is stored in files data0, data1, etc 
 */


/* FJP: To Do List
 * Clean up data file naming conventions
 * Add meta data like response time
 * better error handling (esp for errors found by libcurl)
 * Add ability to look up data file by url, time stamp, etc
 */

#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>
#include <pthread.h>
#include "queue.h"      // fjp - fix this, has different interface than sys/queue.h ??

/* maximum number of requests to make at a time */
#define MAX_SIMULTANEOUS_REQUESTS 2
#define VERBOSE 1

/* setup/teardown functions */
int InitGetData(char *dir);
int CleanupGetData();

/* Loop that waits for requests and gets data 
 * terminate loop by calling EndRequests() */
void *RunGetData();

/* Make a request for data from url */
void MakeRequest(char *url);

/* Requests urls listed in file filename */
void RequestsFromFile(char *filename);

/* Terminate request loop */
void EndRequests();





