#include "get.h"
#include <assert.h>
#include <sys/stat.h>


/*-----------------------------------------------------------------*/
/* Types */

/* information about a request */
typedef struct RequestInfo {
  TAILQ_ENTRY(RequestInfo)entries;  /* next/previous pointers */
  CURL *handle;                      /* curl handle */
  char *url;                         /* url */
  char *filename;                    /* filename for resulting data */
  FILE *stream;                      /* temporary space for received data */
  int requestNumber;                 /* unique id */
} RequestInfo;





/*-----------------------------------------------------------------*/
/* Global variables */


/* directory for result files */
char *resultdir;

/* global lock - yes I'm lazy, can break this up later 
 * lock all accesses to request queues, queue counts, and done_requesting flag */
pthread_mutex_t lock;

/* request queues - accesses must be locked */
TAILQ_HEAD(ToDo,RequestInfo) toDo;                /* requests to be made */
TAILQ_HEAD(InProgress,RequestInfo) inProgress;    /* reqeusts in progress, part of multi_handle */
TAILQ_HEAD(Completed,RequestInfo) completed;      /* complete requests -- sucessful or error */

/* request counts - accesses must be locked */
int numInProgress = 0;
int numToDo = 0;
int numCompleted = 0;
int totalRequests = 0;

/* the request loop continues while this value is 0  - accesses must be locked */
int done_requesting = 0; 

/* libcurl multi handle */
CURLM *multi_handle;  /* multi handle containing all requests in progress */
int still_running;    /* number of transfers in multi handle that are still running */




/*-----------------------------------------------------------------*/
/* PrintQueueContents - print the contents of the 3 request queues */

void PrintQueueContents() {
  RequestInfo *ri;

  pthread_mutex_lock(&lock);
  printf("To Do: ");
  TAILQ_FOREACH(ri,&toDo,entries) {
    printf(ri->url);
    printf(" ");
  }
  printf("\nIn Progress: ");
  TAILQ_FOREACH(ri,&inProgress,entries) {
    printf(ri->url);
    printf(" ");
  }
  printf("\nCompleted: ");
  TAILQ_FOREACH(ri,&completed,entries) {
    printf(ri->url);
    printf(" ");
  }
  printf("\n\n");
  pthread_mutex_unlock(&lock);
}




/*-----------------------------------------------------------------*/
/* Make Request - add a request for url to the toDo queue  */
void MakeRequest(char *url) {
  RequestInfo *ri;
  char *filename;

  /* build new RequestInfo */
  ri = malloc(sizeof(RequestInfo));
  ri->url = url;
  ri->requestNumber = totalRequests++;
  ri->stream = NULL;

  /* build file name for result */
  filename  = malloc(256);
  sprintf(filename, "data%d", ri->requestNumber);
  ri->filename = filename;

  /* add new request to the toDo queue */
  pthread_mutex_lock(&lock);
  TAILQ_INSERT_TAIL(&toDo, ri, entries);
  numToDo++;
  pthread_mutex_unlock(&lock);

  if (VERBOSE) printf("** Added: %s \n\n", url);
}


/*-----------------------------------------------------------------*/
/* EndRequests - notify the main loop that no more requests will be made */
void EndRequests() {

  pthread_mutex_lock(&lock);
  done_requesting = 1;
  pthread_mutex_unlock(&lock);

  if (VERBOSE) printf("** Done Adding Requests to ToDo.\n\n");

}


/*-----------------------------------------------------------------*/
/* StrdupLower - make a lowercase version of a string
 * Adapted from /n/fs/fac/vivek/codeen/comon/applib.c */
char *StrdupLower(const char *orig)
{
  char *temp;
  int i;

  if ((temp = strdup(orig)) == NULL) {
    fprintf(stderr, "error: error in strdup");
    exit(-1); 
  }
  for (i = 0; temp[i]; i++) {
    if (isupper((int) temp[i]))
      temp[i] = tolower(temp[i]);
  }
  return(temp);
}


/*-----------------------------------------------------------------*/
/* GetNextLineBack - reads the next non-blank line of the file. 
 * strips off any leading space, any comments, and any trailing space.
 * returns a lowercase version of the line that has been malloc'd 
 * Adapted from /n/fs/fac/vivek/codeen/comon/applib.c */
static char *GetNextLineBack(FILE *file, int lower, int stripComments)
{

  char line[1024];

  while (fgets(line, sizeof(line), file) != NULL) {
    char *temp;
    int len;

    /* strip off any comments, leading and trailing whitespace */
    if (stripComments) {
      if ((temp = strchr(line, '#')) != NULL)
        *temp = 0;
    }
    len = strlen(line);
    while (len > 0 && isspace((int) line[len-1])) {
      len--;
      line[len] = 0;
    }
    temp = line;
    while (isspace((int) *temp))
      temp++;
    if (temp[0] == 0)
      continue;                 /* blank line, move on */

    if (lower)
      return(StrdupLower(temp));
    return(strdup(temp));
  }

  return(NULL);
}

/*-----------------------------------------------------------------*/
/* PopulateToDoQueue -- read urls from a file and add them to ToDo Queue 
 * Adapted from /n/fs/fac/vivek/codeen/comon/monall.c */
void RequestsFromFile(char *filename) {

  FILE *list;
  char *temp;
  int numRequests = 0;

  if ((list = fopen(filename, "r")) == NULL) {
    fprintf(stderr, "error: couldn't open %s\n", filename);
    exit(-1);
  }

  while ((temp = GetNextLineBack(list,1,1)) != NULL) {
    numRequests++;
    MakeRequest(temp);
  }
  if (numRequests < 1) {
    fprintf(stderr, "couldn't find requests\n");
    exit(-1);
  }
  fclose(list);
}


/*-----------------------------------------------------------------*/
/* WriteDataToFile - write data received on a handle into its file */
size_t WriteDataToFile(void *buffer, size_t size, size_t nmemb, void *stream){

  RequestInfo *out_ri = (RequestInfo *)stream;
  
  if(out_ri && !out_ri->stream) {

    /* open file for writing */
    out_ri->stream = fopen(out_ri->filename, "wb");
    if(!out_ri->stream) {
      fprintf(stderr, "Error opening file %s\n", out_ri->filename);
      exit(-1);
    }
  }
  return fwrite(buffer, size, nmemb, out_ri->stream);
}      

/*-----------------------------------------------------------------*/
/* RequestComplete - determine which request is associated with the 
 * provided handle, and move that request to the completed queue */
void RequestCompleted(CURL *handle) {

  RequestInfo *ri;
  int found = 0;

  /* walk through the inprogress queue until we find the matching handle */
  pthread_mutex_lock(&lock);
  TAILQ_FOREACH(ri,&inProgress,entries) {
    if (ri->handle == handle) {

      curl_multi_remove_handle(multi_handle,handle);

      TAILQ_REMOVE(&inProgress, ri, entries);
      numInProgress--;

      TAILQ_INSERT_TAIL(&completed, ri, entries);
      numCompleted++;

      /* close opened files (not opened if no data returned in error case) */
      if(ri->stream) fclose(ri->stream);

      found = 1;
      break;
    }
  }
  pthread_mutex_unlock(&lock);

  if (VERBOSE) PrintQueueContents();
  
  if (!found) {
    fprintf(stderr, "error: completed handle not found in InProgress queue.\n");
    exit(-1);
  }
}

/*-----------------------------------------------------------------*/
/* HandleMessages - walk through message list and handle each */
void HandleMessages() {

  CURLMsg *msg;
  int msgs_left;

  /* look through all messages */
  while ((msg = curl_multi_info_read(multi_handle, &msgs_left))) {

    char *url;
    curl_easy_getinfo(msg->easy_handle,CURLINFO_PRIVATE, &url);

    /* move completed requests to the completed queue */
    if (msg->msg == CURLMSG_DONE) {

      /* fjp -- eventually do something smarter here */
      if (msg->data.result != CURLE_OK) {
        fprintf(stderr, "error: %d, %s <%s>\n", msg->data.result, 
                curl_easy_strerror(msg->data.result), 
                url);
      }
      RequestCompleted(msg->easy_handle);
     }

    /* fjp -- eventually do something smarter here */
    else {
      fprintf(stderr,"error: unhandled curl message %d", msg->msg);
    }
  }
}


/*-----------------------------------------------------------------*/
/* AddMoreRequests - if there are fewer than the maximum number of 
 * requests currently running and there are more in the todo queue
 * add more requests until the max number is reached */
void AddMoreRequests() {

  CURL *handle;
  RequestInfo *ri;

  /* keep making requests until the max is reached or we run out */
  while (numInProgress < MAX_SIMULTANEOUS_REQUESTS
         && TAILQ_FIRST(&toDo) != NULL) {
    
    /* dequeue from todo queue */
    pthread_mutex_lock(&lock);
    ri = TAILQ_FIRST(&toDo);
    TAILQ_REMOVE(&toDo, ri, entries);
    numToDo--;
    pthread_mutex_unlock(&lock);

    /* add to inprogress */
    pthread_mutex_lock(&lock);
    TAILQ_INSERT_TAIL(&inProgress,ri,entries);
    numInProgress++;
    pthread_mutex_unlock(&lock);

    /* create curl handle */
    ri->handle = curl_easy_init();
    if (!ri->handle) {printf("ERROR");}

    /* set url to fetch */
    curl_easy_setopt(ri->handle, CURLOPT_URL, ri->url);

    /* receieved data should be handled by WriteDataToFile() */
    curl_easy_setopt(ri->handle, CURLOPT_WRITEFUNCTION, WriteDataToFile);

    /* pass ri as argument to writing function */
    curl_easy_setopt(ri->handle, CURLOPT_WRITEDATA, ri);

    /* save url in accessible field for later error messages */
    curl_easy_setopt(ri->handle, CURLOPT_PRIVATE, ri->url);

    /* follow http redirects -- ie. www.princeton.edu */
    curl_easy_setopt(ri->handle, CURLOPT_FOLLOWLOCATION, 1);

    /* fjp - may want to set more easy options? */

    /* add to multihandle */
    pthread_mutex_lock(&lock);
    curl_multi_add_handle(multi_handle, ri->handle);
    pthread_mutex_unlock(&lock);

    if (VERBOSE) PrintQueueContents();
  }
}


/*-----------------------------------------------------------------*/
/* RunGetData - main loop for getting data
 * Continues as long as the following criteria are met:
 * - there are still request in the toDo queue
 * - there are still reqeusts in progress
 * - the flag done_requesting is not set
 * fjp - optimize performance at all by restructuring?
 * fjp - don't understand best way to set select timeout values
 */ 
void *RunGetData() {

  fd_set fdread, fdwrite, fdexcep;
  int maxfd;
  struct timeval tv;
  long timeout;   

  if (VERBOSE) printf("\nRunGetData() is running\n\n");

  /* loop until three criteria are met */
  while( !done_requesting || (numToDo > 0) || (numInProgress > 0) ) {

    /* do read/writes available on each handle in multi_handle */
    while(CURLM_CALL_MULTI_PERFORM ==
          curl_multi_perform(multi_handle, &still_running));


    if (still_running) {
      
      FD_ZERO(&fdread);
      FD_ZERO(&fdwrite);
      FD_ZERO(&fdexcep);
      
      if (curl_multi_fdset(multi_handle, &fdread, &fdwrite, &fdexcep, &maxfd)) {
        fprintf(stderr, "error: curl_multi_fdset\n");
        exit(-1);
      }
      
      /* fjp - this is in sample code, but fails to link with curl libraries? 
      if (curl_multi_timeout(multi_handle,&tv)) {
        fprintf(stderr, "error: curl_multi_timeout\n");
        exit(-1);
      }
      */
      
      tv.tv_sec = timeout/1000;
      tv.tv_usec = (timeout%1000)*1000;
      
      if (0 > select(maxfd+1, &fdread, &fdwrite, &fdexcep, &tv)) {
        fprintf(stderr, "error: select\n");
        exit(-1);
      }
    }
    
    /* Handle any curl messages (ie. completion) */
    HandleMessages();

    /* Add more requests if there's space */
    AddMoreRequests();

  } /* end main loop */

  if (VERBOSE) {
    printf("Final Contents:\n");
    PrintQueueContents();
  }

  /* double check we're really done */
  assert((numToDo == 0) && (numInProgress == 0));
  assert(numCompleted = totalRequests);

  if (VERBOSE) printf("YAY!!!!!\n\n");

  exit(0);
}



/*-----------------------------------------------------------------*/
/* InitGetData - initialize queues, curl handles, lock */
int InitGetData(char *dir) {

  /* initialize queues */
  TAILQ_INIT(&toDo);
  TAILQ_INIT(&inProgress);
  TAILQ_INIT(&completed);


  /* init curl stuff */
  curl_global_init(CURL_GLOBAL_ALL);
  multi_handle = curl_multi_init();

  /* init lock */
  pthread_mutex_init(&lock, NULL);

  /* create directory for results, cd there */
  resultdir = dir;
  // fjp -- should check if it already exists? check errno?
  mkdir(resultdir, S_IRWXU | S_IRGRP | S_IWGRP | S_IXGRP | S_IROTH | S_IXOTH); 
  if (chdir(resultdir)) {
    fprintf(stderr, "error: could not change to %s\n",resultdir);
    exit(-1);
  }
}


/*-----------------------------------------------------------------*/
/* CleanupGetData - cleanup queues, curl stuff, etc */
int CleanupGetData() {

  /* fjp - figure out how to pass meta data back before it disappears... */

  /* fjp - free easy handles */
  /* fjp - free queues */

  /* cleanup  multi handle */
  curl_multi_cleanup(multi_handle);
  curl_global_cleanup();

  pthread_mutex_destroy(&lock);
}


