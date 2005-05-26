/*
 * PADS histogram header file
 * 
 */

#ifndef __PADS_H__
#error "Do not include pads_hist.h directly -- include pads.h instead"
#endif

#ifndef __P_HIST_H__
#define __P_HIST_H__

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "pads.h"

#ifndef INF
#define INF 10000000
#endif

/* Data structure defined for private use only. */
struct wave {
  Pint64 level; // Starting from 0. Log n means the uniform one.
  Pint64 index;	// Starting from 0. Continuous.
  Pfloat64 coef; // The coefficient of the wavelet vector.
};
struct bucket {
  Pint64 bound;	// Start (close) or end point (open) of that bucket.
  Pfloat64 hei; // Height of that bucket.
};
struct dpCell {
  Pfloat64 error; // SSE error in that range.
  Pfloat64 hei;	// Height in that range.
  Pint64 bound;	// Start point (close) of the last bucket. 
};

/* Data structure defined with public access. */

/* Map function maps between a given type and the Pfloat64 type */
typedef Pfloat64 (*Pint_toFloat_fn) (Pint64);
typedef Pint64 (*Pint_fromFloat_fn) (Pfloat64);

struct hist {
  /* These fields can be customized by users */
  Puint64 N; // Dimension of the original data item. 
  Puint32 B; // Number of buckets in the final histogram.
  Pint64 M; // Bound on the range that values can take.
  Pint8 isE; // Buckets required to be of same lenght or not.  
  Pint8 isO; // Result required to be optimal one or near-optimal. Valid when isEqual = f;
  Pint8 n; // Only 1 and 2 are allowed. Specify L1 or L2 norm. Valid when isEqual = f and isOpt = t.
  Pfloat64 e; // Error tolence. For isOpt = f only.
  Pint64 scale; // SCALE factor to make data having proper value.
    
  /* These fields are defined for private use only */
  Puint64 ind; // Index of current data item.
  struct bucket *result; // Result histogram.  
  Pfloat64 partS; // Used in EqualHis.
  Puint64 bukI;	// Used in EqualHis.      
  Puint8 lNumber; // Level Number in wavelet representation.
  Pfloat64 *leftS,*rightS; // Store pipelined information in computing coefficients.
  Puint32 rSize; // Size of robust representation.
  struct wave *top; // Store wavelets with top coefficients.
  Puint32 topI;	// Used in NearOptHis, for top array. 
  struct bucket *bound;	// Used in NearOptHis.
  Puint32 boundI; // Used in NearOptHis, for bound array.      
  struct bucket *rob; // Robust histogram.
  Puint64 robI;	// Used in NearOptHis, for rob array. 
  struct dpCell **dpTable; // Table to compute the dynamic programming.
  Puint32 rowN;	// Used in NearOptHis, for dpTable.
  Puint64 colN;	// Used in NearOptHis, for dpTable.
}; 

typedef struct Pint_hist_s {
  struct hist his_gen;

  Pint_toFloat_fn toFloat; // Map given type to Pfloat64.
  Pint_fromFloat_fn fromFloat; // Map Pfloat64 back to given type.  
} Pint_hist;

typedef Pint_hist Pint8_hist;
typedef Pint_hist Pint16_hist;
typedef Pint_hist Pint32_hist;
typedef Pint_hist Pint64_hist;
typedef Pint_hist Puint8_hist;
typedef Pint_hist Puint16_hist;
typedef Pint_hist Puint32_hist;
typedef Pint_hist Puint64_hist;

/* Functions defined with public access */
Perror_t Pint8_hist_init        (P_t *pads, Pint8_hist *h);
Perror_t Pint8_hist_setConv     (P_t *pads, Pint8_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Pint8_hist_reset       (P_t *pads, Pint8_hist *h);
Perror_t Pint8_hist_cleanup     (P_t *pads, Pint8_hist *h);
Perror_t Pint8_hist_add         (P_t *pads, Pint8_hist *h, Pbase_pd *pd, Pint8 *rep);
Perror_t Pint8_hist_report2io   (P_t *pads, Sfio_t *outstr, Pint8_hist *h);
Perror_t Pint8_hist_report      (P_t *pads, Pint8_hist *h);

Perror_t Pint16_hist_init       (P_t *pads, Pint16_hist *h);
Perror_t Pint16_hist_setConv    (P_t *pads, Pint16_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Pint16_hist_reset      (P_t *pads, Pint16_hist *h);
Perror_t Pint16_hist_cleanup    (P_t *pads, Pint16_hist *h);
Perror_t Pint16_hist_add        (P_t *pads, Pint16_hist *h, Pbase_pd *pd, Pint16 *rep);
Perror_t Pint16_hist_report2io  (P_t *pads, Sfio_t *outstr, Pint16_hist *h);
Perror_t Pint16_hist_report     (P_t *pads, Pint16_hist *h);

Perror_t Pint32_hist_init       (P_t *pads, Pint32_hist *h);
Perror_t Pint32_hist_setConv    (P_t *pads, Pint32_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Pint32_hist_reset      (P_t *pads, Pint32_hist *h);
Perror_t Pint32_hist_cleanup    (P_t *pads, Pint32_hist *h);
Perror_t Pint32_hist_add        (P_t *pads, Pint32_hist *h, Pbase_pd *pd, Pint32 *rep);
Perror_t Pint32_hist_report2io  (P_t *pads, Sfio_t *outstr, Pint32_hist *h);
Perror_t Pint32_hist_report     (P_t *pads, Pint32_hist *h);

Perror_t Pint64_hist_init       (P_t *pads, Pint64_hist *h);
Perror_t Pint64_hist_setConv    (P_t *pads, Pint64_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Pint64_hist_reset      (P_t *pads, Pint64_hist *h);
Perror_t Pint64_hist_cleanup    (P_t *pads, Pint64_hist *h);
Perror_t Pint64_hist_add        (P_t *pads, Pint64_hist *h, Pbase_pd *pd, Pint64 *rep);
Perror_t Pint64_hist_report2io  (P_t *pads, Sfio_t *outstr, Pint64_hist *h);
Perror_t Pint64_hist_report     (P_t *pads, Pint64_hist *h);

Perror_t Puint8_hist_init       (P_t *pads, Puint8_hist *h);
Perror_t Puint8_hist_setConv    (P_t *pads, Puint8_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Puint8_hist_reset      (P_t *pads, Puint8_hist *h);
Perror_t Puint8_hist_cleanup    (P_t *pads, Puint8_hist *h);
Perror_t Puint8_hist_add        (P_t *pads, Puint8_hist *h, Pbase_pd *pd, Puint8 *rep);
Perror_t Puint8_hist_report2io  (P_t *pads, Sfio_t *outstr, Puint8_hist *h);
Perror_t Puint8_hist_report     (P_t *pads, Puint8_hist *h);

Perror_t Puint16_hist_init      (P_t *pads, Puint16_hist *h);
Perror_t Puint16_hist_setConv   (P_t *pads, Puint16_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Puint16_hist_reset     (P_t *pads, Puint16_hist *h);
Perror_t Puint16_hist_cleanup   (P_t *pads, Puint16_hist *h);
Perror_t Puint16_hist_add       (P_t *pads, Puint16_hist *h, Pbase_pd *pd, Puint16 *rep);
Perror_t Puint16_hist_report2io (P_t *pads, Sfio_t *outstr, Puint16_hist *h);
Perror_t Puint16_hist_report    (P_t *pads, Puint16_hist *h);

Perror_t Puint32_hist_init      (P_t *pads, Puint32_hist *h);
Perror_t Puint32_hist_setConv   (P_t *pads, Puint32_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Puint32_hist_reset     (P_t *pads, Puint32_hist *h);
Perror_t Puint32_hist_cleanup   (P_t *pads, Puint32_hist *h);
Perror_t Puint32_hist_add       (P_t *pads, Puint32_hist *h, Pbase_pd *pd, Puint32 *rep);
Perror_t Puint32_hist_report2io (P_t *pads, Sfio_t *outstr, Puint32_hist *h);
Perror_t Puint32_hist_report    (P_t *pads, Puint32_hist *h);

Perror_t Puint64_hist_init      (P_t *pads, Puint64_hist *h);
Perror_t Puint64_hist_setConv   (P_t *pads, Puint64_hist *h, Pint_toFloat_fn to, Pint_fromFloat_fn from);
Perror_t Puint64_hist_reset     (P_t *pads, Puint64_hist *h);
Perror_t Puint64_hist_cleanup   (P_t *pads, Puint64_hist *h);
Perror_t Puint64_hist_add       (P_t *pads, Puint64_hist *h, Pbase_pd *pd, Puint64 *rep);
Perror_t Puint64_hist_report2io (P_t *pads, Sfio_t *outstr, Puint64_hist *h);
Perror_t Puint64_hist_report    (P_t *pads, Puint64_hist *h);

/* Functions defined for private use only */
Perror_t EqualHis    (struct hist *h, Pfloat64 d);
Perror_t OptHis      (struct hist *h, Pfloat64 d);
Perror_t NearOptHis  (struct hist *h, Pfloat64 d);

Perror_t Pint_print  (Pint_hist *h, Sfio_t *outstr);

struct dpCell OptHei (struct hist *h, Puint64 s, Puint64 e); // Optimal height between A[s...e].
Puint64 partition_w  (struct wave** A, Puint64 p, Puint64 r); // Rearrange the array A[p...r].
Puint64 partition_b  (struct bucket** A, Puint64 p, Puint64 r); // Override function.
void select_w        (struct wave** A, struct hist* h, Puint64 p, Puint64 r); // Drops the least half numbers.   
Pfloat64 select_b    (struct bucket** A, struct hist* h, Puint64 p, Puint64 r, Puint64 sel); // Return element at sel rank; 
void quickSort       (struct bucket** A, Puint64 p, Puint64 q); // Sort array A based on end point.
void buildRob        (struct hist *h); // Build robust histograms from top coefficients. 
void compOpt         (struct hist *h); // Use dynamic programming to get the optimal one from rob array.

#endif /*  __P_HIST_H__  */
