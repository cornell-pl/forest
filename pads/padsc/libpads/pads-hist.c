/*
 * histogram implementation
 * 
 */

#include "pads-internal.h"

/* Default mapping functions, can be overwritten by users */
Pfloat64 Pint_to (Pint64 i) { return (Pfloat64)i; }
Pint64 Pint_from (Pfloat64 f) { return (Pint64)f; }

/* Begin Macro */
#define TYPE_HIST_GEN(type) \
\
Perror_t type ## _hist_init (P_t *pads, type ## _hist *h) { \
  Puint32 i; \
  Puint64 adj; \
\
  /* Parameter Checking */ \
  if (h->his_gen.n != 1 && h->his_gen.n != 2) return P_ERR; \
  if (h->his_gen.n == 1) { if (h->his_gen.isO == 0 || h->his_gen.isE != 0) return P_ERR; } \
\
  /* Initialize */ \
  h->toFloat = Pint_to; \
  h->fromFloat = Pint_from; \
  h->his_gen.ind = 0; \
  h->his_gen.result = malloc(h->his_gen.B * sizeof(struct bucket)); \
  if (h->his_gen.result == (struct bucket*)0) exit(-1); \
  if (h->his_gen.isE != 0) { \
    /* Equally spaced */ \
    h->his_gen.partS = 0; \
    h->his_gen.bukI = 0; \
    for (i = 0; i < h->his_gen.B; i++) h->his_gen.result[i].bound = ((Pint64)(h->his_gen.N / h->his_gen.B) + 1) * (i + 1); \
    h->his_gen.result[h->his_gen.B - 1].bound = h->his_gen.N; \
  } \
  else { \
    if (h->his_gen.isO != 0) { \
      /* Optimal result required */ \
      h->his_gen.rob = malloc(h->his_gen.N * sizeof(struct bucket)); \
      if (h->his_gen.rob == (struct bucket*)0) exit(-1); \
      h->his_gen.robI = 0; \
    } \
    else { \
      /* Adjust dimension to be perfect power of 2. */ \
      adj = (Puint64)((Pfloat64)log(h->his_gen.N) / (Pfloat64)log(2)); \
      if ((Puint64)pow(2, (Pfloat64)adj) != h->his_gen.N) h->his_gen.N = (Puint64)pow(2, (Pfloat64)adj + 1); \
\
      h->his_gen.lNumber = (Puint8)((Pfloat64)log(h->his_gen.N) / (Pfloat64)log(2));  \
      h->his_gen.leftS = malloc(h->his_gen.lNumber * sizeof(Pfloat64)); \
      h->his_gen.rightS = malloc(h->his_gen.lNumber * sizeof(Pfloat64)); \
      if (h->his_gen.leftS == (Pfloat64*)0 || h->his_gen.rightS == (Pfloat64*)0) exit(-1); \
      h->his_gen.rSize = (Puint32)((Pfloat64)(h->his_gen.B * ((Pfloat64)log(h->his_gen.N) / (Pfloat64)log(2)) * ((Pfloat64)log(h->his_gen.M) / (Pfloat64)log(2))) / h->his_gen.e); \
      h->his_gen.top = malloc(2 * h->his_gen.rSize * sizeof(struct wave)); \
      if (h->his_gen.top == (struct wave*)0) exit(-1); \
      h->his_gen.topI = 0; \
      h->his_gen.bound = malloc(3 * h->his_gen.rSize * sizeof(struct bucket)); \
      if (h->his_gen.bound == (struct bucket*)0) exit(-1); \
      h->his_gen.boundI = 0; \
      h->his_gen.rob = malloc((3 * h->his_gen.rSize + 1) * sizeof(struct bucket)); \
      if (h->his_gen.rob == (struct bucket*)0) exit(-1); \
      h->his_gen.rob[0].bound = 0; \
      h->his_gen.rob[0].hei = 0; \
      h->his_gen.robI = 1; \
    } \
  } \
  return P_OK; \
} \
\
Perror_t type ## _hist_reset (P_t *pads, type ## _hist *h) { \
  h->his_gen.ind = 0; \
  if (h->his_gen.isE != 0) { \
    /* Equally spaced */ \
    h->his_gen.partS = 0; \
    h->his_gen.bukI = 0; \
  } \
  else { \
    if (h->his_gen.isO != 0) h->his_gen.robI = 0; \
    else { \
      h->his_gen.topI = 0; \
      h->his_gen.boundI = 0; \
      h->his_gen.robI = 1; \
    } \
  } \
  return P_OK; \
} \
\
Perror_t type ## _hist_cleanup (P_t *pads, type ## _hist *h) { \
  free(h->his_gen.result); \
  if (h->his_gen.isE == 0) { \
    free(h->his_gen.rob); \
    if (h->his_gen.isO == 0) { \
      free(h->his_gen.leftS); \
      free(h->his_gen.rightS); \
      free(h->his_gen.top); \
      free(h->his_gen.bound); \
    } \
  } \
  return P_OK; \
} \
\
Perror_t type ## _hist_add (P_t *pads, type ## _hist *h, Pbase_pd *pd, type *rep) { \
  Pfloat64 d; \
  Perror_t res; \
\
  d = (*h->toFloat)((*rep)) / (Pfloat64)h->his_gen.scale; \
  res = P_OK; \
  if (h->his_gen.isE != 0) res = EqualHis(&(h->his_gen), d); \
  else { \
    if (h->his_gen.isO != 0) res = OptHis(&(h->his_gen), d); \
    else res = NearOptHis(&(h->his_gen), d); \
  } \
  h->his_gen.ind++; \
\
  if (h->his_gen.ind == h->his_gen.N) { \
    /* The last element in the scope */ \
    if (h->his_gen.isE == 0) { \
      if (h->his_gen.isO == 0) buildRob(&(h->his_gen)); \
      compOpt(&(h->his_gen)); \
    } \
    if(res == P_OK) res =  type ## _hist_report(pads, h); \
    if(res == P_OK) res = type ## _hist_reset(pads, h); \
  } \
  return res; \
} \
\
Perror_t type ## _hist_report (P_t *pads, type ## _hist *h) { \
  Perror_t res; \
  Puint64 i; \
  Puint64 tempInd; \
\
  res = P_OK; \
  if (h->his_gen.ind != h->his_gen.N) { \
    /* Real data is less than the estimated dimension */ \
    if (h->his_gen.isE != 0) { \
      h->his_gen.result[h->his_gen.bukI].hei = h->his_gen.partS; \
      h->his_gen.result[h->his_gen.bukI].bound = h->his_gen.ind; \
      h->his_gen.bukI++; \
    } \
    else { \
      if (h->his_gen.isO == 1) { \
	h->his_gen.robI = h->his_gen.ind; \
	h->his_gen.N = h->his_gen.ind; \
	if (h->his_gen.B > h->his_gen.robI) h->his_gen.B = h->his_gen.robI; \
	compOpt(&(h->his_gen)); \
      } \
      else { \
	tempInd = h->his_gen.ind; \
	for (i = h->his_gen.ind; i < h->his_gen.N; i++) res = NearOptHis(&(h->his_gen), 0); \
	buildRob(&(h->his_gen)); \
	h->his_gen.ind = tempInd; \
	compOpt(&(h->his_gen)); \
      } \
    } \
  } \
  printf("*** Histogram Result *** \n"); \
  if (h->his_gen.isE == 0) h->his_gen.bukI = h->his_gen.B; \
  for (i = 0; i < h->his_gen.bukI; i++) { \
    if (i == 0) printf("From %d to ", 0); \
    else printf("From %d to ", h->his_gen.result[i-1].bound); \
    printf("%d, with height ", h->his_gen.result[i].bound - 1); \
    printf("%d. \n", (*h->fromFloat)(h->his_gen.result[i].hei * (Pfloat64)h->his_gen.scale)); \
  } \
  return res; \
} 

/* END_MACRO */

/* Functions defined with public access */
TYPE_HIST_GEN(Pint8);
TYPE_HIST_GEN(Pint16);
TYPE_HIST_GEN(Pint32);
TYPE_HIST_GEN(Pint64);
TYPE_HIST_GEN(Puint8);
TYPE_HIST_GEN(Puint16);
TYPE_HIST_GEN(Puint32);
TYPE_HIST_GEN(Puint64);

/* Functions defined for private use only */ 
Perror_t EqualHis(struct hist *h, Pfloat64 d) { 
  Puint64 i;

  if (h->ind == h->result[h->bukI].bound || h->ind == h->N - 1) {
    /* Reach a boundary, height computed, end point recorded */
    h->result[h->bukI].hei = h->partS;
    h->bukI++;
    h->partS = 0;
  }
  else {
    if (h->partS == 0) h->partS = d;
    else {
      if (h->bukI == 0) h->partS = (h->partS * h->ind + d) / (h->ind + 1);
      else {
	i = h->ind - h->result[h->bukI - 1].bound;
	h->partS = (h->partS * i + d) / (i + 1);
      }
    }
  }
  return P_OK;
}

Perror_t OptHis(struct hist *h, double d) {
  h->rob[h->ind].bound = h->ind;
  h->rob[h->ind].hei = d;
  h->robI++;
  return P_OK;
}

Perror_t NearOptHis(struct hist *h, double d) {
  Puint8 i;
  Puint8 tempCoefI;
  Puint64 rem;
  struct wave temp;
  struct wave* tempCoef;

  /* Compute coefficients */
  tempCoef = malloc((h->lNumber + 1) * sizeof(struct wave));
  if (tempCoef == (struct wave*)0) exit(-1);
  tempCoefI = 0;
  for (i = 0; i < h->lNumber; i++) {
    rem = h->ind % (Pint64)pow(2, (Pfloat64)i+1);
    if (rem == 0) {
      /* Hit the left half of a new wavelet on ith. level */
      if (h->ind != 0) {
	/* Roll back current wavelet vector */
	temp.level = i;
	temp.index = h->ind / (Puint64)pow(2, (Pfloat64)i+1);
	temp.coef = (h->rightS[i] - h->leftS[i]) / pow(2, ((Pfloat64)i+1)/2);
	tempCoef[tempCoefI] = temp;
	tempCoefI++;
      }
      h->leftS[i] = d;
    }
    if (rem == (Puint64)pow(2, (Pfloat64)i)) {
      /* Hit the right half of current wavelet on ith. level */
      h->rightS[i] = d;
    }
    if (rem > 0 && rem < (Puint64)pow(2, (Pfloat64)i)) {
      /* Keep on the left half of current wavelet on ith. level */
      h->leftS[i] += d;
    }
    if (rem > (Puint64)pow(2, (Pfloat64)i)) {
      /* Keep on the right half of current wavelet of ith. level */
      h->rightS[i] += d;
      if (h->ind == h->N - 1) {
	/* The last element, roll up */
	temp.level = i;
	temp.index = h->ind / (Puint64)pow(2, (Pfloat64)i+1);
	temp.coef = (h->rightS[i] - h->leftS[i]) / pow(2, ((Pfloat64)i+1)/2);
	tempCoef[tempCoefI] = temp;
	tempCoefI++;
	if (i == h->lNumber - 1) {
	  /* Add the uniform wavelet vector */
	  temp.level = i + 1;
	  temp.index = 0;
	  temp.coef = (h->rightS[i] + h->leftS[i]) / sqrt((Pfloat64)(h->N));
	  tempCoef[tempCoefI] = temp;
	  tempCoefI++;
	}
      }
    }
  }
  /* Examine and remain wavelet vectors with large coefficient only */
  for (i = 0; i < tempCoefI; i++) {
    h->top[h->topI] = tempCoef[i];
    h->topI++;
    if (h->topI == 2 * h->rSize) {
      /* The array is full. Prune required */
      select_w(&(h->top), h, 0, 2 * h->rSize - 1);
      h->topI = h->rSize;
    }
  }
  free(tempCoef);
  return P_OK;
}

void buildRob(struct hist *h) {
  Puint32 i;
  Puint64 left;
  Puint64 right;
  Puint64 mid;
  Pfloat64 hei;
  Puint64 currB;
  Pfloat64 currH;
  struct wave temp;

  select_w(&(h->top), h, 0, 2 * h->rSize - 1);
  /* Build the robust histogram out of the top array, start point recorded */
  for (i = 0; i < h->rSize; i++) {
    if (temp.level == h->lNumber) {
      /* The uniform one */
      h->bound[h->boundI].bound = 0;
      h->bound[h->boundI].hei = temp.coef / sqrt((Pfloat64)(h->N));
      h->boundI++;
    }
    else {
      left = temp.index * (Puint64)pow(2, (Pfloat64)(temp.level + 1));
      right = (temp.index + 1) * (Puint64)pow(2, (Pfloat64)(temp.level + 1));
      mid = (Puint64)(left + right) / 2;
      hei = temp.coef * pow(2, -((Pfloat64)temp.level+1) / 2);

      h->bound[h->boundI].bound = left;
      h->bound[h->boundI].hei = -hei;
      h->boundI++;
      h->bound[h->boundI].bound = mid;
      h->bound[h->boundI].hei = 2 * hei;
      h->boundI++;
      if (right != h->N) {
	h->bound[h->boundI].bound = right;
	h->bound[h->boundI].hei = -hei;
	h->boundI++;
      }
    }
  }
  quickSort(&h->bound, 0, h->boundI-1);

  currB = h->bound[0].bound;
  currH = h->bound[0].hei;
  h->robI = 0;
  for (i = 1; i < h->boundI; i++) {
    if (h->bound[i].bound != currB) {
      /* Finish height accumulation */
      h->rob[h->robI].bound = currB;
      h->rob[h->robI].hei = currH;
      h->robI++;
      currB = h->bound[i].bound;
    }
    currH += h->bound[i].hei;
  }
  h->rob[h->robI].bound = currB;
  h->rob[h->robI].hei = currH;
  h->robI++;
  h->rob[h->robI].bound = h->N;
}

struct dpCell OptHei(struct hist *h, Puint64 s, Puint64 e) {
  Puint64 len;
  Puint64 i;
  Pfloat64 hei;
  Pfloat64 err;
  Pfloat64 sum;
  struct dpCell temp_dp;
  struct bucket *temp;

  if (h->n == 1) {
    /* L-1 norm */
    len = e - s + 1;
    temp = malloc(len * sizeof(struct bucket));
    if (temp == (struct bucket*)0) exit(-1);
    for (i = 0; i < len; i++) temp[i] = h->rob[s + i];
    hei = 0;
    if ((len % 2) == 0) hei = (select_b(&temp, h, 0, len-1, len/2) + select_b(&temp, h, 0, len-1, len/2-1 )) / 2;
    else hei = select_b(&temp, h, 0, len-1, (len-1)/2);
    free(temp);
    err = 0;
    for (i = s; i < e+1; i++) err += fabs(h->rob[i].hei - hei);
    temp_dp.error = err;
    temp_dp.hei = hei;
    return temp_dp;
  }

  /* L-2 norm. Compute the average */
  sum = 0;
  len = 0;
  for (i = s; i < e; i++) {
    len += h->rob[i+1].bound - h->rob[i].bound;
    sum += h->rob[i].hei * (h->rob[i+1].bound - h->rob[i].bound);
  }
  if (e == h->robI - 1) {
    len += h->N - h->rob[e].bound;
    sum += h->rob[e].hei * (h->N - h->rob[e].bound);
  }
  else {
    len += h->rob[e+1].bound - h->rob[e].bound;
    sum += h->rob[e].hei * (h->rob[e+1].bound - h->rob[e].bound);
  }
  hei = sum / (Pfloat64)len;

  /* Compute the error */
  err = 0;
  for (i = s; i < e; i++) err += (h->rob[i+1].bound - h->rob[i].bound) * pow(h->rob[i].hei - hei, 2);
  if (e == h->robI - 1)	err += (h->N - h->rob[e].bound) * pow(h->rob[e].hei - hei, 2);
  else 	err += (h->rob[e+1].bound - h->rob[e].bound) * pow(h->rob[e].hei - hei, 2);

  temp_dp.error = err;
  temp_dp.hei = hei;
  return temp_dp;
}

void compOpt(struct hist *h) {
  Puint64 i;
  Puint64 j;
  Puint64 k;
  Pfloat64 keepE;
  Puint64 keepB;
  Pfloat64 keepH;
  Pfloat64 err;
  Puint32 resultI;
  Puint64 track;
  struct dpCell temp;

  /* Size of the dynamic programming table */
  if (h->isO != 0) h->colN = h->N;
  else {
    if (h->ind != h->N) {
      i = 0;
      while (i < h->robI && h->rob[i].bound < h->ind) i++;
      h->robI = i;
    }
    if (h->B > h->robI) h->B = h->robI;
    h->colN = h->robI;
  }
  h->rowN = h->B;
  h->dpTable = malloc(h->rowN * sizeof(struct dpCell*));
  if (h->dpTable == (struct dpCell**)0) exit(-1);
  for (i = 0; i < h->rowN; i++) {
    h->dpTable[i] = malloc(h->colN * sizeof(struct dpCell));
    if (h->dpTable[i] == (struct dpCell*)0) exit(-1);
  }

  /* Histogram with one bucket only */
  for (i = 0; i < h->colN; i++) {
    h->dpTable[0][i] = OptHei(h, 0, i);
    h->dpTable[0][i].bound = 0;
  }
  /* Histogram with i buckets, computed based on those with i-1 buckets */
  for (j = 0; j < h->colN; j++) {
    /* j stands for the end boundary currently reached */
    for (i = 1; i < h->rowN; i++) {
      /* i stands for the number of buckets currently used */
      keepE = INF;
      keepB = INF;
      keepH = INF;
      for (k = 0; k < j; k++) {
	temp = OptHei(h, k + 1, j);
	err = temp.error + h->dpTable[i - 1][k].error;
	if (err < keepE) {
	  keepE = err;
	  keepB = k + 1;
	  keepH = temp.hei;
	}
      }
      h->dpTable[i][j].bound = keepB;
      h->dpTable[i][j].error = keepE;
      h->dpTable[i][j].hei = keepH;
    }
  }
  /* Fill the result table, for result histogram. End point recorded */
  resultI = h->rowN - 1;
  track = h->colN;
  while (track != 0) {
    if (track == h->colN) h->result[resultI].bound = h->ind;
    else h->result[resultI].bound = h->rob[track].bound;
    h->result[resultI].hei = h->dpTable[resultI][track - 1].hei;
    track = h->dpTable[resultI][track - 1].bound;
    resultI--;
  }
  if (resultI > 0) {
    /* Use less than B buckets, with error guarantee */
     for (i = 0; i < resultI + 1; i++) h->result[i].bound = 0;
  }
  if (h->dpTable != NULL) {
    for (i = 0; i < h->rowN; i++) free(h->dpTable[i]);
    free(h->dpTable);
  }
}

Puint64 partition_w(struct wave** A, Puint64 p, Puint64 r) {
  /* Set the last element in the scope to be pivot */
  struct wave x;
  struct wave swap;
  Puint64 i;
  Puint64 j;

  x = (*A)[r];
  i = p - 1;
  for (j = p; j < r; j++) {
    if (fabs((*A)[j].coef) > fabs(x.coef)) {
      i++;
      swap = (*A)[i];
      (*A)[i] = (*A)[j];
      (*A)[j] = swap;
    }
  }
  swap = (*A)[i+1];
  (*A)[i+1] = (*A)[r];
  (*A)[r] = swap;

  return i+1;
}

void select_w(struct wave** A, struct hist* h, Puint64 p, Puint64 r) {
  Puint64 q;

  if (p == r) return;
  q = partition_w(A, p, r);
  if (q == h->rSize) return;
  if (q < h->rSize) select_w(A, h, q+1, r);
  else select_w(A, h, p, q-1);
}

Puint64 partition_b(struct bucket** A, Puint64 p, Puint64 r) {
  /* Set the last element in the scope to be pivot */
  struct bucket x;
  struct bucket swap;
  Puint64 i;
  Puint64 j;

  x = (*A)[r];
  i = p - 1;
  for (j = p; j < r; j++) {
    if ((*A)[j].bound < x.bound) {
      i++;
      swap = (*A)[i];
      (*A)[i] = (*A)[j];
      (*A)[j] = swap;
    }
  }

  swap = (*A)[i+1];
  (*A)[i+1] = (*A)[r];
  (*A)[r] = swap;

  return i+1;
}

Pfloat64 select_b(struct bucket** A, struct hist *h, Puint64 p, Puint64 r, Puint64 sel) {
  Puint64 q;

  if (p == r) return (*A)[p].hei;
  q = partition_b(A, p, r);
  if (q == sel) return (*A)[q].hei;
  if (q < sel) return select_b(A, h, q+1, r, sel);
  else return select_b(A, h, p, q-1, sel);
}

void quickSort(struct bucket** A, Puint64 p, Puint64 r) {
  if (p < r) {
      Puint64 q = partition_b(A, p, r);
      quickSort(A, p, q-1);
      quickSort(A, q+1, r);
  }
} 



