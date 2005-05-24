/*
 * histogram implementation
 * 
 */

#include "pads-internal.h"

/* Default mapping functions, can be overwritten by users */
Pfloat64 Pint64_to (Pint64 i) { return (Pfloat64)i; }
Pint64 Pint64_from (Pfloat64 f) { return (Pint64)f; }

/* Functions defined with public access */
Perror_t Pint64_hist_init (P_t *pads, Pint64_hist *h) {
  /* Parameter Check */
  if (h->n != 1 && h->n != 2) return P_ERR;
  if (h->n == 1) { if (h->isO == 0 || h->isE != 0) return P_ERR; }	

  /* Initialize */
  h->toFloat = Pint64_to;
  h->fromFloat = Pint64_from;
  h->ind = 0;
  h->result = malloc(h->B * sizeof(struct bucket));
  if (h->result == (struct bucket*)0) exit(-1); 
  if (h->isE != 0) {
    /* Equally spaced */
    h->partS = 0;
    h->bukI = 0;
    Puint32 i;
    for (i = 0; i < h->B; i++) h->result[i].bound = ((Pint64)(h->N / h->B) + 1) * (i + 1);
    h->result[h->B - 1].bound = h->N;
  }
  else {
    if (h->isO != 0) {
      /* Optimal result required */
      h->rob = malloc(h->N * sizeof(struct bucket));
      if (h->rob == (struct bucket*)0) exit(-1);
      h->robI = 0;
    }
    else {
      /* Adjust dimension to be perfect power of 2. */
      Puint64 adj = (Puint64)((Pfloat64)log(h->N) / (Pfloat64)log(2));  
      if ((Puint64)pow(2, (Pfloat64)adj) != h->N) h->N = (Puint64)pow(2, (Pfloat64)adj + 1);

      h->lNumber = (Puint8)((Pfloat64)log(h->N) / (Pfloat64)log(2)); 
      h->leftS = malloc(h->lNumber * sizeof(Pfloat64));
      h->rightS = malloc(h->lNumber * sizeof(Pfloat64));
      if (h->leftS == (Pfloat64*)0 || h->rightS == (Pfloat64*)0) exit(-1); 
      h->rSize = (Puint32)((Pfloat64)(h->B * ((Pfloat64)log(h->N) / (Pfloat64)log(2)) * ((Pfloat64)log(h->M) / (Pfloat64)log(2))) / h->e);
      h->top = malloc(2 * h->rSize * sizeof(struct wave));
      if (h->top == (struct wave*)0) exit(-1);
      h->topI = 0;
      h->bound = malloc(3 * h->rSize * sizeof(struct bucket));
      if (h->bound == (struct bucket*)0) exit(-1);
      h->boundI = 0;
      h->rob = malloc((3 * h->rSize + 1) * sizeof(struct bucket));
      if (h->rob == (struct bucket*)0) exit(-1);
      h->rob[0].bound = 0;
      h->rob[0].hei = 0; 
      h->robI = 1;
    }
  }
  return P_OK;
}  

Perror_t Pint64_hist_reset (P_t *pads, Pint64_hist *h) {
  h->ind = 0;
  if (h->isE != 0) {
    /* Equally spaced */
    h->partS = 0;
    h->bukI = 0;
  }
  else {
    if (h->isO != 0) h->robI = 0;
    else {
      h->topI = 0;
      h->boundI = 0;
      h->robI = 1;
    }
  }
  return P_OK;
}

Perror_t Pint64_hist_cleanup (P_t *pads, Pint64_hist *h) {
  free(h->result);
  if (h->isE == 0) {	 
    free(h->rob);
    if (h->isO == 0) {
      free(h->leftS);
      free(h->rightS);
      free(h->top);
      free(h->bound);
    }  
  }
  return P_OK;
}

Perror_t Pint64_hist_add (P_t *pads, Pint64_hist *h, Pint64 i) {
  Pfloat64 d = (*h->toFloat)(i) / (Pfloat64)h->scale; 
     
  Perror_t res = P_OK;
  if (h->isE != 0) res = EqualHis(h, d);
  else {
    if (h->isO != 0) res = OptHis(h, d);
    else res = NearOptHis(h, d); 
  }
  h->ind++;

  if (h->ind == h->N) {
    /* The last element in the scope */
    if (h->isE == 0) {
      if (h->isO == 0) buildRob(h);
      compOpt(h);
    }
    if(res == P_OK) res =  Pint64_hist_report(pads, h);
    if(res == P_OK) res = Pint64_hist_reset(pads, h);
  }	
  return res;
} 

Perror_t Pint64_hist_report (P_t *pads, Pint64_hist *h) {
  Perror_t res = P_OK;
  if (h->ind != h->N) {
    /* Real data is less than the estimated dimension */
    if (h->isE != 0) {
      h->result[h->bukI].hei = h->partS;
      h->result[h->bukI].bound = h->ind; 
      h->bukI++;
    }
    else {
      if (h->isO == 1) {
	h->robI = h->ind;
	h->N = h->ind;
	if (h->B > h->robI) h->B = h->robI;
	compOpt(h);
      }
      else {
	Puint64 i;
	Puint64 tempInd = h->ind;
	for (i = h->ind; i < h->N; i++) res = NearOptHis(h, 0);
	buildRob(h);
	h->ind = tempInd;
	compOpt(h);
      }
    }
  } 
  
  printf("*** Histogram Result *** \n");
  if (h->isE == 0) h->bukI = h->B;
  Puint64 i;	
  for (i = 0; i < h->bukI; i++) {
    if (i == 0) printf("From %d to ", 0);
    else printf("From %d to ", h->result[i-1].bound);
    printf("%d, with height ", h->result[i].bound - 1);
    printf("%d. \n", (*h->fromFloat)(h->result[i].hei * (Pfloat64)h->scale));
  }
  return res;
}

/* Functions defined for private use only */
Perror_t EqualHis(Pint64_hist *h, Pfloat64 d) {
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
	Pint64 i = h->ind - h->result[h->bukI - 1].bound;
	h->partS = (h->partS * i + d) / (i + 1);
      }
    }
  }
  return P_OK;	
}

Perror_t OptHis(Pint64_hist *h, double d) {
  h->rob[h->ind].bound = h->ind;
  h->rob[h->ind].hei = d;
  h->robI++;
  return P_OK;
}

Perror_t NearOptHis(Pint64_hist *h, double d) {
  /* Compute coefficients */
  Puint8 i;
  struct wave* tempCoef = malloc((h->lNumber + 1) * sizeof(struct wave));
  if (tempCoef == (struct wave*)0) exit(-1);
  Puint8 tempCoefI = 0;
  for (i = 0; i < h->lNumber; i++) {
    Puint64 rem = h->ind % (Pint64)pow(2, (Pfloat64)i+1);		
    
    if (rem == 0) {
      /* Hit the left half of a new wavelet on ith. level */
      if (h->ind != 0) {
	/* Roll back current wavelet vector */
	struct wave temp;
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
	struct wave temp;
	temp.level = i;
	temp.index = h->ind / (Puint64)pow(2, (Pfloat64)i+1);
	temp.coef = (h->rightS[i] - h->leftS[i]) / pow(2, ((Pfloat64)i+1)/2);
	tempCoef[tempCoefI] = temp;
	tempCoefI++;
	if (i == h->lNumber - 1) {
	  /* Add the uniform wavelet vector */
	  struct wave temp;
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

void buildRob(Pint64_hist *h) {
  select_w(&(h->top), h, 0, 2 * h->rSize - 1); 
   
  /* Build the robust histogram out of the top array, start point recorded */
  Puint32 i;
  for (i = 0; i < h->rSize; i++) {
    struct wave temp = h->top[i];
    if (temp.level == h->lNumber) {
      // The uniform one
      h->bound[h->boundI].bound = 0;
      h->bound[h->boundI].hei = temp.coef / sqrt((Pfloat64)(h->N));
      h->boundI++; 
    }
    else {
      Puint64 left = temp.index * (Puint64)pow(2, (Pfloat64)(temp.level + 1));
      Puint64 right = (temp.index + 1) * (Puint64)pow(2, (Pfloat64)(temp.level + 1));
      Puint64 mid = (Puint64)(left + right) / 2;
      Pfloat64 hei = temp.coef * pow(2, -((Pfloat64)temp.level+1) / 2);

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

  Puint64 currB = h->bound[0].bound;
  Pfloat64 currH = h->bound[0].hei;
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

struct dpCell OptHei(Pint64_hist *h, Puint64 s, Puint64 e) {
  if (h->n == 1) {
    /* L-1 norm */
    Puint64 len = e - s + 1;
    struct bucket* temp = malloc(len * sizeof(struct bucket));
    if (temp == (struct bucket*)0) exit(-1);
    Puint64 i; 
    for (i = 0; i < len; i++) temp[i] = h->rob[s + i]; 
    Pfloat64 hei = 0;
    if ((len % 2) == 0) hei = (select_b(&temp, h, 0, len-1, len/2) + select_b(&temp, h, 0, len-1, len/2-1 )) / 2;
    else hei = select_b(&temp, h, 0, len-1, (len-1)/2);
    free(temp);
    Pfloat64 err = 0;
    for (i = s; i < e+1; i++) err += fabs(h->rob[i].hei - hei);
		
    struct dpCell temp_dp;
    temp_dp.error = err;
    temp_dp.hei = hei;
    return temp_dp; 
  }

  /* L-2 norm. Compute the average */
  Pfloat64 sum = 0;
  Puint64 len = 0;
  Puint64 i;
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
  Pfloat64 hei = sum / (Pfloat64)len;

  /* Compute the error */
  Pfloat64 err = 0;
  for (i = s; i < e; i++) err += (h->rob[i+1].bound - h->rob[i].bound) * pow(h->rob[i].hei - hei, 2);
  if (e == h->robI - 1)	err += (h->N - h->rob[e].bound) * pow(h->rob[e].hei - hei, 2);
  else 	err += (h->rob[e+1].bound - h->rob[e].bound) * pow(h->rob[e].hei - hei, 2);  
	
  struct dpCell temp_dp;
  temp_dp.error = err;
  temp_dp.hei = hei;
  return temp_dp;
}

void compOpt(Pint64_hist *h) {
  /* Size of the dynamic programming table */
  if (h->isO != 0) h->colN = h->N;
  else {
    if (h->ind != h->N) {
      Puint64 i = 0;
      while (i < h->robI && h->rob[i].bound < h->ind) i++;
      h->robI = i;
    }
    if (h->B > h->robI) h->B = h->robI;
    h->colN = h->robI;
  }
  h->rowN = h->B;
  h->dpTable = malloc(h->rowN * sizeof(struct dpCell*));
  if (h->dpTable == (struct dpCell**)0) exit(-1);
  Puint32 i;
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
  Puint64 j;
  for (j = 0; j < h->colN; j++) {
    /* j stands for the end boundary currently reached */
    Puint32 i;
    for (i = 1; i < h->rowN; i++) {
      /* i stands for the number of buckets currently used */
      Pfloat64 keepE = INF;
      Pint64 keepB = INF;
      Pfloat64 keepH = INF;
      Puint64 k;
      for (k = 0; k < j; k++) {
	struct dpCell temp = OptHei(h, k + 1, j);
	Pfloat64 err = temp.error + h->dpTable[i - 1][k].error;
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
  Puint32 resultI = h->rowN - 1;
  Puint64 track = h->colN;
  while (track != 0) {
    if (track == h->colN) h->result[resultI].bound = h->ind; 
    else h->result[resultI].bound = h->rob[track].bound;
    h->result[resultI].hei = h->dpTable[resultI][track - 1].hei;
    track = h->dpTable[resultI][track - 1].bound;
    resultI--;
  }
  if (resultI > 0) {
    /* Use less than B buckets, with error guarantee */
    Puint32 i;
    for (i = 0; i < resultI + 1; i++) h->result[i].bound = 0; 	
  } 
  if (h->dpTable != NULL) {
    Puint32 i;
    for (i = 0; i < h->rowN; i++) free(h->dpTable[i]);
    free(h->dpTable);
  }
}

Puint64 partition_w(struct wave** A, Puint64 p, Puint64 r) {
  /* Set the last element in the scope to be pivot */
  struct wave x = (*A)[r];
  Puint64 i = p - 1;

  Puint64 j;
  for (j = p; j < r; j++) {
    if (fabs((*A)[j].coef) > fabs(x.coef)) {
      i++;
      struct wave swap = (*A)[i];
      (*A)[i] = (*A)[j];
      (*A)[j] = swap;
    }
  }
  
  struct wave swap = (*A)[i+1];
  (*A)[i+1] = (*A)[r];
  (*A)[r] = swap;

  return i+1;
} 

void select_w(struct wave** A, Pint64_hist* h, Puint64 p, Puint64 r) {
  if (p == r) return;
	
  Puint64 q = partition_w(A, p, r);
  if (q == h->rSize) return;
  if (q < h->rSize) select_w(A, h, q+1, r);
  else select_w(A, h, p, q-1); 
}

Puint64 partition_b(struct bucket** A, Puint64 p, Puint64 r) {
  /* Set the last element in the scope to be pivot */
  struct bucket x = (*A)[r];
  Puint64 i = p - 1;

  Puint64 j;
  for (j = p; j < r; j++) {
    if ((*A)[j].bound < x.bound) {
      i++;
      struct bucket swap = (*A)[i];
      (*A)[i] = (*A)[j];
      (*A)[j] = swap;
    }
  }	

  struct bucket swap = (*A)[i+1];
  (*A)[i+1] = (*A)[r];
  (*A)[r] = swap;

  return i+1;	
}

Pfloat64 select_b(struct bucket** A, Pint64_hist *h, Puint64 p, Puint64 r, Puint64 sel) {
  if (p == r) return (*A)[p].hei;

  Puint64 q = partition_b(A, p, r);
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




