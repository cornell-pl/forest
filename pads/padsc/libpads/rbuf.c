#pragma prototyped
/*
 * rbuf implementation
 * 
 * Kathleen Fisher, Robert Gruber
 * AT&T Labs Research
 */

#include "rbuf.h"
#include "vmhdr.h"

/* ================================================================================ */
/* RMM : default allin1 functions and disciplines */

void* RMM_allin1_zero(void* vm, void* data, size_t size)
{
  if (!vm) {
    return (void*)vmopen(Vmdcheap, Vmbest, 0);
  }
  if (data || size) {
    return (void*)vmresize((Vmalloc_t*)vm, data, size, VM_RSMOVE|VM_RSCOPY|VM_RSZERO);
  }
  vmclose((Vmalloc_t*)vm);
  return 0;
}

void* RMM_allin1_nozero(void* vm, void* data, size_t size)
{
  if (!vm) {
    return (void*)vmopen(Vmdcheap, Vmbest, 0);
  }
  if (data || size) {
    return (void*)vmresize((Vmalloc_t*)vm, data, size, VM_RSMOVE|VM_RSCOPY);
  }
  vmclose((Vmalloc_t*)vm);
  return 0;
}

RMM_dist_t RMM_zero_disc   = { RMM_allin1_zero };
RMM_dist_t RMM_nozero_disc = { RMM_allin1_nozero };

