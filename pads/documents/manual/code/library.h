/*@FILE init.tex init-mask.tex utility.tex verify.tex genPD.tex read.tex write.tex*/
/*@BEGIN init.tex*/
Perror_t foo_init (P_t *pads, foo *rep);

Perror_t foo_cleanup (P_t *pads, foo *rep);

Perror_t foo_pd_init (P_t *pads, foo_pd *pd);

Perror_t foo_pd_cleanup (P_t *pads, foo_pd *pd);
/*@END init.tex*/

/*@BEGIN init-mask.tex*/
void foo_m_init (P_t *pads, foo_m *mask, Pbase_m baseMask);
/*@END init-mask.tex*/

/*@BEGIN utility.tex*/
Perror_t foo_copy (P_t *pads, foo *rep_dst, foo *rep_src);

Perror_t foo_pd_copy (P_t *pads, foo_pd *pd_dst, foo_pd *pd_src);
/*@END utility.tex*/

/*@BEGIN verify.tex*/
int foo_verify(foo *rep);
/*@END verify.tex*/

/*@BEGIN genPD.tex*/
int foo_genPD (P_t *pads, foo *rep, foo_pd *pd);
/*@END genPD.tex*/


/*@BEGIN read.tex*/
Perror_t foo_read (P_t *pads, foo_m *m, foo_pd *pd, foo *rep);
/*@END read.tex*/

/*@BEGIN write.tex*/
ssize_t foo_write2io (P_t *pads, Sfio_t *io, foo_pd *pd, foo *rep);

ssize_t foo_write2buf (P_t *pads, Pbyte *buf, size_t buf_len, 
                        int *buf_full, foo_pd *pd, foo *rep);
/*@END write.tex*/
