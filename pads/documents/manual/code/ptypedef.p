/*@FILE @LEFT  simple-typedef.tex  parameterized-typedef.tex */

/*@BEGIN simple-typedef.tex */
Ptypedef Puint32 bid_t : bid_t x => {x > 100};
/*@END simple-typedef.tex */


/*@BEGIN parameterized-typedef.tex */
Ptypedef Pa_uint64_FW(:len:) pn_t(:Puint8 len, Puint64 hi:): pn_t x => {x <= hi};
/*@END parameterized-typedef.tex */








