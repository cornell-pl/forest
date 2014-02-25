/*@FILE strings.tex*/
/*@BEGIN strings.tex*/
Perror_t Pstring_init(P_t *pads, Pstring *s);
Perror_t Pstring_cleanup(P_t *pads, Pstring *s);
Perror_t Pstring_share(P_t *pads, Pstring *targ, const Pstring *src);
Perror_t Pstring_cstr_share(P_t *pads, Pstring *targ, const char *src, size_t len);
Perror_t Pstring_copy(P_t *pads, Pstring *targ, const Pstring *src);
Perror_t Pstring_cstr_copy(P_t *pads, Pstring *targ, const char *src, size_t len);
Perror_t Pstring_preserve(P_t *pads, Pstring *s);
int Pstring_eq(const Pstring *str1, const Pstring *str2);
int Pstring_eq_cstr(const Pstring *str, const char *cstr);

Pint8    Pstring2int8  (const Pstring *str);  /* returns P_MIN_INT8 on error   */
Pint16   Pstring2int16 (const Pstring *str);  /* returns P_MIN_INT16 on error  */
Pint32   Pstring2int32 (const Pstring *str);  /* returns P_MIN_INT32 on error  */ 
Pint64   Pstring2int64 (const Pstring *str);  /* returns P_MIN_INT64 on error  */ 

Puint8   Pstring2uint8 (const Pstring *str);  /* returns P_MAX_UINT8 on error  */ 
Puint16  Pstring2uint16(const Pstring *str);  /* returns P_MAX_UINT16 on error */ 
Puint32  Pstring2uint32(const Pstring *str);  /* returns P_MAX_UINT32 on error */ 
Puint64  Pstring2uint64(const Pstring *str);  /* returns P_MAX_UINT64 on error */ 

Pfloat32 Pstring2float32(const Pstring *str); /* returns P_MIN_FLOAT32 on error */
Pfloat64 Pstring2float64(const Pstring *str); /* returns P_MIN_FLOAT64 on error */
/*@END strings.tex*/
