/*@FILE pdisc.tex pinformats.tex poutformats.tex timestamp-format.tex date-format.tex time-format.tex timestamp-output-format.tex date-output-format.tex time-output-format.tex pfopen_fn.tex*/

/*@BEGIN pinformats.tex*/
typedef struct Pin_formats_s {
  const char        *timestamp;
  const char        *date;     
  const char        *time;     
} Pin_formats_t;
/*@END pinformats.tex*/

/*@BEGIN poutformats.tex*/
typedef struct Pout_formats_s {
  const char        *timestamp_explicit;  
  const char        *timestamp;           
  const char        *date_explicit;       
  const char        *date;                
  const char        *time_explicit;       
  const char        *time;                
} Pout_formats_t;
/*@END poutformats.tex*/

/*@BEGIN pdisc.tex */
typedef struct Pdisc_s {
  Pflags_t            version;        /* interface version */
  Pflags_t            flags;          /* control flags */
  Pcharset            def_charset;    /* default char set */ 
  int                 copy_strings;   /* if set,  ASCII string read
					 functions copy strings */
  /* For the next four values, 0 means end-of-record / 
     soft limit for non-record-based IO disciplines */
  size_t              match_max;      /* max match distance */ 
  size_t              numeric_max;    /* max numeric value distance */
  size_t              scan_max;       /* max normal scan distance */
  size_t              panic_max;      /* max panic scan distance */
  Pfopen_fn           fopen_fn;       /* file open function (default P_fopen) */
  Perror_fn           error_fn;       /* error function using  ... */
  PerrorRep           e_rep;          /* controls error reporting */
  Pendian_t           d_endian;       /* endian-ness of the data */ 
  Puint64             acc_max2track;  /* default maximum distinct values for 
					 accumulators to track */
  Puint64             acc_max2rep;    /* default maximum number of tracked values 
					 to describe in detail in report */
  Pfloat64            acc_pcnt2rep;   /* default maximum percent of values to 
					 describe in detail in report */
  const char         *in_time_zone;   /* default time zone for time input, 
					 specified as a string */ 
  const char         *out_time_zone;  /* default time zone for time formatted 
					 output, specified as a string */ 
  Pin_formats_t       in_formats;     /* default input formats */
  Pout_formats_t      out_formats;    /* default output formats */
  Pinv_val_fn_map_t  *inv_val_fn_map; /* map types to inv_val_fn 
					 for write functions */
  Pfmt_fn_map_t      *fmt_fn_map;     /* map types to fmt functions */
  Pio_disc_t         *io_disc;        /* sub-discipline for controlling IO */
} Pdisc_t;
/*@END pdisc.tex */


const char *s = 
/*@BEGIN timestamp-format.tex */
 "%m%d%y+%H%M%S%|%m%d%y+%H%M%S%|%m%d%Y+%H%M%S%|%m%d%Y+%H%M%S%|%&"
/*@END timestamp-format.tex */

const char *sout = 
/*@BEGIN timestamp-output-format.tex */
 "%Y%m%d|%H%M%S"
 "%m/%d/%Y %H:%M"
 "%K" /*default -- %K is the same as "%Y-%m-%d+%H:%M:%S"*/
/*@END timestamp-output-format.tex */

const char *d = 
/*@BEGIN date-format.tex */
 "%m%d%y%|%m%d%Y%|%&"
/*@END date-format.tex */

const char *dout = 
/*@BEGIN date-output-format.tex */
  "%Y%m%d"
  "%m/%d/%Y"
  "%Y-%m-%d" /* default */
/*@END date-output-format.tex */

const char *t = 
/*@BEGIN time-format.tex */
               "%H%M%S%|%H:%M:%S%|%&"
/*@END time-format.tex */

const char *tout = 
/*@BEGIN time-output-format.tex */
 "%H%M%S"
 "%H.%M"
 "%H:%M:%S" /* default */
/*@END time-output-format.tex */

/*@BEGIN pfopen_fn.tex*/
typedef Sfio_t* (*Pfopen_fn)(const char *source, const char *mode);
/*@END pfopen_fn.tex*/
