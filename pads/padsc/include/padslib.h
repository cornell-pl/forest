#define PANIC_SKIPPED              10

#define USER_CONSTRAINT_VIOLATION  100
#define MISSING_LITERAL            101
#define ARRAY_ELEM_ERR             110
#define ARRAY_SEP_ERR              111
#define ARRAY_TERM_ERR             112
#define ARRAY_SIZE_ERR             113
#define ARRAY_USER_CONSTRAINT_ERR  114
#define STRUCT_FIELD_ERR           120
#define UNION_MATCH_FAILURE        130
#define ENUM_MATCH_FAILURE         140

typedef struct loc_s {
  int lineNum;
  int posNum;
  char *begin;
  char *end;
} loc_t;

typedef struct base_ed_s {
  int errCode;
  loc_t loc;
  int panic;
} base_ed;  

typedef enum base_em_e { Check_and_set, Check, Ignore } base_em;

typedef void (*error_f)(loc_t loc, char * desc, int errCode);

typedef struct _toolDisc_t toolDisc_t;

typedef struct _toolState_t toolState_t;


typedef int toolError_t;
#define TOOL_OK 0
#define TOOL_ERROR -1

/* Types to be generated for machine and included. */
typedef signed   char int8;
typedef unsigned char uint8;
typedef signed   int  int32;
typedef unsigned int  uint32;

/* Built-in types */
typedef int8 aint8_rep;
typedef aint8_rep aint8;
typedef base_em aint8_em;
typedef base_ed aint8_ed;

typedef uint8 auint8_rep;
typedef auint8_rep auint8;
typedef base_em auint8_em;
typedef base_ed auint8_ed;

typedef int32 aint32_rep;
typedef aint32_rep aint32;
typedef base_em aint32_em;
typedef base_ed aint32_ed;

typedef uint32 auint32_rep;
typedef auint32_rep auint32;
typedef base_em auint32_em;
typedef base_ed auint32_ed;


toolError_t aint8_read(toolState_t *ts, base_em *em,
                       base_ed *error, int8 *res);

toolError_t aint32_read(toolState_t *ts, base_em *em,
                        base_ed *error, int32 *res);


toolError_t auint8_read(toolState_t *ts, base_em *em,
                        base_ed *error, uint8 *res);

toolError_t auint32_read(toolState_t *ts, base_em *em,
                         base_ed *error, uint32 *res);


