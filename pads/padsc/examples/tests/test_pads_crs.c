#include "libpadsc.h"
#include "pads_crs.h"

extern void print_cpy_crsdet(PDC_t* pdc, cpy_crsdet *tt);

int main(int argc, char** argv) {
  PDC_t             *pdc;
  PDC_IO_disc_t*     io_disc;
  PDC_disc_t         my_disc = PDC_default_disc;

  cpy_crshdr         hdr_rep;
  cpy_crshdr_ed      hdr_ed;

  cpy_crstlr         tlr_rep;
  cpy_crstlr_ed      tlr_ed;

  cpy_crsdet         rep;
  cpy_crsdet_ed      ed;
  cpy_crsdet_acc     acc;

  char              *fileName;
  PDC_error_t        e;
  PDC_pos_t          before_pos, after_pos;
  PDC_uint64         num_recs = 0;
  
  if (argc == 2) {
    fileName = argv[1];
    error(0, "Data file = %s\n", fileName);
  } else {
    fileName = "/dev/stdin";
    error(0, "Data file = standard in\n");
  }

#if 0
  io_disc = PDC_norec_make(0);
#else
  io_disc = PDC_fwrec_noseek_make(0, 86, 0);
#endif

  if (!io_disc) {
    error(ERROR_FATAL, "\nFailed to install IO discipline");
  } else {
    error(0, "\nInstalled IO discipline %s", io_disc->name);
  }

  if (PDC_ERR == PDC_open(&pdc, &my_disc, io_disc)) {
    error(2|ERROR_FATAL, "*** PDC_open failed ***");
  }

  if (PDC_ERR == PDC_IO_fopen(pdc, fileName)) {
    error(2|ERROR_FATAL, "*** PDC_IO_fopen failed ***");
  }

  if (PDC_ERR == cpy_crshdr_init(pdc, &hdr_rep)) {
    error(2|ERROR_FATAL, "*** cpy_crshdr representation initialization failed ***");
  }
  if (PDC_ERR == cpy_crshdr_ed_init(pdc, &hdr_ed)) {
    error(2|ERROR_FATAL, "*** cpy_crshdr error description initialization failed ***");
  }


  if (PDC_ERR == cpy_crstlr_init(pdc, &tlr_rep)) {
    error(2|ERROR_FATAL, "*** cpy_crstlr representation initialization failed ***");
  }
  if (PDC_ERR == cpy_crstlr_ed_init(pdc, &tlr_ed)) {
    error(2|ERROR_FATAL, "*** cpy_crstlr error description initialization failed ***");
  }

  if (PDC_ERR == cpy_crsdet_init(pdc, &rep)) {
    error(2|ERROR_FATAL, "*** cpy_crsdet representation initialization failed ***");
  }
  if (PDC_ERR == cpy_crsdet_ed_init(pdc, &ed)) {
    error(2|ERROR_FATAL, "*** cpy_crsdet error description initialization failed ***");
  }

  if (PDC_ERR == cpy_crsdet_acc_init(pdc, &acc)) {
    error(2|ERROR_FATAL, "*** cpy_crsdet accumulator initialization failed ***");
  }

  /*
   * Try to read the header
   */
  if (PDC_OK != cpy_crshdr_read(pdc, 0, &hdr_ed, &hdr_rep)) {
    error(2|ERROR_FATAL, "crshdr_read returned error");
  }
  error(0, "HEADER INFO:");
  error(0, "          XX_COPY_ID_8: %s", hdr_rep.XX_CRS_HEADER_1.XX_COPY_ID_8.str);
  error(0, "           XX_RPC_CD_8: %s", hdr_rep.XX_CRS_HEADER_1.XX_RPC_CD_9.str);
  error(0, "  XX_BILL_CYCLE_DATE_3: %02llu/%02llu/%02llu",
	hdr_rep.XX_CRS_HEADER_1.XX_BILL_CYCLE_DATE_3.XX_BILL_CYC_MM_5,
	hdr_rep.XX_CRS_HEADER_1.XX_BILL_CYCLE_DATE_3.XX_BILL_CYC_DD_6,
	hdr_rep.XX_CRS_HEADER_1.XX_BILL_CYCLE_DATE_3.XX_BILL_CYC_YY_4);
  error(0, "     XX_CREATE_DATE_11: %02llu/%02llu/%02llu\n",
	hdr_rep.XX_CRS_HEADER_1.XX_CREATE_DATE_11.XX_CREATE_MM_13,
	hdr_rep.XX_CRS_HEADER_1.XX_CREATE_DATE_11.XX_CREATE_DD_14,
	hdr_rep.XX_CRS_HEADER_1.XX_CREATE_DATE_11.XX_CREATE_YY_12);

  /*
   * Try to read each line of data
   */
  while (!PDC_IO_at_EOF(pdc)) {
    if (num_recs) { /* complete processing of record */
      PDC_IO_commit(pdc);
      if (PDC_OK != e) {
	error(2, "crsdet_read returned error");
      } else {
	print_cpy_crsdet(pdc, &(rep));
      }
      /* accum both good and bad vals */
      if (PDC_ERR == cpy_crsdet_acc_add(pdc, &acc, &ed, &rep)) {
	error(2|ERROR_FATAL, "*** accumulator add failed ***");
      }
    }
    /* start processing new record */ 
    num_recs++;
    PDC_IO_checkpoint(pdc, 0);
    PDC_IO_getPos(pdc, &before_pos, 0);
    e = cpy_crsdet_read(pdc, 0, &ed, &rep);
    PDC_IO_getPos(pdc, &after_pos, 0);
    if (PDC_POS_EQ(before_pos, after_pos)) {
      error(2|ERROR_FATAL, "** crsdet_read did not advance IO cursor, giving up **");
    }
  }
  /* back up one record and try to parse trailer */
  PDC_IO_restore(pdc);
  num_recs--;
  if (PDC_OK != cpy_crstlr_read(pdc, 0, &tlr_ed, &tlr_rep)) {
    error(2|ERROR_FATAL, "crstlr_read returned error");
  }
  error(0, "TRAILER INFO:");
  error(0, "          XX_TOTAL_RECORDS_3: %llu", tlr_rep.XX_CRS_TRAILER_1.XX_TOTAL_RECORDS_3);
  error(0, "         compare to num_recs: %llu\n", num_recs);

  if (PDC_ERR == cpy_crsdet_acc_report(pdc, "", 0, 0, &acc)) {
    error(0, "** accum_report failed **");
  }

  if (PDC_ERR == PDC_IO_close(pdc)) {
    error(2|ERROR_FATAL, "*** PDC_IO_close failed ***");
  }

  if (PDC_ERR == cpy_crsdet_cleanup(pdc, &rep)) {
    error(0, "** representation cleanup failed **");
  }

  if (PDC_ERR == cpy_crsdet_ed_cleanup(pdc, &ed)) {
    error(0, "** error descriptor cleanup failed **");
  }

  if (PDC_ERR == cpy_crsdet_acc_cleanup(pdc, &acc)) {
    error(0, "** accumulator cleanup failed **");
  }

  if (PDC_ERR == PDC_close(pdc)) {
    error(2|ERROR_FATAL, "*** PDC_close failed ***");
  }

  return 0;
}

#define	PR_O(ofst)				error(ERROR_PROMPT, "%d:", (int)(ofst))
#define	PR_T_CHARS(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": '%.*s'", indent, (ff)->len, (ff)->str)
#define	PR_T_NUM(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_SNUM(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_BCD(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_SBCD(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_BINARY(nm,ff,ofst)			PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_UBINARY(nm,ff,ofst)		PR_O(ofst), error(0, "%s" #nm ": %lld", indent, (*(ff)))
#define	PR_T_FIXEDPOINT(nm,ff,ofst,z,d)		PR_O(ofst), error(0, "%s" #nm ": %*.*f", indent, z, d, PDC_FPOINT2FLT(*ff))
#define	PR_T_SFIXEDPOINT(nm,ff,ofst,z,d)	PR_O(ofst), error(0, "%s" #nm ": %*.*f", indent, z, d, PDC_FPOINT2FLT(*ff))
#define	PR_T_SBCDFIXEDPOINT(nm,ff,ofst,z,d)	PR_O(ofst), error(0, "%s" #nm ": %*.*f", indent, z, d, PDC_FPOINT2FLT(*ff))
#define	PR_T_UBINFIXEDPOINT(nm,ff,ofst,z,d)	PR_O(ofst), error(0, "%s" #nm ": %*.*f", indent, z, d, PDC_FPOINT2FLT(*ff))

static char *tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t";

static void
pr_XX_GROSS_AND_DISCS_21_crsdet(PDC_t* pdc, char *indent, int offset, XX_GROSS_AND_DISCS_21_crsdet *tt)
{
  error(0, "%sXX_GROSS_AND_DISCS_21_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_GROSS_USAGE_22, &(tt->XX_GROSS_USAGE_22), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_DISC_AMT_23, &(tt->XX_DISC_AMT_23),    offset+6, 9, 2);
}
static void
pr_XX_TOTALS_BY_JURISD_RD_20_crsdet(PDC_t* pdc, char *indent, int offset, XX_TOTALS_BY_JURISD_RD_20_crsdet *tt)
{
  error(0, "%sXX_TOTALS_BY_JURISD_RD_20_crsdet >>>>>>aggr", indent--);
  {
    int i;
    for(i = 0; i < 5; i++) {
      pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent, offset+(i*12),
				      &(tt->XX_GROSS_AND_DISCS_21.array_5_elts_eltType_XX_GROSS_AND_DISCS_21_crsdet[i]));
    }
  }
}
static void
HACK_pr_XX_TOTALS_BY_JURISD_RD_20_crsdet(PDC_t* pdc, char *indent, int offset, XX_TOTALS_BY_JURISD_4_crsdet *tt)
{
  error(0, "%sXX_TOTALS_BY_JURISD_RD_20_crsdet >>>>>>aggr", indent--);
  pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent, offset, (XX_GROSS_AND_DISCS_21_crsdet*)&(tt->XX_DOM_GROSS_AND_DISCS_5));
  pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent, offset+12,  (XX_GROSS_AND_DISCS_21_crsdet*)&(tt->XX_CAN_GROSS_AND_DISCS_8));
  pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent, offset+24,  (XX_GROSS_AND_DISCS_21_crsdet*)&(tt->XX_OVS_GROSS_AND_DISCS_11));
  pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent,  offset+36, (XX_GROSS_AND_DISCS_21_crsdet*)&(tt->XX_MEX_GROSS_AND_DISCS_14));
  pr_XX_GROSS_AND_DISCS_21_crsdet(pdc, indent,  offset+48, (XX_GROSS_AND_DISCS_21_crsdet*)&(tt->XX_LOC_GROSS_AND_DISCS_17));
}
static void
pr_XX_LOC_GROSS_AND_DISCS_17_crsdet(PDC_t* pdc, char *indent, int offset, XX_LOC_GROSS_AND_DISCS_17_crsdet *tt)
{
  error(0, "%sXX_LOC_GROSS_AND_DISCS_17_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_LOC_GROSS_USAGE_18, &(tt->XX_LOC_GROSS_USAGE_18), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_LOC_DISC_AMT_19, &(tt->XX_LOC_DISC_AMT_19), offset+6, 9, 2);
}
static void
pr_XX_MEX_GROSS_AND_DISCS_14_crsdet(PDC_t* pdc, char *indent, int offset, XX_MEX_GROSS_AND_DISCS_14_crsdet *tt)
{
  error(0, "%sXX_MEX_GROSS_AND_DISCS_14_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_MEX_GROSS_USAGE_15, &(tt->XX_MEX_GROSS_USAGE_15), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_MEX_DISC_AMT_16, &(tt->XX_MEX_DISC_AMT_16), offset+6, 9, 2);
}
static void
pr_XX_OVS_GROSS_AND_DISCS_11_crsdet(PDC_t* pdc, char *indent, int offset, XX_OVS_GROSS_AND_DISCS_11_crsdet *tt)
{
  error(0, "%sXX_OVS_GROSS_AND_DISCS_11_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_OVS_GROSS_USAGE_12, &(tt->XX_OVS_GROSS_USAGE_12), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_OVS_DISC_AMT_13, &(tt->XX_OVS_DISC_AMT_13), offset+6, 9, 2);
}
static void
pr_XX_CAN_GROSS_AND_DISCS_8_crsdet(PDC_t* pdc, char *indent, int offset, XX_CAN_GROSS_AND_DISCS_8_crsdet *tt)
{
  error(0, "%sXX_CAN_GROSS_AND_DISCS_8_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_CAN_GROSS_USAGE_9, &(tt->XX_CAN_GROSS_USAGE_9), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_CAN_DISC_AMT_10, &(tt->XX_CAN_DISC_AMT_10), offset+6, 9, 2);
}
static void
pr_XX_DOM_GROSS_AND_DISCS_5_crsdet(PDC_t* pdc, char *indent, int offset, XX_DOM_GROSS_AND_DISCS_5_crsdet *tt)
{
  error(0, "%sXX_DOM_GROSS_AND_DISCS_5_crsdet >>>>>>aggr", indent--);
  PR_T_SBCDFIXEDPOINT(XX_DOM_GROSS_USAGE_6, &(tt->XX_DOM_GROSS_USAGE_6), offset, 9, 2);
  PR_T_SBCDFIXEDPOINT(XX_DOM_DISC_AMT_7, &(tt->XX_DOM_DISC_AMT_7), offset+6, 9, 2);
}
static void
pr_XX_TOTALS_BY_JURISD_4_crsdet(PDC_t* pdc, char *indent, int offset, XX_TOTALS_BY_JURISD_4_crsdet *tt)
{
  error(0, "%sXX_TOTALS_BY_JURISD_4_crsdet >>>>>>aggr", indent--);
  pr_XX_DOM_GROSS_AND_DISCS_5_crsdet(pdc, indent, offset, &(tt->XX_DOM_GROSS_AND_DISCS_5));
  pr_XX_CAN_GROSS_AND_DISCS_8_crsdet(pdc, indent, offset+12,  &(tt->XX_CAN_GROSS_AND_DISCS_8));
  pr_XX_OVS_GROSS_AND_DISCS_11_crsdet(pdc, indent, offset+24,  &(tt->XX_OVS_GROSS_AND_DISCS_11));
  pr_XX_MEX_GROSS_AND_DISCS_14_crsdet(pdc, indent,  offset+36, &(tt->XX_MEX_GROSS_AND_DISCS_14));
  pr_XX_LOC_GROSS_AND_DISCS_17_crsdet(pdc, indent,  offset+48, &(tt->XX_LOC_GROSS_AND_DISCS_17));
}
static void
pr_XX_CRS_RECORD_1_crsdet(PDC_t* pdc, char *indent, int offset, XX_CRS_RECORD_1_crsdet *tt)
{
  error(0, "%sXX_CRS_RECORD_1_crsdet >>>>>>aggr", indent--);
  PR_T_CHARS(XX_LEAD_ACCT_NUM_2, &(tt->XX_LEAD_ACCT_NUM_2), offset);
  PR_T_CHARS(XX_SUB_ACCT_NUM_3, &(tt->XX_SUB_ACCT_NUM_3), offset+13);
  /* XXX ignore tag and print redefine union value both ways, since that is what Hume's code does. */
  pr_XX_TOTALS_BY_JURISD_4_crsdet(pdc, indent, offset+26,
				  &(tt->redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20.val.union_arm_XX_TOTALS_BY_JURISD_4));

#if 0
  pr_XX_TOTALS_BY_JURISD_RD_20_crsdet(pdc, indent, offset+26,
				      &(tt->redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20.val.union_arm_XX_TOTALS_BY_JURISD_RD_20));
#else
  /* XXX hack: the representation differs due to use of variable length array so need to fake it here */
  HACK_pr_XX_TOTALS_BY_JURISD_RD_20_crsdet(pdc, indent, offset+26,
				  &(tt->redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20.val.union_arm_XX_TOTALS_BY_JURISD_4));
#endif
}
static void
pr_cpy_crsdet(PDC_t* pdc, char *indent, int offset, cpy_crsdet *tt)
{
  error(0, "%scpy_crsdet >>>>>>aggr", indent--);
  pr_XX_CRS_RECORD_1_crsdet(pdc, indent, offset, &(tt->XX_CRS_RECORD_1));
}

void
print_cpy_crsdet(PDC_t* pdc, cpy_crsdet *tt)
{
  pr_cpy_crsdet(pdc, strchr(tabs, 0), 0, tt);
}

