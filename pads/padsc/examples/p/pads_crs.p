//- FROM pads_crshdr.p

pstruct XX_CREATE_DATE_11_crshdr {
	ebc_uint64(:2:) XX_CREATE_YY_12;
	ebc_uint64(:2:) XX_CREATE_MM_13;
	ebc_uint64(:2:) XX_CREATE_DD_14;
};
pstruct XX_BILL_CYCLE_DATE_3_crshdr {
	ebc_uint64(:2:) XX_BILL_CYC_YY_4;
	ebc_uint64(:2:) XX_BILL_CYC_MM_5;
	ebc_uint64(:2:) XX_BILL_CYC_DD_6;
};
pstruct XX_CRS_HEADER_1_crshdr {
	e_string_FW(:6:) FILLER_2;
	XX_BILL_CYCLE_DATE_3_crshdr XX_BILL_CYCLE_DATE_3;
	e_string_FW(:6:) FILLER_7;
	e_string_FW(:2:) XX_COPY_ID_8;
	e_string_FW(:1:) XX_RPC_CD_9;
	e_string_FW(:6:) FILLER_10;
	XX_CREATE_DATE_11_crshdr XX_CREATE_DATE_11;
	e_string_FW(:53:) FILLER_15;
};
precord pstruct cpy_crshdr {
	XX_CRS_HEADER_1_crshdr XX_CRS_HEADER_1;
};

//- FROM pads_crstlr.p

pstruct XX_CRS_TRAILER_1_crstlr {
	e_string_FW(:6:) FILLER_2;
	ebc_uint64(:8:) XX_TOTAL_RECORDS_3;
	e_string_FW(:72:) FILLER_4;
};
precord pstruct cpy_crstlr {
	XX_CRS_TRAILER_1_crstlr XX_CRS_TRAILER_1;
};

//- FROM pads_crsdet.p

pstruct XX_GROSS_AND_DISCS_21_crsdet {
	bcd_fpoint64(:11,2:) XX_GROSS_USAGE_22;
	bcd_fpoint64(:11,2:) XX_DISC_AMT_23;
};
parray array_5_elts_eltType_XX_GROSS_AND_DISCS_21_crsdet {
  XX_GROSS_AND_DISCS_21_crsdet [5];
};
pstruct XX_TOTALS_BY_JURISD_RD_20_crsdet {
	//- XXX_FIX Type here should be an array type with elt type XX_GROSS_AND_DISCS_21_crsdet, 5 elts
	//- XX_GROSS_AND_DISCS_21_crsdet XX_GROSS_AND_DISCS_21;
  array_5_elts_eltType_XX_GROSS_AND_DISCS_21_crsdet XX_GROSS_AND_DISCS_21;
};
pstruct XX_LOC_GROSS_AND_DISCS_17_crsdet {
	bcd_fpoint64(:11,2:) XX_LOC_GROSS_USAGE_18;
	bcd_fpoint64(:11,2:) XX_LOC_DISC_AMT_19;
};
pstruct XX_MEX_GROSS_AND_DISCS_14_crsdet {
	bcd_fpoint64(:11,2:) XX_MEX_GROSS_USAGE_15;
	bcd_fpoint64(:11,2:) XX_MEX_DISC_AMT_16;
};
pstruct XX_OVS_GROSS_AND_DISCS_11_crsdet {
	bcd_fpoint64(:11,2:) XX_OVS_GROSS_USAGE_12;
	bcd_fpoint64(:11,2:) XX_OVS_DISC_AMT_13;
};
pstruct XX_CAN_GROSS_AND_DISCS_8_crsdet {
	bcd_fpoint64(:11,2:) XX_CAN_GROSS_USAGE_9;
	bcd_fpoint64(:11,2:) XX_CAN_DISC_AMT_10;
};
pstruct XX_DOM_GROSS_AND_DISCS_5_crsdet {
	bcd_fpoint64(:11,2:) XX_DOM_GROSS_USAGE_6;
	bcd_fpoint64(:11,2:) XX_DOM_DISC_AMT_7;
};
pstruct XX_TOTALS_BY_JURISD_4_crsdet {
	XX_DOM_GROSS_AND_DISCS_5_crsdet XX_DOM_GROSS_AND_DISCS_5;
	XX_CAN_GROSS_AND_DISCS_8_crsdet XX_CAN_GROSS_AND_DISCS_8;
	XX_OVS_GROSS_AND_DISCS_11_crsdet XX_OVS_GROSS_AND_DISCS_11;
	XX_MEX_GROSS_AND_DISCS_14_crsdet XX_MEX_GROSS_AND_DISCS_14;
	XX_LOC_GROSS_AND_DISCS_17_crsdet XX_LOC_GROSS_AND_DISCS_17;
};
punion redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20_crsdet {
  XX_TOTALS_BY_JURISD_4_crsdet       union_arm_XX_TOTALS_BY_JURISD_4;
  XX_TOTALS_BY_JURISD_RD_20_crsdet   union_arm_XX_TOTALS_BY_JURISD_RD_20;
};
pstruct XX_CRS_RECORD_1_crsdet {
	e_string_FW(:13:) XX_LEAD_ACCT_NUM_2;
	e_string_FW(:13:) XX_SUB_ACCT_NUM_3;
	redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20_crsdet redefine_of_XX_TOTALS_BY_JURISD_4_to_XX_TOTALS_BY_JURISD_RD_20;
};
precord pstruct cpy_crsdet {
	XX_CRS_RECORD_1_crsdet XX_CRS_RECORD_1;
};
