
Pstruct s1 {
    {:|%I8d(int64_A)|%I4u(uint32_B)|%7I4u(uint32_fw7_C)foo:} Pint8 nongen1;  {:|%d(int32_D)%ld(int32_E)%lld(int64_F):};      Pchar nongen2;
    {:|%u(uint32_G)%lu(uint32_H)%llu(uint64_I):};
    {:|%4d(int32_fw4_J)%4ld(int32_fw4_K)%4lld(int64_fw4_L):};
    {:|%4u(uint32_fw4_M)%4lu(uint32_fw4_N)%4llu(uint64_fw4_O):};
    {:%8s(fw8_string_P)%s(vbar_term_string_Q)|%s(eor_term_string_R):}
    {Pre:\s+%s(int_term_string_S)%I4d(int32_T)\t:}
  Pchar nongen3;
};
