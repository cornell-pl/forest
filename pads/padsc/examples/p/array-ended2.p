int isDone(Pint32 value, int *consume){
  if (value == 1) {
    *consume = 1;
    return 1;
  };
  if (value == -1){
    *consume = 0;
    return 1;
  };
  return 0;
}

Parray fseq_t {
  Pint32 [] : Psep(Pre "/,/") && Pended(isDone(fseq_t[current], &consume));
}

Parray sseq_t {
  Pint32 [] : Psep(',') && Pterm(Pre "/$/");
}

Precord Pstruct entry{
  fseq_t first;
  Pre "/,?/";
  sseq_t second;
}
