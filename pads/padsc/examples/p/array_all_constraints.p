
Precord Parray list(PDC_uint32 min, PDC_uint32 max) {
  Pint32 [min : max] : Psep == ':' && Pterm == '|' && Pforall i Pin elts { elts[i] == list[i]};
};

