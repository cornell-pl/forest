parray intList {
  a_int32 [] : sep == '|' && term == '.' ;
};

punion auth_id_t {
  a_char unauthorized : unauthorized == '-';    /- non-authenticated http session
  a_string(:' ':) id;                           /- login supplied during authentication
};

int check(PDC_int32 t, auth_id_t user){
  switch (user.tag) {
  case unauthorized: 
    return t == 1;
  case id:
    return t == 2;
  default: 
    return 1;  // Don't want to trigger another error 
  }
};

precord pstruct line{
       intList    f;
       a_int32    a : a == f.length;
  ' '; auth_id_t  user;
  ' '; a_int32    t : check(t, user);
}
