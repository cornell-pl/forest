precord punion dibblerPN(int x){
  a_uint32 code : code %x == 0;  
  a_uint32 pn;  //- phone number
};

precord pstruct int32record{
  a_uint32 x;
}

pstruct call{
  int32record x;
  dibblerPN(:x.x:) pn; 
}	    



