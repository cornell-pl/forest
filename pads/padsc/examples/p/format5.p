record punion dibblerPN(int x){
  auint32 code : code %x == 0;  
  auint32 pn;  //- phone number
};

record pstruct int32record{
  auint32 x;
}

pstruct call{
  int32record x;
  dibblerPN(:x.x:) pn; 
}	    



