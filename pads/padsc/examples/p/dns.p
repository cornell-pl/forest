pbits{

}

pstruct header_t{
  b_int16 id;
}

pstruct packet{
  header_t     head;
  question_t   question;  /-- the question for the name server
  answer_t     answer;    /-- RRs answering the question
  auth_t       auth;      /-- RRs pointing toward an authority
  more_t       more;      /-- RRs holding additional information
}
