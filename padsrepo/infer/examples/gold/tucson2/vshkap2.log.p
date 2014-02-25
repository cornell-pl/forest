#include "vanilla.p"

Pstruct bracket_t {
  '(';
  Pstring (:')':) v_some_id;
  ')';
};

Punion identifier_t {
  PPip v_ip;
  PPstring v_str;
  bracket_t bracketed_stuff;
};

Pstruct msg_t {
  Popt Pstring (:':':) v_process;
  Popt Pstring_ME (:"/: /":) v_quote;
  Pstring_SE (:Peor:) v_msg;
};

Precord Pstruct entry_t {
  PPdate v_date;
  ' ';
  PPtime v_time;
  ' ';
  identifier_t v_id;
  ' ';
  msg_t v_messsage;
};

Psource Parray entries_t {
  entry_t[];
}
