open Built_ins
let __PML__has_records = false

(* ptype dict_entry = { *)
(*     0x92849899; *)
(*     key: lp_string; *)

(*     0x86; *)

(*     0x92849899; *)
(*     value : lp_string; *)
(*   } *)

(* ptype tNSDictionary = { *)
(*     "NSDictionary"; '\000'; *)
(*     0x9495; *)
(*     num_entries: pb_uint8; *)
(*     (\* that many dictionary entries. *\) *)
(*   } *)

(* ptype tNSMutableDictionary = { *)
(*     "NSMutableDictionary"; '\000'; *)
(*     0x9f95; *)
(*     num_entries: pb_uint8; *)
(*     (\* that many dictionary entries. *\) *)
(*   } *)

(* ptype tNSHTTPURLRequestParameters = { *)
(*     "NSHTTPURLRequestParameters"; '\000'; *)
(*     (\* ... stuff *\) *)
(*     0x92849899; *)
(*     url_request_params : lp_string; *)
(*   } *)

(* ptype tNSURL = { *)
(*     "NSURL"; '\000'; *)
(*     tNSString; *)
(*     tNSHTTPURLRequestParameters; *)
(*   } *)

(* ptype tNSURLRequest = { *)
(*     "NSURLRequest"; '\000'; *)
(*      tNSURL; *)
(*      tNSDictionary; *)

(*   } *)

(* ptype tNSCachedURLResponse = { ... } *)

(* ptype tNSKeyedArchive = { *)
(*     0x040b; *)
(*     "streamtyped"; *)
(*     blah_blah: pstring_SE("/NSURLRequest/"); *)

(*     request     : tNSURLRequest; *)
(*     response    : tNSCachedURLResponse; *)
(*     cached_data : tNSData; *)
(*     0x8692; *)
(*     0x85950086; *)
(*   } *)

ptype lp_len =
    Small_string of [l:pb_uint8 | l < 129 ]
  | Ext_string of '\129' * pb_uint8 * pb_uint8

let int_of_lp_len = function 
   Small_string x -> x 
 | Ext_string (l,h) -> ((h lsl 8) lor l)

(* ptype lp_len = pb_uint8 *)
(* let int_of_lp_len x = x *)

(* a length-prefixed string. *)
ptype lp_string = {
    len256 : lp_len;
    chars : pstring_FW(int_of_lp_len len256);
  }

ptype tNSString = {
    (* 4 bytes *)
    nsstring_prefix: pstring_FW(4);
    '+'; (* hex 2b *)
    the_string : lp_string;
    (* ... more bytes *)
(*     nsstring_bytes: [x:pb_uint32 | x = 0x92849899L]; *)
(*     another_string : lp_string; *)
  }

ptype tNSData = {
    "NSData"; '\000';
    data_prefix : pstring('[');
    '['; data_len : pint; "c]";
    payload : pb_uint8 plist(No_sep, Length_term data_len);
}



ptype tNSKeyedArchive = {
    magic_number: [x : pb_uint16 | x = 0x040b];
    "streamtyped";
    filler1: pstring_SE("/NSString/");
    url: tNSString;
(*     filler2: pstring_SE("/NSData/");     *)
(*     cached_data : tNSData; *)
  }

ptype source = tNSKeyedArchive
