let (res,pads) = Padsc.p_open None None

let res = Padsc.p_io_fopen pads "data.txt"

let (res,x_pd,x) = Pint_c.pint_read pads

let (res,offset) = Padsc.pchar_lit_scan1 pads ' ' 1 0

let (res,y_pd,y) = Pint_c.pint_read pads

let _ = Padsc.p_io_close pads

let _ = Padsc.p_close pads

let _ = print_endline (string_of_int x)
let _ = print_endline (string_of_int y)
