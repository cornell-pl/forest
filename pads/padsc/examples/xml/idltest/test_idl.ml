open Test

let rec depth_first_walk t n = 
  print_string t;
  print_string (name n);
  let kids = children n in
  let len = (Array.length kids) in
  if (len > 0) then
    begin
      print_string(" has "^(string_of_int len)^" kids:\n");
      Array.iter (depth_first_walk (t^"\t")) kids 
    end
  else
    print_string(" has typed_value "^(typed_value n)^"\n")

let _ = 
  let r = root() in depth_first_walk "" r
