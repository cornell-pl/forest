structure PadsState =
struct
  fun reset () = 
     (Select.reset();
      CharClass.reset())
end
