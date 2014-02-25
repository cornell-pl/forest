
BEGIN { flag = 0; srand() }

function myrand ()
{ return (rand()) }

function dept ()
{ x = int(myrand() * 7); 
  if (x == 0) {return "JPN"} else 
 {if (x == 1) {return "COS"} else
 {if (x == 2) {return "HST"} else
 {if (x == 3) {return "MAT"} else
 {if (x == 4) {return "PHY"} else
 {if (x == 5) {return "ORF"} else 
   {return "CHM"}}}}}}
}

function classnum ()
{ return 100 + int(myrand() * 399) 
}

function grade ()
{ x = int(myrand() * 16); 
  if (x == 0) {return "A+"} else 
 {if (x == 1) {return "A"} else
 {if (x == 2) {return "A-"} else
 {if (x == 3) {return "B+"} else
 {if (x == 4) {return "B"} else
 {if (x == 5) {return "B-"} else
 {if (x == 6) {return "C+"} else
 {if (x == 7) {return "C"} else
 {if (x == 8) {return "C-"} else
 {if (x == 9) {return "D"} else
 {if (x == 10) {return "F"} else
 {if (x == 11) {return "P"} else
 {if (x == 12) {return "INC"} else
 {if (x == 13) {return "AUD"} else
   {return "N"}}}}}}}}}}}}}}
}


flag == 2 && NF == 7 {$5 = dept(); $6 = classnum();  $7 = grade(); print}
flag == 2 && NF != 7 {print}

flag < 2 && $0 ~ /^-/ {flag++; print}
flag < 2 && $0 !~ /^-/ {print}
