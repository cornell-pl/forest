for $h in ./elt/host[symbolic]
return 
    <host>{ $h/*[not(name(.) = "pd")] }</host>




