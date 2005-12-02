for $c in distinct-values(./padsns:PSource/elt/response/val)
return <code> { $c } </code>