(:

Question:  
  What are the definitions of all terms whose name is mitochondrion inheritance?
:)

<mit-def>
 {
   let $terms := ./*/stanzas/elt
   let $mits := $terms[tvpairs/elt/tag/val = "name" and 
	tvpairs/elt/val/val = "mitochondrion inheritance"]
   return $mits/tvpairs/elt[tag/val = "def"]/val/val
}
</mit-def>
