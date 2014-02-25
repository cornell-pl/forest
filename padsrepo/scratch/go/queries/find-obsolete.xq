(:

Question:  
  Show me all terms that are marked obsolete.

:)

   let $terms := ./*/stanzas/elt
   let $obsoletes := $terms[tvpairs/elt[tag/val = "is_obsolete" and val/val = "true"]]
   return <obsolete-terms>{
     for $t in $obsoletes return
       <term>
         <id>{fn:data($t/id/val/val)}</id>
         <def>{$t/tvpairs/elt[tag/val = "def"]/val/val/text()}</def>
       </term>
   }</obsolete-terms>
