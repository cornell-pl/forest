(:

Question:  
  Show me all terms that are marked obsolete.

:)

   let $terms := ./*/stanzas/elt
   let $obsoletes := $terms[tvpairs/elt/tag/val = "is_obsolete"
                            and tvpairs/elt/val/val = "true"]
   return <obsolete-terms>{$obsoletes}
          </obsolete-terms>
