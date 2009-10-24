structure ClusterUnions = 
struct 
  open Tokens 

  val clusterThreshold = 0.15
  fun cmpChar (c1, c2) = c1 = c2
  fun eqLToken ((t1,l1), (t2,l2)) = eqToken(t1,t2)

  val min = Int.min
  val max = Int.max
  fun minimum a b c = if a < b then min (a,c) else min (b,c)
  fun intDiv (x,y) = (Real.fromInt x) / (Real.fromInt y)

  type 'a Table = ('a array) array
  val sub = Array.sub
  fun lookup (table: 'a Table) ((i,j) : int * int)   = sub (sub(table,i), j)
  fun getCol (table : 'a Table) (i:int) = sub(table,i)

  fun mkTable n m = 
      let val init_col = Array.array (m,0) 
	  val table : int Table = Array.array(n,init_col)
	  fun initCol i = if i < n then (
					 let val newCol = Array.array(m,0)
					 in (
					     Array.update(newCol,0, i);
					     Array.update(table, i, newCol);
					     initCol (i+1)) 
					 end ) else ()
	  fun initRow row j = if j < m then (
					     Array.update(row, j, j);
					     initRow row (j+1)) 
			      else ()
      in
	  initCol 0;
	  initRow (getCol table 0) 0;
	  table
      end


  fun distance teq x y = case (length x, length y) 
      of (0,n) => n
    | (m,0) => m
    | (m,n) => 
	  let val table = mkTable (m+1) (n+1) 
	      val ax = Array.fromList x
	      val ay = Array.fromList y
	      fun loopi i = if i > m then () else
		  let val s = getCol table i
		      val t = getCol table (i-1)
		      fun loopj j = if j > n then () else
			  let val cost = if teq(sub(ax,i-1), sub(ay,j-1)) then 0 else 1
			      val next = minimum
				  (sub(t,j) +1)       (* Deletion *)
				  (sub(s,j-1) +1)     (* Insertion *)
				  (sub(t,j-1) + cost) (* Substitution *)
			  in (Array.update(s,j,next));
			      loopj (j+1)
			  end
		  in (loopj 1;
		      loopi (i+1))
		  end
	      
	  in (loopi 1;
	      lookup table (m,n))
	  end

  (* Compute the edit distance between two token sequences *)
  (* Expected invariant: recordDistance is always <= 1 *)
  fun recordDistance (r1, r2) = let 
      val len1 = length r1
      val len2 = length r2
      in
	  intDiv  (distance eqLToken r1 r2, max (len1,len2))
      end

  (* Compute the edit distance between r1 and rlist up to clusterThreshold. *)
  (* Distance defined to be minimum distance between r1 and any item in rlist *)
  (* If distance is less than clusterThreshold, r will be put in rlist, so further checking is not necessary. *)
  fun minDistance (computeDistance, r1, rlist) = let
      fun findMin current cl = case cl
           of [] => current
           | (r::rs) => let val distance = computeDistance (r1, r)
	               in
			   if distance < clusterThreshold then distance
			   else findMin (Real.min (distance, current)) rs
		       end
      in
	  findMin 2.0 rlist    (* 2.0 is bigger than any possible real distance *)
      end 

  fun distClusterToRec (cluster,r)  = minDistance (recordDistance, r, cluster)
  fun distClusterToCluster (c1, c2) = minDistance (distClusterToRec, c1, c2)

  type record_t = (Token * {beginloc:int, endloc:int, lineNo:int, recNo:int}) list 
  type cluster_t = record_t list   

  fun cmpLoc (l1,l2) = case (l1,l2) 
      of ([],_) => true
       | (l,[]) => false
       | ((t1,{beginloc=bl1,endloc=el1,lineNo=lineNo1,recNo=recNo1})::ltokens1, 
	  (t2,{beginloc=bl2,endloc=el2,lineNo=lineNo2,recNo=recNo2})::ltokens2) => lineNo1 < lineNo2

  fun split ls = 
    let fun sp []  (l1,l2) = (l1,    l2)
          | sp [x] (l1,l2) = (x::l1, l2)
          | sp (x::y::rest) (l1,l2) = sp rest (x::l1, y::l2)
    in sp ls ([],[])
    end

 fun quicksort cmp xs = let
   fun qs [] = []
     | qs [x] = [x]
     | qs (pivot::xs) = let
         fun ltP x = cmp (x, pivot)
         val less = List.filter ltP xs
 	 val more = List.filter (not o ltP) xs
         in
           (qs less) @ [pivot] @ (qs more)
         end
   in
     qs xs
   end 

  fun mergeClusters (rs : record_t list) = 
      let val initialClusters = map (fn x => [x]) rs
          fun mergeOneCluster (c : cluster_t) (clusters: cluster_t list)  (eqCluster : cluster_t, others: cluster_t list) = case clusters 
	      of [] => (eqCluster::others)
              | (c1::cs) => if distClusterToCluster(c,c1) <= clusterThreshold 
		            then mergeOneCluster c cs (eqCluster @ c1, others)
			    else mergeOneCluster c cs (eqCluster, c1::others)
          fun mergeAllClusters (m1: cluster_t list) (m2:cluster_t list) = case m1
              of [] => m2 
              | (m::ms) => let
		  val m_result = mergeOneCluster m m2 (m,[])
		  in
		      mergeAllClusters ms m_result
		  end
          fun merge clusters = case clusters 
              of [] => []
              |  [x] => [x] 
              |  _ => let val (c1,c2) = split clusters
			  val m1 = merge c1
			  val m2 = merge c2
		      in
			  mergeAllClusters m1 m2
		      end
	  val mergedClusters = merge initialClusters
      in
	  map (quicksort cmpLoc) mergedClusters
      end


  (* Testing code *)

  val t1 = explode "parallelogram"
  val t2 = explode "parallelograms"
  val t3 = explode "kitten"
  val t4 = explode "fred"
  val t5 = explode "fret" 
  val t6 = explode "sitten"
  val t7 = explode "mitten"
  val t8 = explode "pred"

  val input = [t6,t1,t2,t3,t4,t5,t5,t6,t7,t8]
  val input2 = [t3,t6,t3,t6]

  fun cmpChar (c1, c2) = c1 = c2
  val tokenEq = cmpChar

  fun testStrings x y = let
			    val xs = explode x 
			    val ys = explode y
			in (distance cmpChar xs ys,
			    distance cmpChar ys xs)
			end

  val test = distance cmpChar t1 t2
  (* End testing code *)
end