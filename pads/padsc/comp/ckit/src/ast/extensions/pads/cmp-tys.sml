structure PTys = 

struct
  type pTyInfo = {diskSize : TyProps.diskSize,
		  memChar  : TyProps.memChar}

  fun mergeTyInfo f (r1 : pTyInfo, r2:pTyInfo) =
      {diskSize = TyProps.mergeDiskSize f (#diskSize r1, #diskSize r2),
       memChar  = TyProps.mergeMemChar(#memChar r1,   #memChar  r2)}

  val minTyInfo = {diskSize = TyProps.Size 0, memChar = TyProps.Static}

  type pTyMap = pTyInfo PBaseTys.PBST.map

  val pTys : pTyMap ref = ref PBaseTys.PBST.empty

  fun insert(name:Atom.atom, data:pTyInfo) = 
     pTys := PBaseTys.PBST.insert(!pTys, name, data)

  val find: Atom.atom -> pTyInfo option = fn a => PBaseTys.PBST.find(!pTys, a)
end