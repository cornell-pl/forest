structure PTys = 

struct
  datatype PKind = Typedef | Struct | Union | Array | Enum

  type sTyInfo = {diskSize : TyProps.diskSize,
		  memChar  : TyProps.memChar,
		  endian   : bool,
                  isRecord : bool,
		  containsRecord : bool,
		  largeHeuristic : bool} 

  type pTyInfo = {kind     : PKind,
		  diskSize : TyProps.diskSize,
                  compoundDiskSize : TyProps.compoundSize,
		  memChar  : TyProps.memChar,
		  endian   : bool,
                  isRecord : bool,
		  containsRecord: bool,
	          largeHeuristic: bool,
		  isFile   : bool,
		  numArgs  : int,
		  repName  : string,
		  repInit  : string option,
		  repRead  : string,
		  repClean : string option,
		  pdName   : string,
                  pdTid    : Tid.uid,
		  pdInit   : string option,
		  pdClean  : string option,
		  accName  : string,
		  accInit  : string,
		  accAdd   : string,
		  accReport: string,
		  accClean : string}

  fun mergeTyInfo mergeDiskSizes (r1 : sTyInfo, r2:sTyInfo) =
      {diskSize = mergeDiskSizes (#diskSize r1, #diskSize r2),
       memChar  = TyProps.mergeMemChar(#memChar r1,   #memChar  r2),
       endian   = #endian r1 andalso #endian r2,
       isRecord = #isRecord r1 orelse #isRecord r2,
       containsRecord = #containsRecord r1 orelse #containsRecord r2,
       largeHeuristic = #largeHeuristic r1 orelse #largeHeuristic r2}

  val minTyInfo = {diskSize = TyProps.Size (0,0), 
                    memChar = TyProps.Static, 
                     endian = true, 
                   isRecord = false, 
             containsRecord = false,
             largeHeuristic = false}

  type pTyMap = pTyInfo PBaseTys.PBST.map

  val pTys : pTyMap ref = ref PBaseTys.PBST.empty

  fun insert(name:Atom.atom, data:pTyInfo) = 
     pTys := PBaseTys.PBST.insert(!pTys, name, data)

  val find: Atom.atom -> pTyInfo option = fn a => PBaseTys.PBST.find(!pTys, a)
end