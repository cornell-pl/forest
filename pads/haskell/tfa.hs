{-- 
Tagged Finite Automato
Implemented by Kathleen Fisher based on the paper
 "NFAs with Tagged Transitions, their Conversion to Determinitic Automata
  and Applications to Regular Expressions"
by  Ville Laurikari  (fl@iki.fi)
--}
import qualified Data.Set as S
import qualified Data.Map as M
import Maybe
import List

{--- Declarations related to TNFA  ---}
type NState = Int
type Tag = Int
type Symbol = Char
type Priority = Int
type NTrans = (NState, [Symbol], Maybe Tag, Priority, NState)
type TNFA = (S.Set NState, S.Set Tag, S.Set Symbol, S.Set NTrans, NState, S.Set NState)

{--- Some helper functions ---}
getNTrans :: TNFA -> S.Set NTrans
getNTrans (states, tags, alphabet, trans, start, finish) = trans

getNFinals :: TNFA -> S.Set NState
getNFinals (states, tags, alphabet, trans, start, finish) = finish

select :: S.Set NTrans -> NState -> [Symbol] -> S.Set NTrans
select nTrans state input = S.filter (\(src,label,_,_,dst) -> state == src && input == label) nTrans

usedSymbols :: TNFA -> S.Set [Symbol]
usedSymbols nMachine = 
  let nTrans = getNTrans nMachine
  in S.fold (\(src,symbols,tag,priority,dst) accum-> 
                if not (symbols == "") then S.insert symbols accum else accum) S.empty nTrans

{-- Example TNFAs --}
exampleTrans :: S.Set NTrans
   = S.fromList [(0,['a'],Nothing,0,0),
                 (0,[],   Just 0, 1,1),
                 (1,['a'],Nothing,2,2),
                 (2,[],   Nothing,3,1),
                 (2,['a'],Nothing,4,3)]

exampleTNFA :: TNFA
        = (S.fromList [0,1,2,3], {--- states ---}
           S.fromList[0],        {--- tags   ---}
           S.fromList['a'],      {--- alphabet ---}
           exampleTrans,         {--- transitions ---}
           0,                    {--- start state ---}
           S.fromList[3])        {--- final states ---}

type TagIndex = Int
type TagPair = (Tag, TagIndex)
type TagSet = S.Set TagPair
type TaggedNState = (NState, TagSet)
type DState = S.Set TaggedNState

data Location where
  Current :: Location
  FromTag :: TagPair -> Location
  deriving Show
type TagCommand = (TagPair,Location)
type DInit = [TagCommand]
type DTrans = (DState, [Symbol], DState, DInit)
type FinishCommand = (Tag,Location)
type DFinish = [(DState,[FinishCommand])]
type TDFA = ([DState], S.Set Symbol, [DTrans], DState, [DState], DInit, DFinish)

getDTrans :: TDFA -> [DTrans]
getDTrans (states,input,trans,start,finishStates,initializers,finalizer) = trans

getDInit :: TDFA -> [TagCommand]
getDInit (states,input,trans,start,finishStates,initializers,finalizer) = initializers

getDFinish :: TDFA -> DFinish
getDFinish (states,input,trans,start,finishStates,initializers,finalizer) = finalizer

getAllTagPairsSet :: DState -> S.Set TagPair  
getAllTagPairsSet dState = S.unions(S.toList (S.map (\(s,k)->k) dState))

getAllTagPairs :: DState -> [TagPair]  
getAllTagPairs dState = (S.toList(getAllTagPairsSet dState))


{--- For each nstate (u,k) in dState,
      for each transtion (u,a,_,_,u') in nTrans of nMachine,
        add (u',k) to result 
Note that a more efficient implementation is possible if avoid fold/select
As written, this code does not follow epsilon transitions.  Should it?
No, I think the epsilons are covered in te_closure.
---}
reach' :: (TNFA, DState, [Symbol], [TaggedNState]) -> [TaggedNState]
reach' (nMachine, dState, input, accum) = 
     let (_,_,_,nTrans,_,_) = nMachine
         doOneNState ((u,k)::TaggedNState) (accum::[TaggedNState]) = 
           let doOneTrans k (src,label,_,_,dst) accum = (dst,k):accum
           in S.fold (doOneTrans k) accum (select nTrans u input)
     in S.fold doOneNState accum dState

reach :: (TNFA, DState, [Symbol]) -> DState
reach (tnfa,dstate,input) = S.fromList(reach'(tnfa,dstate,input,[]))

test = reach(exampleTNFA,S.fromList[(0,S.empty)], "a")

type ClosureItem = (NState, Priority, TagSet)
--teClosure :: TNFA -> DState -> DState
teClosure nMachine dState = 
   let nTrans = getNTrans nMachine
       (initialStack :: [ClosureItem]) = S.fold (\(u,k) accum -> (u,0,k):accum) [] dState
       initialClosure = S.fromList initialStack
       -- For each tag, build nextTagIndex function to compute the next "fresh" index for argument tag
       allTagPairsList = getAllTagPairs dState
       insertOne map (tag,index) = M.insertWith max tag index map
       mergedTagSets :: M.Map Tag TagIndex = foldl insertOne M.empty allTagPairsList
       nextTagIndex tag = if M.member tag mergedTagSets then (mergedTagSets M.! tag) + 1 else 0
       -- Compute closure of state
       computeClosure :: [ClosureItem] -> S.Set ClosureItem -> S.Set ClosureItem
       computeClosure [] closure = closure
       computeClosure ((s,p,k):stack) closure =  
         let eTrans = select nTrans s ""  -- Select epsilon transitions fromList
             processETransition (src,label,edgeTagOpt,edgePri,dst) (lstack,lclosure) = 
               let k' = case edgeTagOpt of
                          Nothing -> (k::TagSet)
                          Just edgeTag -> 
                               let (match,rest) = S.partition (\(tag,index)->tag==edgeTag) k
                                   newPair = (edgeTag, nextTagIndex edgeTag)
                               in S.insert newPair rest
                   {-- Remove edges with low priority: this part is not well-spec'd in paper.  --}
                   (dstClosure,otherClosure) = S.partition (\(nstate,prior,tagset)->nstate==dst) lclosure
                   {-- newly discovered target state shoud replace worseDstStates, but shouldn't be added
                        if any betterDstStates already exist. --}
                   (betterDstStates, worseDstStates) = S.partition (\(nstate,prior,tagset)->prior<edgePri) lclosure
                   {-- find betterDstStates with same item map --}
                   (betterDstKStates, betterDstDifferentKStates) = S.partition (\(nstate,prior,tagset)->tagset==k') betterDstStates
                   {-- If we've discovered a new closureItem, update closure and stack --}
                   residualClosure = S.union otherClosure betterDstDifferentKStates
                   updates = if (S.size betterDstKStates) == 0 then  
                                          ((dst,edgePri,k'):lstack, S.insert (dst,edgePri,k') residualClosure)
                                       else (lstack, residualClosure)
               in updates 
             (rstack,rclosure) = S.fold processETransition (stack,closure) eTrans 
          in computeClosure rstack rclosure
       finalClosure = computeClosure initialStack initialClosure 
       result = S.fromList (S.fold (\(s,p,k) accum ->(s,k):accum) [] finalClosure)
   in --finalClosure
      result

dStart = teClosure exampleTNFA (S.fromList[(0,S.empty)])
-- = fromList [(0,fromList []),(1,fromList [(0,0)])]
dStart' = reach(exampleTNFA, dStart, "a")
-- = fromList [(0,fromList []),(2,fromList [(0,0)])]
dTwo = teClosure exampleTNFA (S.fromList [(0,S.fromList []),(2,S.fromList [(0,0)])])
-- fromList [(0,fromList []),(1,fromList [(0,1)]),(2,fromList [(0,0)])]

type MarkedState = (DState,Bool) -- true indicates a "marked" state

-- Find all new tag pairs and record seting them from the current position
computeInitializer :: DState -> DState -> DInit
computeInitializer srcState dstState = 
   let srcTagPairsSet = getAllTagPairsSet srcState
       dstTagPairsSet = getAllTagPairsSet dstState
       newTagPairsSet = S.difference dstTagPairsSet srcTagPairsSet
       initNewTags = S.fold (\tp clist -> (tp,Current):clist) [] newTagPairsSet
   in initNewTags

type TagPairMap = M.Map TagPair TagPair

-- tagPairMaptoDInit reorder = M.foldWithKey (\tp1 tp2 accum -> (tp1,FromTag tp2):accum) [] reorder
tagPairMaptoDInit reorder = M.foldWithKey (\tp1 tp2 accum -> (tp2,FromTag tp1):accum) [] reorder

tagPairMatchwithReorder :: Tag -> TagIndex -> TagIndex -> TagPairMap -> Maybe TagPairMap
tagPairMatchwithReorder tag oldIndex newIndex reorder = 
    case M.lookup (tag,newIndex) reorder of
      Nothing            -> Just (M.insert (tag,newIndex) (tag,oldIndex) reorder)  -- indices match with new mapping added
      Just (tag',index') -> if oldIndex == index' then Just reorder                -- indices match under existing map
                            else Nothing                                           -- indices don't match

matchEachTag :: [Tag] -> (M.Map Tag TagIndex) -> (M.Map Tag TagIndex) -> TagPairMap -> Maybe TagPairMap
matchEachTag [] oldTagMap newTagMap reorder = Just reorder
matchEachTag (t:tags) oldTagMap newTagMap reorder = 
   case tagPairMatchwithReorder t (fromJust(M.lookup t oldTagMap)) (fromJust(M.lookup t newTagMap)) reorder of 
      Nothing -> Nothing
      Just reorder' -> matchEachTag tags oldTagMap newTagMap reorder'

tagSetsMatchwithReorder :: TagSet -> TagSet -> TagPairMap -> Maybe TagPairMap
tagSetsMatchwithReorder oldTags newTags  reorder = 
   if (S.size oldTags) /= (S.size newTags) then Nothing
   else let oldTagMap = M.fromList(S.toList oldTags)
            newTagMap = M.fromList(S.toList newTags)
            oldTagList = M.keys oldTagMap
            newTagList = M.keys newTagMap
        in if oldTagList /= newTagList then Nothing
           else matchEachTag newTagList oldTagMap newTagMap reorder

matchEachTagSet :: [NState] -> (M.Map NState TagSet) -> (M.Map NState TagSet) -> TagPairMap -> Maybe TagPairMap
matchEachTagSet [] oldTagSetMap newTagSetMap reorder = Just reorder
matchEachTagSet (ns:nss) oldTagSetMap newTagSetMap reorder= 
   case tagSetsMatchwithReorder (fromJust(M.lookup ns oldTagSetMap)) (fromJust(M.lookup ns newTagSetMap)) reorder  of
      Nothing -> Nothing
      Just reorder' -> matchEachTagSet nss oldTagSetMap newTagSetMap reorder'


optStatesMatchWithReorder :: (M.Map NState TagSet) -> [NState] -> DState -> Maybe DInit
optStatesMatchWithReorder newMap newStateList oldState = 
   if (S.size oldState) /= (length newStateList) then Nothing --two states must have same number of nstates
   else let oldMap = M.fromList(S.toList oldState)
            oldStateList = M.keys oldMap
        in if (oldStateList /= newStateList) then Nothing -- two sets of states must be the same
           else case matchEachTagSet newStateList oldMap newMap M.empty of
                Nothing -> Nothing  -- Could not unify states
                Just reorder -> Just (tagPairMaptoDInit reorder)

-- Convenenient test wrapping function; not used in main algorithm.
statesMatchWithReorder :: DState -> DState -> Maybe DInit
statesMatchWithReorder newState oldState = 
  let newMap = M.fromList(S.toList newState)
      newStateList = M.keys newMap
  in optStatesMatchWithReorder newMap newStateList oldState

-- If there is an existing state u' such that u can be converted to it via a series of rewritings
-- return that state and the rewriting.
optGetMatchingState :: (M.Map NState TagSet) -> [NState] -> [MarkedState] -> Maybe (DState, DInit)
optGetMatchingState newMap newStateList [] = Nothing
optGetMatchingState newMap newStateList ((u',mark):us) = 
   case (optStatesMatchWithReorder newMap newStateList  u') of
     Nothing -> optGetMatchingState newMap newStateList us
     Just dInit -> Just(u',dInit)

getMatchingState :: DState -> [MarkedState] -> Maybe (DState, DInit)
getMatchingState newState oldStates = 
  let newMap = M.fromList(S.toList newState)
      newStateList = M.keys newMap
  in optGetMatchingState newMap newStateList oldStates


-- Tests for statesMatchWithReorder
mkDStateC(nstate,tag,index) = (nstate, S.fromList[(tag,index)])

matchStateTest1 = S.fromList [(0,S.fromList []),(1,S.fromList [(0,0)])]
matchStateTest2 = S.fromList [(0,S.fromList []),(1,S.fromList [(0,1)])]
matchStateTest3 = S.fromList [mkDStateC(1,0,1), mkDStateC(0,0,2), mkDStateC(2,0,1), mkDStateC(3,0,0)]
matchStateTest4 = S.fromList [mkDStateC(1,0,2), mkDStateC(0,0,3), mkDStateC(2,0,2), mkDStateC(3,0,1)]
matchStateTest5 = S.fromList [mkDStateC(1,0,2), mkDStateC(0,0,3), mkDStateC(2,0,1), mkDStateC(3,0,1)]
matchStateTest6 = S.fromList [mkDStateC(1,0,2), mkDStateC(0,0,3), mkDStateC(2,0,2), mkDStateC(3,0,1), mkDStateC(4,0,1)]
matchStateTest7 = S.fromList [mkDStateC(2,0,1), mkDStateC(3,0,0)]
matchStateTest8 = S.fromList [mkDStateC(2,0,1), mkDStateC(3,0,1)]

matchTest1 = statesMatchWithReorder matchStateTest1 matchStateTest2 -- Just [((0,1),FromTag (0,0))]
matchTest2 = statesMatchWithReorder matchStateTest3 matchStateTest4 -- Just [((0,1),FromTag (0,0)),((0,2),FromTag (0,1)),((0,3),FromTag (0,2))]
matchTest3 = statesMatchWithReorder matchStateTest3 matchStateTest5  -- Nothing
matchTest4 = statesMatchWithReorder matchStateTest3 matchStateTest6 -- Nothing
matchTest5 = statesMatchWithReorder matchStateTest7 matchStateTest8 --Nothing

tagPairMap = M.insert (0,1) (0,0) (M.insert (0,2) (0,1) (M.insert (0,3) (0,2) M.empty))
tagPairMapSubTest = tagPairMatchwithReorder 0 1 1 tagPairMap  -- Nothing

mst7oldMap = M.fromList(S.toList matchStateTest7)
mst8newMap = M.fromList(S.toList matchStateTest8)
newStateList = M.keys mst8newMap
metsResult = matchEachTagSet newStateList mst7oldMap mst8newMap M.empty
-- End tests for statesMatchWithReorder

-- Test for getMatchingStage
stateFindTest = getMatchingState matchStateTest4 (map (\d ->(d,False)) [matchStateTest3, matchStateTest5,matchStateTest6])
-- Just (fromList [(0,fromList [(0,2)]),(1,fromList [(0,1)]),(2,fromList [(0,1)]),(3,fromList [(0,0)])], // matchStateTest3
--       [((0,1),FromTag (0,0)),((0,2),FromTag (0,1)),((0,3),FromTag (0,2))])
-- End test for getMatchingStage

computeFinalizer :: TNFA -> DState -> ([DState],DFinish)
computeFinalizer nMachine dState = 
  let finalStates :: S.Set NState = getNFinals nMachine
      taggedNStates = S.toList dState
  in case List.find (\(nstate,tagset)->S.member nstate finalStates) taggedNStates of
      Nothing -> ([],[])
      Just (fstate,tagSet) -> 
           let finalizers = S.fold (\(tag,index) accum ->(tag,FromTag(tag,index)):accum) [] tagSet
           in ([dState],[(dState, finalizers)])
      
       
processOneStateInput :: TNFA -> DState -> [Symbol] -> ([MarkedState],[DTrans],[DState],DFinish) -> ([MarkedState], [DTrans],[DState],DFinish)
processOneStateInput nMachine dState input (oldStates,oldTrans,oldFinalStates,oldFinalizers) = 
    let u = teClosure nMachine (reach(nMachine,dState,input))   -- u is state reachable from dState on input followed by epsilon transitions
        inits = computeInitializer dState u                     -- record need to set all fresh tags of u from current position
        (dst,commands,newStates) =                              -- find dst state: either u or existing state with rewrites commands
            case getMatchingState u oldStates of   
               Nothing -> (u, inits, (u,False):oldStates)                           -- add u as unmarked state to set of states
               Just (oldState,rewrites) -> (oldState, inits ++ rewrites,oldStates)  -- state set remains the same in this case.
        newTrans = (dState,input,dst,commands)
        (newFinalStates,newFinalizers) = computeFinalizer nMachine dState
    in (newStates,newTrans:oldTrans,newFinalStates++oldFinalStates,newFinalizers++oldFinalizers)  

testOneStateInput = processOneStateInput exampleTNFA dStart "a" ([(dStart,True)],[],[],[]) 

processOneState :: TNFA -> DState -> ([MarkedState],[DTrans],[DState],DFinish) -> ([MarkedState], [DTrans],[DState],DFinish)
processOneState nMachine dState (allStates,allTrans,allFinalStates,allFinalizers) = 
    let inputs = usedSymbols nMachine
    in S.fold (processOneStateInput nMachine dState) (allStates,allTrans,allFinalStates,allFinalizers) inputs

stateClosure' :: TNFA -> ([MarkedState],[DTrans],[DState],DFinish) -> ([MarkedState],[DTrans],[DState],DFinish)
stateClosure' tnfa (dStates@((dState,True):rest),trans,finals,finishes) = (dStates,trans,finals, finishes) 
                                                    --all states should be marked at this point; we're done
stateClosure' tnfa (dStates@((dState,False):rest),trans,finals,finishes) =
   let (newMarkedStates,newTrans,newFinals,newFinishes) = processOneState tnfa dState (rest++[(dState,True)],trans,finals,finishes)
   in stateClosure' tnfa (newMarkedStates, newTrans,newFinals,newFinishes)

stateClosure :: TNFA -> [DState] -> ([DState],[DTrans],[DState],DFinish)
stateClosure tnfa dStates =
  let markedInputStates = map (\dstate -> (dstate,False)) dStates
      (markedOutputStates, trans,finals,finishers) =  stateClosure' tnfa (markedInputStates,[],[],[])
      outputStates = map (\(dstate,mark) -> dstate) markedOutputStates
  in (outputStates, trans,finals,finishers)

convertTNFAtoTDFA :: TNFA -> TDFA
convertTNFAtoTDFA nMachine @ (nStates,nTags,nAlphabet,nTrans,nStart,nFinish) = 
  let dStart = teClosure nMachine (S.fromList[(nStart,S.empty)])
      dInit  = [ (tp,Current) | tp <- getAllTagPairs dStart]
      (dStates,dTrans,dFinals,dFinishers) = stateClosure nMachine [dStart]
  in (dStates, nAlphabet, dTrans, dStart, dFinals, dInit, dFinishers)  

testDFA = convertTNFAtoTDFA exampleTNFA

{-- Functions related to outputting TDFA to .gv format for graphviz to view --}
fsmStringTemplate :: [String] -> [String] -> String
fsmStringTemplate finishStates transitions = 
  let l1 = "digraph finite_state_machine {\n"
      l2 = "\trankdir=LR;\n"
      l3 = "\tsize=\"8,5\"\n"
      states = (foldl (\saccum state-> saccum++" "++state) "" finishStates)
      l4 = "\tnode [shape = doublecircle];" ++ states ++ "\n"
      l5 = "\tnode [shape = circle];\n"
      body = foldl (\saccum trans-> saccum++"\t"++trans++";\n") "" transitions
      l6 = "}\n"
  in l1 ++ l2 ++ l3 ++ l4 ++ l5 ++ body ++ l6

convertDStateToDString :: M.Map DState () -> DState -> String
convertDStateToDString stateMap dstate = 
 let stateName = show (M.findIndex dstate stateMap)
     cnvTagPair (tag,tagIndex) = "("++(show tag)++", "++(show tagIndex)++")"
     cnvTagSet tagset = "{ "++(S.fold (\tagpair saccum->saccum++(cnvTagPair tagpair)++" ") "" tagset)++" }"
     cnvTagNState (nstate,tagset) = "("++(show nstate)++": "++(cnvTagSet tagset)++")"
     cnvDState = S.fold (\taggedNState saccum -> saccum ++"\n\t" ++(cnvTagNState taggedNState)) "" dstate 
 in  stateName++cnvDState++"\n"

convertDStatesToDString :: M.Map DState () -> [DState] -> String 
convertDStatesToDString stateMap dstates = foldl (\saccum dstate -> saccum++(convertDStateToDString stateMap dstate )) "" dstates

cnvTagPair :: TagPair -> String
cnvTagPair (tag,index) = "("++(show tag)++", " ++(show index)++")"

cnvLoc :: Location -> String
cnvLoc loc = case loc of Current -> "p" ; FromTag tp -> cnvTagPair tp

convertDInitToDString :: DInit -> String
convertDInitToDString dInits = 
  let cnvTagCommand (tp,loc) = (cnvTagPair tp)++" <- "++(cnvLoc loc)
  in foldl (\saccum dInit -> saccum++(cnvTagCommand dInit)++"\\n") "" dInits
    

convertDTransToDString :: M.Map DState () ->  DTrans -> String
convertDTransToDString stateMap (src,input,dst,inits) = 
  let s = show(M.findIndex src stateMap)
      d = show(M.findIndex dst stateMap)
      initSs = convertDInitToDString inits
  in s++" -> "++d++" [ label = \""++input++"/ "++ initSs ++ "\" ];"

makeStartTransition :: M.Map DState () -> DState -> DInit -> String 
makeStartTransition stateMap dStart init = 
  let d = show (M.findIndex dStart stateMap)
      initSs = convertDInitToDString init
  in
    "Start"++" -> "++d++" [ label = \""++ initSs ++ "\" ];"

convertDFinishToString :: FinishCommand -> String
convertDFinishToString (tag, location) = (show tag)++" <- "++(cnvLoc location)

makeFinalTransition :: M.Map DState () -> DState -> [FinishCommand] -> String
makeFinalTransition stateMap dFinish finishCommands = 
  let s = show(M.findIndex dFinish stateMap)
      finishes = foldl (\saccum finishCommand -> saccum++(convertDFinishToString finishCommand)++"\\n") "" finishCommands
  in s++" -> Finish"++" [ label = \""++ finishes ++ "\" ];"

makeFinalTransitions :: M.Map DState () -> DFinish -> [String]
makeFinalTransitions stateMap dFinishes = 
      map (\(dState, finishCommands)-> makeFinalTransition stateMap dState finishCommands) dFinishes

convertFinalStates :: M.Map DState () -> [DState] -> [String]
convertFinalStates stateMap dStates = map (\dstate -> (show(M.findIndex dstate stateMap))) dStates

convertTDFAtoDString :: TDFA -> String
convertTDFAtoDString (dStates,dAlphabet,dTrans,dStart,dFinish,inits,finishes) = 
  let stateMap :: M.Map DState () = foldl (\map dState-> M.insert dState () map) M.empty dStates
      states = "Number of states = "++(show(M.size stateMap)) ++".\n"
      trans = "Number of transitions = "++(show(length dTrans)) ++".\n"
      stateKey = convertDStatesToDString stateMap dStates
      comments = "/*\n "++states++trans++stateKey++"\n*/\n"
      finalStates = convertFinalStates stateMap dFinish 
      startTransition = makeStartTransition stateMap dStart inits
      finalTransitions = makeFinalTransitions stateMap finishes
      transitions = map (convertDTransToDString stateMap) dTrans
  in comments ++ (fsmStringTemplate finalStates (startTransition:transitions++finalTransitions))

outputTDFA :: FilePath -> TDFA -> IO ()
outputTDFA filepath tdfa = writeFile filepath (convertTDFAtoDString tdfa)

{-- Functions related to executing the TDFA --}
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

mapUpdateList :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
mapUpdateList key value map = 
    let f newValue oldValue = newValue ++ oldValue
    in M.insertWith f key [value] map 

type TransMap = M.Map (DState) [([Symbol],DState,DInit)]
type MachineState = (Int, String, DState, OffsetMap)  -- position in original input, remaining input, current state, tagmaps
type FinishMap = M.Map DState [FinishCommand]  -- Final states are in domaon of FinishMap
type OffsetMap = M.Map TagPair Int
type ResultMap = M.Map Tag Int

getOffset :: Location -> Int -> OffsetMap -> Int
getOffset loc position offsetMap = 
  case loc of Current -> position; FromTag oldtp -> fromJust(M.lookup oldtp offsetMap)

doTagCommand :: Int -> OffsetMap -> TagCommand -> OffsetMap 
doTagCommand position offsetMap (tp,loc) = M.insert tp (getOffset loc position offsetMap) offsetMap

doTagCommands :: Int-> OffsetMap -> [TagCommand] -> OffsetMap
doTagCommands position origOffsetMap tagCommands = 
    foldl (\offsetMap tc-> (doTagCommand position offsetMap tc)) origOffsetMap tagCommands

doFinishCommands :: [FinishCommand] -> MachineState -> ResultMap
doFinishCommands fcmds (position,_,_,offsetMap) = 
    foldl (\resultMap (tag,loc) -> M.insert tag (getOffset loc position offsetMap) resultMap) M.empty fcmds

oneStep :: TransMap -> MachineState -> Maybe MachineState
oneStep transitions (position, input, state, offsetMap) = 
     let outEdges = fromJust(M.lookup state transitions) -- Well-defined machine will never produce Nothing here.
         matches input (label,dst,cmds) = List.isPrefixOf label input
     in case List.find (matches input) outEdges of 
           Nothing -> Nothing  -- input is not recognized by machine.
           Just (label,dst,cmds) -> 
             let newPosition = position + length label
                 newInput = fromJust(stripPrefix label input)
                 newOffsetMap = doTagCommands newPosition offsetMap cmds
             in Just (newPosition, newInput, dst, newOffsetMap)

allSteps :: TransMap -> MachineState -> Maybe MachineState
allSteps transMap machineState = 
    case oneStep transMap machineState of
     Nothing -> Nothing   -- Machine had no transition for prefix of input
     Just resultMS@(position,input,state,offsetMap) -> 
        if input == "" then Just resultMS  -- consumed all input
        else allSteps transMap resultMS    -- do the rest of the steps      

isAccepted :: FinishMap -> Maybe MachineState -> Maybe ResultMap
isAccepted finalMap mms = 
  case mms of
    Nothing -> Nothing -- machine stuck with input remaining, so input not accepted.
    Just ms@(position,_,lastState,offsetMap) -> 
       case M.lookup lastState finalMap of
         Nothing -> Nothing  -- lastState was not a final state, so input not accepted.
         Just finishCommands -> Just (doFinishCommands finishCommands ms)  -- accepted, set tag values from offsetMap

runTDFA :: TDFA -> String -> Maybe ResultMap
runTDFA tdfa input = 
  let (states,alphabet,trans,startState,finishStates,initializers,finishers) = tdfa
      transMap = List.foldl (\map (src,input,dst,init)->mapUpdateList src (input,dst,init) map) M.empty trans
      initMachineState = (0,input,startState,doTagCommands 0 M.empty initializers)
      finalMachineStateM = allSteps transMap initMachineState 
      result = isAccepted (M.fromList finishers) finalMachineStateM
  in 
      result

dfarun_a = runTDFA testDFA "a"      -- Nothing
dfarun_aa = runTDFA testDFA "aa"    -- Just (fromList [(0,0)])
dfarun_aaa = runTDFA testDFA "aaa"  -- XXX Just (fromList [(0,0)])
dfarun_b = runTDFA testDFA "b"      -- Nothing