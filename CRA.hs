{-
 - Impl. of Cost Register Automata
 -}

{-# LANGUAGE GADTs #-}

module CRA where

import Data.Function ((&))
import qualified Data.List as L
import qualified Data.HashMap.Strict as M -- from unordered-containers
import qualified Data.IntSet as IS -- from containers
import Data.Hashable (Hashable)
import qualified Data.Set as St
import Data.Maybe

-- A primitive operation is an operation of arbitrary arity with all arguments and result in d
data Prim d = Prim String Int ([d] -> d)
instance Show (Prim d) where
  show (Prim id _ _) = id

-- An expression is an AST describing how register values and the current value are combined via primitive operations
data Expression d where
  PrimOp :: Prim d -> [Expression d] -> Expression d
  RegRead :: Int -> Expression d
  CurVal :: Expression d
  deriving (Show)

-- An update operation is a map between each register (by register id) and an expression to be used as the updated value of that register
-- Note that all update operations must supply an expression for all registers (even if the expression is just the identity function)
type UpdateOp d = M.HashMap Int (Expression d)
showUpdate u = u & M.toList & fmap (\(r, e) -> "      " ++ show r ++ " <- " ++ show e) & L.intercalate "\n"

-- Transition and transition map describe transitions and map between state, symbol pairs and the corresponding transition
data Transition s d = Transition Int s (UpdateOp d) Int
instance (Show s, Show d) => Show (Transition s d) where
  show (Transition q s u t) = 
    show q ++ " -> " ++ show t ++ " on " ++ show s ++ " with \n" ++ showUpdate u

type TransitionMap s d = M.HashMap (Int, s) [Transition s d]

-- ETransition and ETransitionMap describe epsilon transitions.
data ETransition d = ETransition Int (UpdateOp d) Int
instance (Show d) => Show (ETransition d) where
  show (ETransition q u t) = 
    show q ++ " -> " ++ show t ++ " on <epsilon> with \n" ++ showUpdate u

type ETransitionMap d = M.HashMap Int [ETransition d]

-- Mapping between initial states (by state id) and operations to produce initial register assignments for those states
type InitFunc d = M.HashMap Int (UpdateOp d)

-- Mapping between final states (by state id) and expressions to produce the output value for those final states
type FinalFunc d = M.HashMap Int (Expression d)

-- Map between registers (by register id) and their assigned values
-- Note that register assignments can be partial (e.g., the register assignment handed to the very first update operation)
type RegAssign d = M.HashMap Int d

-- Map between active states (by state id) and the active register assignments at each state.
type State d = M.HashMap Int [RegAssign d]

-- CRA num_states num_registers transitions initialization_func finalization_func
data (Hashable s, Eq s, Ord s) =>
     CRA s d = CRA Int Int (TransitionMap s d) (ETransitionMap d) (InitFunc d) (FinalFunc d)

instance (Hashable s, Eq s, Ord s, Show s, Show d) => Show (CRA s d) where
  show (CRA numStates numRegs transitions etransitions initial final) =
    "CRA states: " ++ show numStates ++ " registers: " ++ show numRegs ++ "\n" ++
    "  transitions: \n" ++ showTrans transitions ++
    "  epsilon transitions: \n" ++ showETrans etransitions ++
    "  initial: \n" ++ showInit initial ++
    "  final: \n" ++ showFin final
    where showTrans t = t & M.toList & concatMap snd & fmap (\t -> "    " ++ show t ++ "\n") & concatMap id
          showETrans t = t & M.toList & concatMap snd & fmap (\t -> "    " ++ show t ++ "\n") & concatMap id
          showInit i = i & M.toList & fmap (\(q, u) -> "    " ++ show q ++ ":\n" ++ showUpdate u ++ "\n") & concatMap id
          showFin i = i & M.toList & fmap (\(q, u) -> "    " ++ show q ++ ": " ++ show u) & L.intercalate "\n"

--
-- Helper functions
--

-- An empty register assignment --- should only be used for input to initialization function
emptyRegAssign :: RegAssign d
emptyRegAssign = M.empty

-- Evaluates the given expression using the given register assignments to produce a new value
evalExpression :: RegAssign d -> d -> Expression d -> d
evalExpression ra cur CurVal = cur
evalExpression ra _ (RegRead r) =
  case M.lookup r ra of
    Just v -> v
    Nothing -> error $ "Expression trying to read invalid register id: " ++ (show r)
evalExpression ra cur (PrimOp (Prim _ arity op) children)
  | arity /= length children = error $ "Expression has operation applied to wrong number of arguments: " ++ (show arity) ++ " /= " ++ (show $ length children)
  | otherwise =
      let results = fmap (evalExpression ra cur) children
      in op results

-- Evaluates all expressions of the given update operation and produces an update register assignment.
-- Also copies over any assigned registers not referenced by the update operation.
evalUpdateOp :: RegAssign d -> d -> UpdateOp d -> RegAssign d
evalUpdateOp ra cur u =
  let keys = M.keys ra `L.union` M.keys u
  in keys & fmap doUpdate & M.fromList
  where doUpdate key =
          case (M.lookup key u, M.lookup key ra) of
            (Just expr, _) -> (key, evalExpression ra cur expr)
            (Nothing, Just val) -> (key, val)
            _ -> error "shouldn't happen"

-- Executes a single epsilon transition recursively generating the full set of reached (state, register) assignment pairs
doETransition :: ETransitionMap d -> (Int, RegAssign d) -> [(Int, RegAssign d)]
doETransition eTransMap (q, ra) = 
  doETransRec [(q, ra)] []
  where doETransRec ((q, ra):ras) res =
          case M.lookup q eTransMap of
            Just ts ->
              let cur = error "Found reference to `CurVal` in an epsilon transition"
                  followOne (ETransition _ u q') = (q', evalUpdateOp ra cur u)
              in doETransRec ((fmap followOne ts) ++ ras) res
            Nothing -> doETransRec ras ((q, ra):res)
        doETransRec [] res = res

doETransition' :: ETransitionMap d -> FinalFunc d -> d -> (Int, RegAssign d) -> ([(Int, RegAssign d)], [d])
doETransition' eTransMap finalF cur (q, ra) = 
  doETransRec [(q, ra)] [] []
  where doETransRec ((q, ra):ras) res outs =
          let outs' =
                case M.lookup q finalF of
                  Just expr -> (evalExpression ra cur expr):outs
                  Nothing -> outs
          in case M.lookup q eTransMap of
              Just ts ->
                let cur = error "Found reference to `CurVal` in an epsilon transition"
                    followOne (ETransition _ u q') = (q', evalUpdateOp ra cur u)
                in doETransRec ((fmap followOne ts) ++ ras) res outs'
              Nothing -> doETransRec ras ((q, ra):res) outs'
        doETransRec [] res outs = (res, outs)

-- Builds a CRA state from a list of (state id, register assignment pairs)
buildState :: [(Int, RegAssign d)] -> State d
buildState ras =
  ras & foldl addToMap M.empty
  where addToMap m (q, ra) = M.alter f q m
          where f Nothing = Just [ra]
                f (Just prev) = Just (ra:prev)

--
-- Main operational CRA interface
--

-- Produces the initial state of the given CRA
initial :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> (State d, [d])
initial (CRA _ _ _ eTransMap initFunc finalFunc) =
  M.map getInitRegAssign initFunc & M.toList & fmap (doETransition' eTransMap finalFunc cur) & flattenOutput [] []
  where cur = error "Init expr has cur!"
        getInitRegAssign u = evalUpdateOp emptyRegAssign cur u
        flattenOutput flatRas flatOuts ((ras, outs):theRest) = flattenOutput (ras ++ flatRas) (outs ++ flatOuts) theRest
        flattenOutput flatRas flatOuts [] = (buildState flatRas, flatOuts)

-- Performs a transition for the given CRA, state, and tagged data word.
-- Produces an updated state and any results produced by the transition.
transition :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> State d
  -> (s, d)
  -> (State d, [d])
transition (CRA _ _ transMap eTransMap _ finalFunc) state (sym, cur) =
  state & M.toList & concatMap doTransition & fmap (doETransition' eTransMap finalFunc cur) & flattenOutput [] []

  where doTransition (q, ras) =
          case M.lookup (q, sym) transMap of
            Just ts ->
              let tsras = [(t, ra) | t <- ts, ra <- ras]
              in fmap (\(Transition _ _ u q', ra) -> (q', evalUpdateOp ra cur u)) tsras
            Nothing -> []

        flattenOutput flatRas flatOuts ((ras, outs):theRest) = flattenOutput (ras ++ flatRas) (outs ++ flatOuts) theRest
        flattenOutput flatRas flatOuts [] = (buildState flatRas, flatOuts)

-- Runs the given CRA from its initial state over a list of tagged data words.
runList :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> [(s, d)]
  -> [d]
runList a l =
  let (s0, r0) = initial a
  in r0 ++ (reverse $ runListInternal a s0 l [])
  where runListInternal a s (x:xs) res =
          let (s', ys) = transition a s x
          in runListInternal a s' xs (ys ++ res)
        runListInternal a s [] res = res

--
-- Utilities
--

primConst :: d -> Prim d
primConst x = Prim "const" 0 (\[] -> x)

primId :: Prim d
primId = Prim "id" 1 (\[x] -> x)

primUnary :: (d -> d) -> Prim d
primUnary f = Prim "unary" 1 (\[x] -> f x)

primBinary :: (d -> d -> d) -> Prim d
primBinary f = Prim "binary" 2 (\[x, y] -> f x y)

exprConst :: d -> Expression d
exprConst x = PrimOp (primConst x) []

exprUOp :: (d -> d) -> Expression d -> Expression d
exprUOp f x = PrimOp (primUnary f) [x]

exprBinOp :: (d -> d -> d) -> Expression d -> Expression d -> Expression d
exprBinOp f l r = PrimOp (primBinary f) [l, r]

--
-- Builder functions to wrap the internal types
--

-- Builds an update operation for a list of expressions
-- e.g., [expression for register 1, expression for register 2, ...]
-- Note that this must supply an expression for all registers...
buildUpdateOp ::
     [Expression d]
  -> UpdateOp d
buildUpdateOp exprs = zip [1..] exprs & M.fromList

-- Build an initialization function from a list of (init state id, update operation) pairs
buildInitFunc ::
  [(Int, UpdateOp d)]
  -> InitFunc d
buildInitFunc = M.fromList

-- Build a finalization function from a list of (final state id, finalization expression) pairs
buildFinalFunc ::
  [(Int, Expression d)]
  -> FinalFunc d
buildFinalFunc = M.fromList


buildTransitionMap :: (Hashable s, Eq s, Ord s) =>
     [Transition s d]
  -> TransitionMap s d
buildTransitionMap trans =
  let keyTrans t@(Transition q s _ _) = ((q, s), t)
  in trans & fmap keyTrans & foldl addToMap M.empty
  where addToMap m (k, t) = M.alter f k m
          where f Nothing = Just [t]
                f (Just prev) = Just (t:prev)

buildETransitionMap ::
     [ETransition d]
  -> ETransitionMap d
buildETransitionMap trans =
  let keyTrans t@(ETransition q _ _) = (q, t)
  in trans & fmap keyTrans & foldl addToMap M.empty
  where addToMap m (k, t) = M.alter f k m
          where f Nothing = Just [t]
                f (Just prev) = Just (t:prev)

buildCRA :: (Hashable s, Eq s, Ord s) =>
     Int
  -> Int
  -> [Transition s d]
  -> [ETransition d]
  -> InitFunc d
  -> FinalFunc d
  -> CRA s d
buildCRA numStates numRegs trans etrans init final =
  CRA numStates numRegs (buildTransitionMap trans) (buildETransitionMap etrans) init final


--
-- Primitive operations for combinators
--

type IdMap = M.HashMap Int [Int]

idMapFromList :: [(Int, [Int])] -> IdMap
idMapFromList = M.fromList

-- Translate all register id references in the given expression according to the given register id map
translateExpr :: IdMap -> Expression d -> Expression d
translateExpr regMap (PrimOp op children) = PrimOp op (fmap (translateExpr regMap) children)
translateExpr regMap (RegRead r) = RegRead (M.lookup r regMap & fromJust & head)
translateExpr regMap CurVal = CurVal

-- Translate all register ids in the given update operation according to the given register id map
translateUpdateOp :: IdMap -> UpdateOp d -> UpdateOp d
translateUpdateOp regMap u =
  u & M.toList & fmap doUpdate & M.fromList
  where doUpdate (r, exp) =
          let r' = M.lookup r regMap & fromJust & head
              exp' = translateExpr regMap exp
          in (r', exp')

-- Translate both register and state ids in the init function according to the given maps
translateInit :: IdMap -> IdMap -> InitFunc d -> InitFunc d
translateInit stateMap regMap init = 
  init & M.toList & concatMap doUpdate & M.fromList
  where doUpdate (q, u) = [(q', translateUpdateOp regMap u) | q' <- stateMap M.! q]

translateFinal :: IdMap -> IdMap -> FinalFunc d -> FinalFunc d
translateFinal stateMap regMap final =
  final & M.toList & concatMap doUpdate & M.fromList
  where doUpdate (q, e) = [(q', translateExpr regMap e) | q' <- stateMap M.! q]


--
-- Produces a CRA that combines two input CRAs by executing
-- transitions of both input CRAs over the same input on a shared
-- state and register space.
--
-- Assumptions:
-- - states and registers are numbered consecutively starting with 1 (e.g., 1, 2, 3, 4)
--
-- Output: (combinedCRA, (leftStateMap, leftRegisterMap), (rightStateMap, rightRegisterMap))
-- 
-- Note that the init and final functions of the combinedCRA are left empty because each
-- QRE operator has a different semantic about which states should be init/final.
--
combine :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> CRA s d
  -> (CRA s d, (IdMap, IdMap), (IdMap, IdMap))
combine (CRA numStatesL numRegsL transitionsL eTransitionsL initL finalL) (CRA numStatesR numRegsR transitionsR eTransitionsR initR finalR) =

  let numStates = numStatesL * numStatesR
      stateMapL = [(i, [1..numStatesR] & fmap (+ ((i - 1) * numStatesR))) | i <- [1..numStatesL]] & idMapFromList
      stateMapR = [(i, [0..(numStatesL-1)] & fmap (* numStatesR) & fmap (+ i)) | i <- [1..numStatesR]] & idMapFromList

      numRegs = numRegsL + numRegsR
      regMapL = [(i, [i]) | i <- [1..numRegsL]] & idMapFromList
      regMapR = [(i, [i + numRegsL]) | i <- [1..numRegsR]] & idMapFromList

      tranSyms = (M.keys transitionsL ++ M.keys transitionsR) & fmap snd & St.fromList

      --
      -- Combine left and right transitions
      -- The resulting transition will simulate both transitions occuring
      -- simultaneously in the combined state and register spaces
      --
      combineTransitions (Transition qL sL updateL tL) (Transition qR sR updateR tR) =
        let s = if sL == sR then sL else (error $ "Something went wrong: trying to combine transitions for diff. symbols")
            q' = (stateMapL M.! qL) `L.intersect` (stateMapR M.! qR)
            update' = (translateUpdateOp regMapL updateL) `M.union` (translateUpdateOp regMapR updateR)
            t' = (stateMapL M.! tL) `L.intersect` (stateMapR M.! tR)
        in (q' `zip` t') & fmap (\(q', t') -> Transition q' s update' t')

      --
      -- Updates the state and register ids of the given transition
      -- according to {stateMap, regMap}Us, but limited to only the states given by stateMapThem and theirState
      --
      updateTransStatesRegs stateMapUs regMapUs stateMapThem theirState (Transition q s update t) =
        let theirState' = stateMapThem M.! theirState
            q' = (stateMapUs M.! q ) & L.intersect theirState'
            update' = translateUpdateOp regMapUs update
            t' = (stateMapUs M.! t) & L.intersect theirState'
        in (q' `zip` t') & fmap (\(q', t') -> Transition q' s update' t')

      --
      -- Produces a list of transitions in the combined state, register space for the given
      -- symbol, left state id, and right state id
      --
      oneTransition (s, (l, r)) =
        case (M.lookup (l, s) transitionsL, M.lookup (r, s) transitionsR) of
          (Just lTrans, Just rTrans) -> [combineTransitions lt rt | lt <- lTrans, rt <- rTrans] & concatMap id
          (Just lTrans, Nothing) -> concatMap (updateTransStatesRegs stateMapL regMapL stateMapR r) lTrans
          (Nothing, Just rTrans) -> concatMap (updateTransStatesRegs stateMapR regMapR stateMapL l) rTrans
          _ -> []

      transitions =
        [(s, (l, r)) | s <- St.toList tranSyms, l <- [1..numStatesL], r <- [1..numStatesR]]
        & concatMap oneTransition
        & buildTransitionMap

      -- TODO: e transition combination starts here....needs testing!!!

      combineETransitions (ETransition qL updateL tL) (ETransition qR updateR tR) =
        let q' = (stateMapL M.! qL) `L.intersect` (stateMapR M.! qR)
            update' = (translateUpdateOp regMapL updateL) `M.union` (translateUpdateOp regMapR updateR)
            t' = (stateMapL M.! tL) `L.intersect` (stateMapR M.! tR)
        in (q' `zip` t') & fmap (\(q', t') -> ETransition q' update' t')

      updateETransStatesRegs stateMapUs regMapUs stateMapThem theirState (ETransition q update t) =
        let theirState' = stateMapThem M.! theirState
            q' = (stateMapUs M.! q ) & L.intersect theirState'
            update' = translateUpdateOp regMapUs update
            t' = (stateMapUs M.! t) & L.intersect theirState'
        in (q' `zip` t') & fmap (\(q', t') -> ETransition q' update' t')

      oneETransition (l, r) =
        case (M.lookup l eTransitionsL, M.lookup r eTransitionsR) of
          (Just lETrans, Just rETrans) -> [combineETransitions lt rt | lt <- lETrans, rt <- rETrans] & concatMap id
          (Just lETrans, Nothing) -> concatMap (updateETransStatesRegs stateMapL regMapL stateMapR r) lETrans
          (Nothing, Just rETrans) -> concatMap (updateETransStatesRegs stateMapR regMapR stateMapL l) rETrans
          _ -> []

      eTransitions =
        [(l, r) | l <- [1..numStatesL], r <- [1..numStatesR]]
        & concatMap oneETransition
        & buildETransitionMap

  in (CRA numStates numRegs transitions eTransitions M.empty M.empty, (stateMapL, regMapL), (stateMapR, regMapR))


-- Combines the given update operations in serial---simulates updates by o1 being read by o2
serializeUpdateOps :: UpdateOp d -> UpdateOp d -> UpdateOp d
serializeUpdateOps o1 o2 =
  (o2 & M.map pullPrevOp) `M.union` o1
  where pullPrevOp (PrimOp o e) = PrimOp o (fmap pullPrevOp e)
        pullPrevOp (RegRead i) = case M.lookup i o1 of
                                    Just expr -> expr
                                    Nothing -> RegRead i
        pullPrevOp CurVal = CurVal


-- Maps the register ideas of all reads (inputs) and writes (outputs) of the given update operation
mapUpdateOpsInsOuts :: IdMap -> IdMap -> UpdateOp d -> UpdateOp d
mapUpdateOpsInsOuts inputMap outputMap o =
  o & M.mapKeys (head . (M.!) outputMap) & M.map mapInputs
  where mapInputs (PrimOp o e) = PrimOp o (fmap mapInputs e)
        mapInputs (RegRead i) = RegRead (head (inputMap M.! i))
        mapInputs CurVal = CurVal


-- Generates a map from state ids to register maps for each state id
getRegMaps :: Int -> Int -> M.HashMap Int IdMap
getRegMaps numStates numRegs = [1..numStates] & fmap regMapForState & M.fromList
  where regMapForState q = (q, [1..numRegs] & fmap (\r -> (r, [r + ((q - 1) * numRegs)])) & M.fromList)

-- Computes the epsilon closure of given node q
-- For CRA's, the epsilon closure includes a combined operation that must be executed before the closure is entered
-- to realize any update operations on the subsumed epsilon transitions.
-- Assumes stateRegMap maps each state to a disjoint set of registers.
getEpsilonClosure :: Int -> ETransitionMap d -> M.HashMap Int IdMap -> (UpdateOp d, [Int])
getEpsilonClosure q etrans stateRegMap =
  recEClosure q M.empty [q]
  where recEClosure q combinedOp states =
          case M.lookup q etrans of
            Just ts ->
              let oneETrans (o, ss) (ETransition q update t) =
                    let o' = serializeUpdateOps o (mapUpdateOpsInsOuts (stateRegMap M.! q) (stateRegMap M.! t) update)
                        ss' = t:ss
                    in recEClosure t o' ss'
              in foldl oneETrans (combinedOp, states) ts
            Nothing -> (combinedOp, states)

-- Ensures that all epsilon transitions have at least a self-read update operation
eliminateNoops :: Int -> ETransitionMap d -> ETransitionMap d
eliminateNoops numRegs etrans =
  M.map (fmap elimOne) etrans
  where elimOne (ETransition q update t) =
          let update' = update `M.union` ([1..numRegs] & fmap (\i -> (i, RegRead i)) & M.fromList)
          in ETransition q update' t

-- WARNING: need to eliminateNoops before going into getEpsilonClosure!

-- Converts a possibly non-deterministic CRA to an equivalent deterministic CRA
makeDeterministic :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> CRA s d
makeDeterministic (CRA numStates numRegs transitions eTransitions init final) =
  let numRegs' = numStates * numRegs
      
      worklist = M.singleton (M.keys init & IS.fromList) 1
      
      doWork wl rl trans =
        case M.toList wl of
          (origStates, newState):theRest ->
            -- TODO:
            -- For each symbol:
            --    for each transition from any of origStates on this symbol, compute the epsilon closure of the target state
            --    compute union of these closures as the potential new node (as an int set)
            --    look up the potential new node's original states in theRest and rl
            --       if it's not in either, add it (i.e., the set of original states, and the new nodes' new state id) to theRest
            --    add transition (to trans) in new state space whose operation is a combination of all update operations accumulated in the above process
            --
            doWork (M.fromList theRest) (M.insert origStates newState rl) trans
          [] -> (rl, trans)
      

  in undefined

{-
 - Inputs:
 -  - a non-deterministic, unambiguous CRA
 -  - the set of all possible transition symbols
 -
 - Q <- closure(start states)
 - while there is a node x in Q that doesn't have a transition for a symbol s,
 -  compute the closure of all states reached from x on s
 -  take their union and call it y
 -  if y is not in Q, add it <---- need equality between nodes ... based on what? which or the original states they represent?
 -  add an edge from x to y on s that executes all the different update operations accumulated (incld from computed closures)
 -
 - worklist implementation...
 -
 - add Q to work list
 - while work list is not empty
 -  take a node from work list and go through the process above for each symbol, adding any new nodes to the worklist
 -  add the worked on node to the result list
 -
 - The work list and results list are maps from sets of states (IntSets!) to nodes in the new CRA
 - because each time we compute a target set of states (union of closures) we have to check if we've considered that set of states before or note by looking it up in these lists.
 -
 -}

-- NEED TO ASSUME INPUT CRA is unambiguous---otherwise this falls apart because you might have more than one register valuation at each state!

{-
 - Make deterministic simulates evaluation keeping track of the sets of active state at each step
 - Here, each transition might do different updates so each combined state needs an independent set of registers
 - for each original state.
 -
 - Transitions in the output then combine operations of all transitions between the state sets in the input, but
 - separate the sets of registers read from and written to based on the states of the transition on the input side.
 -
 - The kind of interesting thing is that registers are a CRA-global concept whereas here we're talking about sets of registers
 - at each (combined) states being dependent on the (local) transitions / absorbed states...
 -  -> the easiest thing to do whould be to initial set up a set of registers for each state so there's a single deterministic
 -     and global state-to-register-set mapping to use during this process, then later go back and minimize the number of registers
 -     required...
 -     -> map any register reads to the register set of the originating state
 -     -> map any register writes to the register set of the destination state
 -
 - Also need to deal with epsilon transitions.... should this be a separate step (perhaps before making deterministic)?
 -  -> The epsolon closure of a node is a set of nodes and a combined transition (to be executed before reaching the combined node!)
 -     that updates the register sets of all nodes reached via an epsilon-transition.
 -  -> The tricky thing is that this combined transition must be generated for each path (and subpath) along the epsilon transitions
 -     eminating from the node which requires sequential combination of the original transition operations...
 -
 - Tasks:
 - - implement serial combination of transition operations (preserving register ids)
 - - implement the main bit
 - - implement register minimization (or maybe wait till after DFA minimization to do this?)
 -}
