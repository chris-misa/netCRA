{-
 - Impl. of Cost Register Automata
 -}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CRA where

import Data.Function ((&))
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M -- from unordered-containers
import qualified Data.IntSet as IS -- from containers
import Data.Hashable (Hashable)
import qualified Data.Set as St
import Data.Maybe

import Utils

-- A primitive operation is an operation of arbitrary arity with all arguments and result in d
data Prim d = Prim String Int ([d] -> d)
instance Show (Prim d) where
  show (Prim id _ _) = id

-- An expression is an AST describing how register values and the current value are combined via primitive operations
data Expression d where
  PrimOp :: Prim d -> [Expression d] -> Expression d
  RegRead :: Int -> Expression d
  CurVal :: Expression d
  ExprError :: String -> Expression d
  deriving (Show)

-- Note: most operations on expressions silently pass ExprError values (except of course evalExpression)
-- Because register assignments are lazy, we use ExprError to hide possible errors in expressions
-- in a way that we can still show an expression with errors without raising an exception.
-- Originally developed to handle uninitialized registers in makeRegistersPerState

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
evalExpression _ _ (ExprError e) = error e


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
translateExpr regMap (ExprError e) = ExprError e -- silently translate errors if this ever happens

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


-- 
-- Start functions for makeDeterministic
------------------------------------------------------------

-- Generates a map from state ids to register maps for each state id
getRegMaps :: Int -> Int -> M.HashMap Int IdMap
getRegMaps numStates numRegs = [1..numStates] & fmap regMapForState & M.fromList

  where regMapForState q = (q, [1..numRegs] & fmap (\r -> (r, [r + ((q - 1) * numRegs)])) & M.fromList)
-- Maps the registers read by the given expression using the given IdMap
mapExprIns :: IdMap -> Expression d -> Expression d
mapExprIns inputMap (PrimOp o e) = PrimOp o (fmap (mapExprIns inputMap) e)
mapExprIns inputMap (RegRead i) = RegRead (head (inputMap M.! i))
mapExprIns inputMap CurVal = CurVal
mapExprIns inputMap (ExprError e) = ExprError e

-- Maps the register ids of all reads (inputs) and writes (outputs) of the given update operation
mapUpdateOpInsOuts :: IdMap -> IdMap -> UpdateOp d -> UpdateOp d
mapUpdateOpInsOuts inputMap outputMap o =
  o & M.mapKeys (head . (M.!) outputMap) & M.map (mapExprIns inputMap)

-- Ensures that all transitions have at least a self-read update operation
eliminateTransNoops :: (Hashable s, Eq s, Ord s) => Int -> TransitionMap s d -> TransitionMap s d
eliminateTransNoops numRegs trans =
  M.map (fmap elimOne) trans
  where elimOne (Transition q sym update t) =
          let update' = update `M.union` ([1..numRegs] & fmap (\i -> (i, RegRead i)) & M.fromList)
          in Transition q sym update' t

-- Ensures that all epsilon transitions have at least a self-read update operation
eliminateETransNoops :: Int -> ETransitionMap d -> ETransitionMap d
eliminateETransNoops numRegs etrans =
  M.map (fmap elimOne) etrans
  where elimOne (ETransition q update t) =
          let update' = update `M.union` ([1..numRegs] & fmap (\i -> (i, RegRead i)) & M.fromList)
          in ETransition q update' t

eliminateInitNoops :: Int -> InitFunc d -> InitFunc d
eliminateInitNoops numRegs = M.map elimOne
  where elimOne update = update `M.union` ([1..numRegs] & fmap (\i -> (i, ExprError "read before write error")) & M.fromList)

--
-- Transforms all register references in the given CRA to be per-state.
-- In otherwords, an independent set of registers is added and updated for each state or the registers
-- for each state are disjoint.
--
-- This simplifies reasoning about combining operations during state combinations in makeDeterministic
--
makeRegistersPerState :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> CRA s d
makeRegistersPerState (CRA numStates numRegs transitions eTransitions init final) =
  let numRegs' = numStates * numRegs
      transitions' = transitions & eliminateTransNoops numRegs & M.map (fmap oneTrans)
      eTransitions' =  eTransitions & eliminateETransNoops numRegs & M.map (fmap oneETrans)
      init' = init & eliminateInitNoops numRegs & M.mapWithKey oneInit
      final' = final & M.mapWithKey oneFinal
  in CRA numStates numRegs' transitions' eTransitions' init' final'

  where regMap = getRegMaps numStates numRegs

        oneTrans (Transition q sym update t) =
          let update' = mapUpdateOpInsOuts (regMap M.! q) (regMap M.! t) update
          in Transition q sym update' t

        oneETrans (ETransition q update t) = 
          let update' = mapUpdateOpInsOuts (regMap M.! q) (regMap M.! t) update
          in ETransition q update' t

        oneInit q = mapUpdateOpInsOuts (error "init should not read anything!") (regMap M.! q)

        oneFinal q = mapExprIns (regMap M.! q)


--
-- Combines the given update operations in serial---simulates updates by o1 being read by o2
-- Most likely want to have passed through makeRegistersPerState before this to ensure o1 and o2 are disjoint.
--
serializeUpdateOps :: UpdateOp d -> UpdateOp d -> UpdateOp d
serializeUpdateOps o1 o2 =
  (o2 & M.map pullPrevOp) `M.union` o1
  where pullPrevOp (PrimOp o e) = PrimOp o (fmap pullPrevOp e)
        pullPrevOp (RegRead i) = case M.lookup i o1 of
                                    Just expr -> expr
                                    Nothing -> RegRead i
        pullPrevOp CurVal = CurVal
        pullPrevOp (ExprError e) = ExprError e

--
-- Computes the epsilon closure of given node q
-- For CRA's, the epsilon closure includes a combined operation that must be executed before the closure is entered
-- to realize any update operations on the subsumed epsilon transitions.
--
-- Assumes all updates are on per-state disjoint registers (e.g., after makeRegistersPerState)
-- Also assumes etrans is unambiguous, i.e., DFS from q in trans must return a proper tree
-- If either of these assumptions are not met, the returned update operation will be wrong.
--
getEpsilonClosure :: Int -> ETransitionMap d -> (UpdateOp d, [Int])
getEpsilonClosure q etrans =
  recEClosure q M.empty [q]
  where recEClosure q combinedOp states =
          case M.lookup q etrans of
            Just ts ->
              let oneETrans (o, ss) (ETransition q update t) =
                    let o' = serializeUpdateOps o update
                        ss' = t:ss
                    in recEClosure t o' ss'
              in foldl oneETrans (combinedOp, states) ts
            Nothing -> (combinedOp, states)

-- 
-- Computes a list of all distinct transition symbols used in the given CRA
--
getTransitionSymbols :: (Hashable s, Eq s, Ord s) => CRA s d -> [s]
getTransitionSymbols (CRA _ _ trans _ _ _) =
  M.elems trans & concatMap (fmap (\(Transition _ sym _ _) -> sym)) & unique

--
-- Converts a possibly non-deterministic CRA to an equivalent deterministic CRA
--
-- The input CRA must be unambiguous!
--
makeDeterministic :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> CRA s d
makeDeterministic cra =
  let cra' = makeRegistersPerState cra
      symbols = getTransitionSymbols cra'
  in makeDeterministicInternal symbols cra'

-- 
-- Internal makeDeterministic impl. assuming registers are already per-state disjoint and distinct transition symbols already known
--
-- The input CRA must be unambiguous!
--
makeDeterministicInternal :: (Hashable s, Eq s, Ord s) =>
     [s]
  -> CRA s d
  -> CRA s d
makeDeterministicInternal symbols (CRA numStates numRegs transitions eTransitions init final) =

  let combineOpsStates (op, ts) (newOp, newTs) = (serializeUpdateOps op newOp, ts ++ newTs)

      -- Compute combined epsilon closure of all initial states and start the work list
      (initOp, initStates) =
        M.keys init
          & fmap (\q -> getEpsilonClosure q eTransitions)
          & foldl combineOpsStates (M.empty, [])
      initWorklist = M.singleton (IS.fromList initStates) 1
      
      -- doWork recursively evaluates the workList
      -- Each iteration:
      --  1. pulls the first oldStates -> newState mapping from the work list;
      --  2. discovers the new sets of states reached from oldStates, computes their corresponding new state index, and adds them to the work list;
      --  3. adds transitions from newState to existing of new state indices in the output state space.
      --  4. pushes the oldStates -> newState mapping onto resultList
      doWork workList resultList trans numNewStates =
        case M.keys workList of
          oldStates:_ ->
            let newState = workList M.! oldStates
                remainingWork = M.delete oldStates workList

                -- oneSymState produces the epsilon closure reached from (original) state q on transition sym
                oneSymState (q, sym) =
                  case M.lookup (q, sym) transitions of
                    Just trans ->
                      let doOneTrans (Transition _ _ op t) =
                            let (eOp, ts) = getEpsilonClosure t eTransitions
                                op' = serializeUpdateOps op eOp
                            in (op', ts)
                      in fmap doOneTrans trans & foldl combineOpsStates (M.empty, [])
                    Nothing -> (M.empty, [])

                -- oneSym processes the combined transition from any state in oldStates on sym producing (maybe) a new combined node and a single combined transition from newState
                oneSym (work, trans, num) sym =
                  let (targetOp, targetStates) = IS.toList oldStates & fmap (\q -> oneSymState (q, sym)) & foldl combineOpsStates (M.empty, [])
                      targetStatesSet = IS.fromList targetStates
                      lookups = [resultList, workList, work] & fmap (\m -> M.lookup targetStatesSet m) & filter isJust
                  in case lookups of
                      (Just t):_ -> 
                        let newTrans = Transition newState sym targetOp t
                        in (work, newTrans:trans, num)
                      [] ->
                        let num' = num + 1
                            newTrans = Transition newState sym targetOp num'
                        in (M.insert targetStatesSet num' work, newTrans:trans, num')

                -- Compute new reached states, new generated transitions and new number of new states
                (newWork, newTrans, numNewStates') = foldl oneSym (M.empty, [], numNewStates) symbols

            in doWork (remainingWork `M.union` newWork) (M.insert oldStates newState resultList) (trans ++ newTrans) numNewStates'

          [] ->
            let transMap = buildTransitionMap trans
            in (resultList, transMap, numNewStates)
      
      (stateMap, transitions', numStates') = doWork initWorklist M.empty [] 1

      init' =
        let op = M.elems init & foldl serializeUpdateOps M.empty
        in M.singleton 1 (serializeUpdateOps op initOp) -- execute first the init op, then any ops from epsilon transitions in initial closure

      final' =
        let oldStateMap = M.toList stateMap
                        & concatMap (\(oldStates, newState) -> [(old, newState) | old <- IS.toList oldStates])
                        & M.fromList
        in final & M.mapKeys ((M.!) oldStateMap)
        -- Assumes that each new combined state includes at most one original final state.
        -- This assumption should hold so long as the original CRA was unambiguous.
        -- (Otherwise there would be two terminating traces for the same input word.)

  in CRA numStates' numRegs transitions' (buildETransitionMap []) init' final'




-- 
-- Start functions for minRegs
------------------------------------------------------------
-- Note: these function all assume the CRA has already passed makeDeterministic
-- In particular, they assume there are no epsilon transitions and a single initial stage
--


-- RegModifier is a union of everything that can modify a register
data RegModifier s d = RMInit (InitFunc d) | RMTrans (Transition s d) | RMFinal (Expression d)
  deriving (Show)

-- Return the set of register ids defined by this modifier
rmDefined :: (Hashable s, Eq s, Ord s) => RegModifier s d -> IS.IntSet
rmDefined (RMInit init)
  | M.size init == 1 = M.elems init & head & M.keys & IS.fromList
  | otherwise = error "More than one initial state in CRA given to minRegs"
rmDefined (RMTrans (Transition _ _ op _)) =
  M.keys op & IS.fromList
rmDefined (RMFinal _) =
  IS.empty

-- Utility to compute the set of register ids read in the given expression
getReadRegs :: Expression d -> IS.IntSet
getReadRegs (PrimOp _ exprs) = fmap getReadRegs exprs & foldl IS.union IS.empty
getReadRegs (RegRead i) = IS.singleton i
getReadRegs CurVal = IS.empty
getReadRegs (ExprError e) = error e

-- Return the set of register ids used (read) by this modifier
rmUsed :: (Hashable s, Eq s, Ord s) => RegModifier s d -> IS.IntSet
rmUsed (RMInit _) =
  IS.empty
rmUsed (RMTrans (Transition _ _ op _)) =
  M.elems op & fmap getReadRegs & foldl IS.union IS.empty
rmUsed (RMFinal expr) =
  getReadRegs expr
  

-- Return a list of this modifier's successors along the given CRA's topology
rmSuccessors :: (Hashable s, Eq s, Ord s) => RegModifier s d -> CRA s d -> [RegModifier s d]
rmSuccessors (RMInit init) cra@(CRA _ _ trans _ _ final)
  | M.size init == 1 =
      let allSymbs = getTransitionSymbols cra
          q = M.keys init & head
          
          -- One type of successor is a final function
          fs = case M.lookup q final of
                Just expr -> [RMFinal expr]
                Nothing -> []

          -- The other type is a transition
          ts = repeat q `zip` allSymbs
              & fmap (trans M.!?)
              & filter isJust
              & fmap fromJust
              & concatMap id
              & fmap RMTrans
      in fs ++ ts
  | otherwise = error "More than one initial state in CRA given to minRegs"
rmSuccessors (RMTrans (Transition _ _ _ t)) cra@(CRA _ _ trans _ _ final) =
  let allSymbs = getTransitionSymbols cra
      
      -- One type of successor is a final function
      fs = case M.lookup t final of
            Just expr -> [RMFinal expr]
            Nothing -> []

      -- The other type is a transition
      ts = repeat t `zip` allSymbs
          & fmap (trans M.!?)
          & filter isJust
          & fmap fromJust
          & concatMap id
          & fmap RMTrans
  in fs ++ ts
rmSuccessors (RMFinal _) _ = []


-- 
-- Given a list of register modifiers (and associated ids), computes the sets of live-in and live-out variables for each register
-- modifier
--
-- Maybe this should deal with maps from the (caller-assigned) id to the register modifier?
-- ...makes looking up the live-in set for other register modifiers easier below...
-- ...except that rmSuccessors would also need to return the register modifier ids (not just a list of register modifiers...
-- ...seems like rmSuccessors has to be updated anyway: can't just be making new RegModifier on the fly like that!
-- ...we might need a more explicit representation (e.g., using HashMaps) of the topology between register modifiers...
-- ... need to reconnect with already allocated RegModifiers in succ for the fixed point thing to work at all.
--
-- TODO: this doesn't compile...
-- liveVariables :: (Hashable s, Eq s, Ord s) => CRA s d -> [(Int, RegModifier s d)] -> [(Int, RegModifier s d, IS.IntSet, IS.IntSet)]
-- liveVariables cra rms = 
--   let initInsOuts = fmap (\(id, rm) -> (id, rm, IS.empty, IS.empty)) rms
--   in recLiveVars initInsOuts
-- 
--   where recLiveVars insOuts =
--           let res = fmap updateOne insOuts
--               done = fmap res fst & foldl (||) False
--               insOuts' = fmap res snd
--           in if done then insOuts'
--              else recLiveVars insOuts'
-- 
--           where updateOne (id, rm, liveIn, liveOut) =
--                   let liveIn' = rmUsed rm `IS.union` (liveOut `IS.difference` rmDefined rm)
--                       liveOut' = let succIns = rmSuccessors rm cra & fmap rmIn -- here's the tricky bit: we need live-in for all succs!!
--                                  in foldl `IS.union` IS.empty succIns
--                   in (id, rm, liveIn', liveOut')
            

-- 
-- Produces a map from state ids to pairs of live-in and live-out registers
--
perStateLiveVariables :: (Hashable s, Eq s, Ord s) => CRA s d -> M.HashMap Int (IS.IntSet, IS.IntSet)
perStateLiveVariables = undefined

-- 
-- Produces an equivalent CRA that uses as few registers as possible
--
minimizeRegisters :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d
minimizeRegisters = undefined






-- 
-- Live variable analysis produces a list/set of live variables going into and out of each initFunc, transition, and finalFun
-- How we represent this depends on how we plan on using it of course...
-- ... need to ultimately produce a map for each initFunc, transition, and finalFun from their original register ids to the updated register ids and a total number of register ids...
--
-- Take 1:
-- let rm be a register modifier
--   we know the set of live registers going into and coming out of rm.
--   (also know mapping from parent rm from original register space to output register space.) <--- there's no sense of parent here: multiple previous register modifiers could have got us here...
--   Any reads in this rm have to be updated w.r.t. parent mapping.
--   Have to decide which registers this rm updates and compute an updated mapping to pass to successor.
--
-- Take 2:
-- Have to some how figure out each group of inter-rm transitions and come up with a mapping that all defining and using rms agree on
-- But aren't these inter-rm groups just the states of the CRA?
--
-- General:
-- Each state in the CRA has a set of rms that happen before that state is active and a set of rms that happen after that state is active.
-- Compute union of all live-out registers of before rms and union of all live-in registers of after rms.
--   ... compute union of both of these sets to get the total number of registers needed at the state.
--   ... compute mappings for writes in the before rms and for reads in the after rms to use only as many registers as needed.
--









