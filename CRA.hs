{-
 - Impl. of Cost Register Automata
 -}

{-# LANGUAGE GADTs #-}

module CRA where

import Data.Function ((&))
import qualified Data.List as L
import qualified Data.HashMap.Strict as M -- from unordered-containers
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
  -> State d
initial (CRA _ _ _ eTransMap initFunc _) =
  M.map getInitRegAssign initFunc & M.toList & concatMap (doETransition eTransMap) & buildState
  where getInitRegAssign u = evalUpdateOp emptyRegAssign (error $ "Init expr has cur!") u

-- Performs a transition for the given CRA, state, and tagged data word.
-- Produces an updated state and any results produced by the transition.
transition :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> State d
  -> (s, d)
  -> (State d, [d])
transition (CRA _ _ transMap eTransMap _ finalFunc) state (sym, cur) =
  let state' = state & M.toList & concatMap doTransition & concatMap (doETransition eTransMap) & buildState
      res = state' & M.toList & concatMap doFinal
  in (state', res)

  where doTransition (q, ras) =
          case M.lookup (q, sym) transMap of
            Just ts ->
              let tsras = [(t, ra) | t <- ts, ra <- ras]
              in fmap (\(Transition _ _ u q', ra) -> (q', evalUpdateOp ra cur u)) tsras
            Nothing -> []

        doFinal (q, ras) =
          case M.lookup q finalFunc of
            Just expr -> fmap (\ra -> evalExpression ra cur expr) ras
            Nothing -> []


-- Runs the given CRA from its initial state over a list of tagged data words.
runList :: (Hashable s, Eq s, Ord s) =>
     CRA s d
  -> [(s, d)]
  -> [d]
runList a l =
  let s0 = initial a
  in reverse $ runListInternal a s0 l []
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

-- Need: product of states, union of registers (for `op`), other things?
-- In particular: combine two CRAs such that we get product of state spaces and union of registers
-- but also produce some way of tracking which of the new states/registers was associated with which of the original CRAs

-- Idea 1: do a pre-combine which remaps all state and register indices (for transitions as well as for init/final functions)
--  then combining the (independent) CRAs is straightforward

-- Idea 2: all register and state id's are drawn from the same increasing sequence and hence are unique from the beginning.
--  still have the issue of how to get product of states....

-- Idea 3: use arbitry types for register and state ids...can then just use tuples / per-CRA labels to disambiguate...

-- Idea 4 (current impl below): single combine function to produce the core combined CRA also returns maps from ids used in inputs to ids used in output.

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

-- first come up with id maps...
-- then create new CRA applying them...

{-

Look at adding transitions on a per-transition-symbol basis:

For each transition symbol s,
  For each combined start state (L, R),
    If L and R both transitions on s, add transitions (L, R) -> [(L', R') | L' <- all trans from L, R' <- all trans from R]
    else if L transitions on s, add (L, R) -> [(L', R) | L' <- all trans from L]
    else if R transitions on s, add (L, R) -> [(L, R') | R' <- all trans from R]


... have to make sure (prove) that this correctly simulates both L and R on any given sequence of input symbols...
in other words, the projection back to L or R should be in the same state for any given input sequence...
-}

