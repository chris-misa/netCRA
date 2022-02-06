{-
 - Impl. of Cost Register Automata
 -}

{-# LANGUAGE GADTs #-}

module CRA where

import Data.Function ((&))
import Data.HashMap.Strict as M -- from unordered-containers
import Data.Hashable (Hashable)

-- A primitive operation is an operation of arbitrary arity with all arguments and result in d
data Prim d = Prim Int ([d] -> d)

-- An expression is an AST describing how register values and the current value are combined via primitive operations
data Expression d where
  PrimOp :: Prim d -> [Expression d] -> Expression d
  RegRead :: Int -> Expression d
  CurVal :: Expression d

-- An update operation is a map between each register (by register id) and an expression to be used as the updated value of that register
-- Note that all update operations must supply an expression for all registers (even if the expression is just the identity function)
type UpdateOp d = M.HashMap Int (Expression d)

-- Transition and transition map describe transitions and map between state, symbol pairs and the corresponding transition
data Transition s d = Transition Int s (UpdateOp d) Int
type TransitionMap s d = M.HashMap (Int, s) (Transition s d)

-- Mapping between initial states (by state id) and operations to produce initial register assignments for those states
type InitFunc d = M.HashMap Int (UpdateOp d)

-- Mapping between final states (by state id) and expressions to produce the output value for those final states
type FinalFunc d = M.HashMap Int (Expression d)

-- Map between registers (by register id) and their assigned values
-- Note that register assignments can be partial (e.g., the register assignment handed to the very first update operation)
type RegAssign d = M.HashMap Int d

-- Map between active states (by state id) and their associated register assignments
type State d = M.HashMap Int (RegAssign d)

-- CRA num_states num_registers transitions initialization_func finalization_func
data (Hashable s, Eq s) => CRA s d = CRA Int Int (TransitionMap s d) (InitFunc d) (FinalFunc d)

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
evalExpression ra cur (PrimOp (Prim arity op) children)
  | arity /= length children = error $ "Expression has operation applied to wrong number of arguments: " ++ (show arity) ++ " /= " ++ (show $ length children)
  | otherwise =
      let results = fmap (evalExpression ra cur) children
      in op results

-- Evaluates all expressions of the given update operation and produces an update register assignment
evalUpdateOp :: RegAssign d -> d -> UpdateOp d -> RegAssign d
evalUpdateOp ra cur u = M.mapWithKey doUpdate u
  where doUpdate regId expr = evalExpression ra cur expr

--
-- Main CRA interface
--

initial :: (Hashable s, Eq s) =>
     CRA s d
  -> State d
initial (CRA _ _ _ initFunc _) =
  M.map getInitRegAssign initFunc
  where getInitRegAssign u = evalUpdateOp emptyRegAssign (error $ "Init expr has cur!") u

transition :: (Hashable s, Eq s) =>
     CRA s d
  -> State d
  -> (s, d)
  -> (State d, [d])
transition (CRA _ _ transMap _ finalFunc) state (sym, cur) =
  let state' = state & M.toList & fmap doTransition & M.fromList
      res = state' & M.toList & concatMap doFinal
  in (state', res)
  where doTransition (q, ra) =
          case M.lookup (q, sym) transMap of
            Just (Transition _ _ u q') -> (q', evalUpdateOp ra cur u)
            Nothing -> error $ "No transition for symbol!"
        doFinal (q, ra) =
          case M.lookup q finalFunc of
            Just expr -> [evalExpression ra cur expr]
            Nothing -> []

runList :: (Hashable s, Eq s) =>
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
primConst x = Prim 0 (\[] -> x)

primId :: Prim d
primId = Prim 1 (\[x] -> x)

primUnary :: (d -> d) -> Prim d
primUnary f = Prim 1 (\[x] -> f x)

primBinary :: (d -> d -> d) -> Prim d
primBinary f = Prim 2 (\[x, y] -> f x y)

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

buildCRA :: (Hashable s, Eq s) =>
     [Transition s d]
  -> InitFunc d
  -> FinalFunc d
  -> CRA s d
buildCRA trans init final =
  let keyTrans t@(Transition q s _ _) = ((q, s), t)
      transMap = trans & fmap keyTrans & M.fromList
  in CRA 0 0 transMap init final
