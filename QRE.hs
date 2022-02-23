{-
 - QRE combinators over CRAs
 -}

module QRE where

import Data.Hashable (Hashable)
import Data.HashMap.Strict as M -- from unordered-containers
import Data.Function ((&))

import CRA

-- We need at least the following:
-- TODO: query combinators should have input types as well as output types, right?
-- ... maybe we need a higher-level wrapping to express both the parallel-execution combinator form and the merged-execution form?

-- The tricky part is that whatever we wrap with must also be translated/implementable in the switch...

{-

What all do we need?

predicates of some kind: tests based on values, packet fields, boolean operators

  the interesting thing is that is every symbol is the result of a predicate, then essentially S is always just {T, F}...
  there's also an interesting question in how closely to tie to network monitoring...

key-based stuff: splitting into substeams based on a key-generating function, then merging the results.

-}


--
-- Match a single symbol and emit a constant value when the symbol matches
-- Note that `expr` should not have any register reads (enforce this later...)
--
atom :: (Hashable s, Eq s, Ord s) => s -> s -> Expression d -> CRA s d
atom yesSym noSym expr =
  let noop = buildUpdateOp []
      initF = buildInitFunc [(1, noop)]
      finalF = buildFinalFunc [(2, expr)]
      transitions = [
          Transition 1 yesSym noop 2,
          Transition 1 noSym noop 1,
          Transition 2 yesSym noop 2,
          Transition 2 noSym noop 1
        ]
  in buildCRA 2 0 transitions initF finalF


--
-- Run two CRAs in parallel and combine the outputs (when defined) using the given function
--
op :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d -> Prim d -> CRA s d
op l@(CRA _ _ _ initL finalL) r@(CRA _ _ _ initR finalR) combOp =

  let (CRA numStates numRegs transitions _ _, (stateMapL, regMapL), (stateMapR, regMapR)) = combine l r

      -- States of init are the intersection of init state from left and right
      -- The update op at each state is the union of updates from left and right (should assign to disjoint registers)
      initF = M.intersectionWith M.union (translateInit stateMapL regMapL initL) (translateInit stateMapR regMapR initR)

      -- States of final are the intersection of final states from left and right
      -- The expresion at each state combines left and right expressions using combOp
      finalF = M.intersectionWith combExprs (translateFinal stateMapL regMapL finalL) (translateFinal stateMapR regMapR finalR)
        where combExprs e1 e2 = PrimOp combOp [e1, e2]

  in CRA numStates numRegs transitions initF finalF

--
-- Produce result of whichever CRA currently matches
--
ifelse :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d -> CRA s d
ifelse l@(CRA _ _ _ initL finalL) r@(CRA _ _ _ initR finalR) =

  let (CRA numStates numRegs transitions _ _, (stateMapL, regMapL), (stateMapR, regMapR)) = combine l r

      -- States of init are the intersection of init state from left and right
      -- The update op at each state is the union of updates from left and right (should assign to disjoint registers)
      initF = M.intersectionWith M.union (translateInit stateMapL regMapL initL) (translateInit stateMapR regMapR initR)

      -- States of final are the symmetric difference of final states from left and right
      -- The expresion at each state is unchanged (modulo translating registers)
      finalF =
        let newL = translateFinal stateMapL regMapL finalL
            newR = translateFinal stateMapR regMapR finalR
        in (newL `M.union` newR) `M.difference` (newL `M.intersection` newR)

  in CRA numStates numRegs transitions initF finalF

--
-- Quantitative concatenation (note the primitive operation must be binary)
--
split :: CRA s d -> CRA s d -> Prim d -> CRA s d
split l@(CRA _ _ _ initL finalL) r@(CRA _ _ _ initR finalR) combOp =

  let (CRA numStates numRegs transitions _ _, (stateMapL, regMapL), (stateMapR, regMapR)) = combine l r

      -- Just translate left init function
      initF = translateInit stateMapL regMapL initL

      auxReg = numRegs + 1
      internalTrans = [makeInternalTrans from to | from <- M.toList finalL, to <- M.toList initR]
        where makeInternalTrans (q, exp) (t, op) =
                let op' = op & M.insert auxReg exp
                in EpsilonTransition q op' t

      transitions' = buildTransitionMap $ internalTrans ++ M.toList transitions

      -- Translate right final and add combOp looking up the previous result of left final
      finalF = translateFinal stateMapR regMapR finalR & M.map addCombOp
        where addCombOp e = PrimOp combOp [RegRead auxReg, e]

  in CRA numStates (numRegs + 1) transitions'  initF finalF

-------------------------------------
{-
As with non-determinism, adding epsilon transitions requires a non-trivial pass on the CRA code (currently incomplete)
Not that it's hard, just that it requries (non-trivial) changes in a number of places...

Seems like the best approach is to have separate epsilon transition map in the CRA data structure (keyed just by start state id).

May should also should make CRAs named records since we might need to continue tacking things on?
-}
-------------------------------------

--
-- Quantitative iteration (note the primitive operation must be binary)
--
iter :: CRA s d -> d -> Prim d -> CRA s d
iter = undefined
{-
Add register for keeping track of aggregate value.
Add an epsilon transition from unique final state to unique initial state.
  - this transition updates the aggregate register
Add new initial state which supplies the initial value of the aggregate register.
Add new final state which produces the aggregate register value.
...this might produce an ambiguous CRA so take the product with a (register free) DFA that accepts the right language (f*)
-}
