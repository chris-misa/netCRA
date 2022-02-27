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
-- Match a single symbol (at the beginning of the stream) and evaluate expr when the symbol matches
-- Note that `expr` should not have any register reads (enforce this later...)
--
atom :: (Hashable s, Eq s, Ord s) => s -> Expression d -> CRA s d
atom sym expr =
  let noop = buildUpdateOp []
      updateReg = buildUpdateOp [expr]
      initF = buildInitFunc [(1, noop)]
      finalF = buildFinalFunc [(2, RegRead 1)]
      transitions = [
          Transition 1 sym updateReg 2
        ]
  in buildCRA 2 1 transitions [] initF finalF


--
-- Run two CRAs in parallel and combine the outputs (when defined) using the given function
--
op :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d -> Prim d -> CRA s d
op l@(CRA _ _ _ _ initL finalL) r@(CRA _ _ _ _ initR finalR) combOp =

  let (CRA numStates numRegs transitions eTransitions _ _, (stateMapL, regMapL), (stateMapR, regMapR)) = combine l r

      -- States of init are the intersection of init state from left and right
      -- The update op at each state is the union of updates from left and right (should assign to disjoint registers)
      initF = M.intersectionWith M.union (translateInit stateMapL regMapL initL) (translateInit stateMapR regMapR initR)

      -- States of final are the intersection of final states from left and right
      -- The expresion at each state combines left and right expressions using combOp
      finalF = M.intersectionWith combExprs (translateFinal stateMapL regMapL finalL) (translateFinal stateMapR regMapR finalR)
        where combExprs e1 e2 = PrimOp combOp [e1, e2]

  in CRA numStates numRegs transitions eTransitions initF finalF

--
-- Produce result of whichever CRA currently matches
--
ifelse :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d -> CRA s d
ifelse l@(CRA _ _ _ _ initL finalL) r@(CRA _ _ _ _ initR finalR) =

  let (CRA numStates numRegs transitions eTransitions _ _, (stateMapL, regMapL), (stateMapR, regMapR)) = combine l r

      -- States of init are the intersection of init state from left and right
      -- The update op at each state is the union of updates from left and right (should assign to disjoint registers)
      initF = M.intersectionWith M.union (translateInit stateMapL regMapL initL) (translateInit stateMapR regMapR initR)

      -- States of final are the symmetric difference of final states from left and right
      -- The expresion at each state is unchanged (modulo translating registers)
      finalF =
        let newL = translateFinal stateMapL regMapL finalL
            newR = translateFinal stateMapR regMapR finalR
        in (newL `M.union` newR) `M.difference` (newL `M.intersection` newR)

  in CRA numStates numRegs transitions eTransitions initF finalF

--
-- Quantitative concatenation (note the primitive operation must be binary)
--
split :: (Hashable s, Eq s, Ord s) => CRA s d -> CRA s d -> Prim d -> CRA s d
split l@(CRA numStatesL numRegsL transitionsL eTransitionsL initL finalL) r@(CRA numStatesR numRegsR transitionsR eTransitionsR initR finalR) combOp =

  let numStates = numStatesL + numStatesR
      stateMapL = [(i, [i]) | i <- [1..numStatesL]] & idMapFromList
      stateMapR = [(i, [i + numStatesL]) | i <- [1..numStatesR]] & idMapFromList

      numRegs = numRegsL + numRegsR + 1
      regMapL = [(i, [i]) | i <- [1..numRegsL]] & idMapFromList
      regMapR = [(i, [i + numRegsL]) | i <- [1..numRegsR]] & idMapFromList

      auxReg = numRegs

      -- Directly translate left initial
      initF = translateInit stateMapL regMapL initL

      --
      -- Directly translate state and register ids for transitions
      --

      updateTransStatesRegs stateMap regMap (Transition q s update t) =
        let [q'] = stateMap M.! q
            update' = translateUpdateOp regMap update
            [t'] = stateMap M.! t
        in Transition q' s update' t'

      transitions =
        let lTrans = transitionsL & M.toList & concatMap snd & fmap (updateTransStatesRegs stateMapL regMapL)
            rTrans = transitionsR & M.toList & concatMap snd & fmap (updateTransStatesRegs stateMapR regMapR)
        in (lTrans ++ rTrans) & buildTransitionMap
          
      --
      -- Directly translate state and register ids for epsilon transitions
      --

      updateETransStatesRegs stateMap regMap (ETransition q update t) =
        let [q'] = stateMap M.! q
            update' = translateUpdateOp regMap update
            [t'] = stateMap M.! t
        in ETransition q' update' t'

      eTransitions =
        let lETrans = eTransitionsL & M.toList & concatMap snd & fmap (updateETransStatesRegs stateMapL regMapL)
            rETrans = eTransitionsR & M.toList & concatMap snd & fmap (updateETransStatesRegs stateMapR regMapR)
            internalETrans = 
              [makeInternalTrans from to | from <- M.toList finalL, to <- M.toList initR]
        in (lETrans ++ rETrans ++ internalETrans) & buildETransitionMap

        where makeInternalTrans (q, exp) (t, op) =
                let [q'] = stateMapL M.! q
                    op' = (translateUpdateOp regMapR op) & M.insert auxReg (translateExpr regMapL exp)
                    [t'] = stateMapR M.! t
                in ETransition q' op' t'

      -- Translate right final and add combOp looking up the previous result of left final
      finalF = translateFinal stateMapR regMapR finalR & M.map addCombOp
        where addCombOp e = PrimOp combOp [RegRead auxReg, e]

  in CRA numStates (numRegs + 1) transitions eTransitions initF finalF

--
-- Quantitative iteration (note the primitive operation must be binary)
--
iter :: (Hashable s, Eq s, Ord s) => CRA s d -> d -> Prim d -> CRA s d
iter (CRA numStates numRegs transitions eTransitions init final) startVal op =
  
  let numStates' = numStates + 2
      newInitState = numStates + 1
      newFinalState = numStates + 2

      numRegs' = numRegs + 1
      auxReg = numRegs'

      init' =
        let initOp = M.singleton auxReg (exprConst startVal)
        in M.singleton newInitState initOp

      eTransitions' =
        let noop = buildUpdateOp []
            updateAuxReg expr = M.singleton auxReg (PrimOp op [RegRead auxReg, expr])

            initETrans = M.keys init & fmap (\t -> ETransition newInitState noop t)
            finalETrans = M.toList final & fmap (\(q, e) -> ETransition q (updateAuxReg e) newFinalState)
      
            internalETrans = M.keys init & fmap (\t -> ETransition newFinalState noop t)
        in (initETrans ++ finalETrans ++ internalETrans) & buildETransitionMap

      final' = M.singleton newFinalState (RegRead auxReg)
  
  in CRA numStates' numRegs' transitions eTransitions' init' final'
      
  
{-
Add register for keeping track of aggregate value.
Add an epsilon transition from unique final state to unique initial state.
  - this transition updates the aggregate register
Add new initial state which supplies the initial value of the aggregate register.
Add new final state which produces the aggregate register value.
...this might produce an ambiguous CRA so take the product with a (register free) DFA that accepts the right language (f*)

What does it mean for a CRA to be ambiguous?

Unambiguous means for each trace either (i) the trace does not end in a final state, or (ii) the trace ends in exactly one final state.

-}
