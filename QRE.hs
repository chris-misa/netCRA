{-
 - QRE combinators over CRAs
 -}

module QRE where

import Data.Hashable (Hashable)

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


-- Match a single symbol and emit a constant value when the symbol matches
atom :: (Hashable s, Eq s, Ord s) => s -> s -> d -> CRA s d
atom yesSym noSym dat =
  let noop = buildUpdateOp []
      initF = buildInitFunc [(1, noop)]
      finalF = buildFinalFunc [(2, exprConst dat)]
      transitions = [
          Transition 1 yesSym noop 2,
          Transition 1 noSym noop 1,
          Transition 2 yesSym noop 2,
          Transition 2 noSym noop 1
        ]
  in buildCRA 2 0 transitions initF finalF

-- Run list of CRAs in parallel and combine the outputs (when defined) using the given function
op :: Prim d -> [CRA s d] -> CRA s d
op = undefined
{-
State space: product of state spaces of both inputs
Registers: union of registers of both inputs
Initial function: union of initial functions of both inputs.
Final function: intersection of inputs final function, the apply op.
-}

-- Produce result of which ever CRA currently matches
ifelse :: CRA s d -> CRA s d -> CRA s d
ifelse = undefined
{-
State space: product of state spaces of both inputs
Registers: union of registers of both inputs
Initial function: union of initial functions of both inputs.
Final function: symmetric difference of both final functions.
-}

-- Quantitative concatenation (note the primitive operation must be binary)
split :: CRA s d -> CRA s d -> Prim d -> CRA s d
split = undefined
{-
State space: product of state spaces of both inputs
Registers: union of inputs
Initial function: initial function of first input
Final function: final function of second input
Add transition between final function of first input and initial function of second input
-}

-- Quantitative iteration (note the primitive operation must be binary)
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
