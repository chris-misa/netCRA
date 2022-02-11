{-
 - QRE combinators over CRAs
 -}

module QRE where

import CRA

-- We need at least the following:

-- Match a single symbol and emit a constant value when the symbol matches
atom :: s -> d -> CRA s d

-- Run list of CRAs in parallel and combine the outputs (when defined) using the given function
op :: Prim d -> [CRA s d] -> CRA s d

-- Produce result of which ever CRA currently matches
ifelse :: CRA s d -> CRA s d -> CRA s d

-- Quantitative concatenation (note the primitive operaetion must be binary)
split :: CRA s d -> CRA s d -> Prim d -> CRA s d

-- Quantitative iteration (note the primitive operaetion must be binary)
iter :: CRA s d -> d -> Prim d -> CRA s d
