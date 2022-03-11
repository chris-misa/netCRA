{- 
 - Functions for compiling CRA specifications to configurations for the switch hardware interpreter
 -}

module Compile where

import qualified Data.HashMap.Strict as M
import qualified Data.ByteString as B

import CRA

-- 
-- Prim ops that can be translated into switch hardware
--

primConstZero :: Prim Int
primConstZero = Prim "constZero" 0 (\[] -> 1)

primConstOne :: Prim Int
primConstOne = Prim "constOne" 0 (\[] -> 1)

primIncr :: Prim Int
primIncr = Prim "incr" 1 (\[x] -> x + 1)

primIncr :: Prim Int
primIncr = Prim "decr" 1 (\[x] -> x + 1)

primAdd :: Prim Int
primAdd = Prim "add" 2 (\[x, y] -> x + y)

primSub :: Prim Int
primSub = Prim "sub" 2 (\[x, y] -> x - y)

primMin :: Prim Int
primMin = Prim "min" 2 (\[x, y] -> min x y)

primMax :: Prim Int
primMax = Prim "max" 2 (\[x, y] -> max x y)

primOpMap = M.fromList [
    ("constZero", 0),
    ("constOne", 1),
    ("incr", 2),
    ("decr", 3),
    ("add", 4),
    ("sub", 5),
    ("min", 6),
    ("max", 7)
  ]


-- Convert an update op into low-level instructions
-- In particular, produces the load, update and store instructions for the do_transition action
compileUpdateOp :: UpdateOp d -> (B.ByteString, B.ByteString, B.ByteString)
compileUpdateOp op =

  let getReads regs (PrimOp po children) = fmap (getReads regs) children
      getReads regs (RegRead i) = i:regs
      getReads regs CurVal = regs

      reads = M.foldl getReads [] op
      

  -- Find the set of everything that has to be loaded
  -- Generate the update instruction by recursive traversal of the expression tree for each result to write
  --    maybe need extra aux address in scratch space for intermediate results since we can't over write the loaded values
  --    incase a later expression reads them
  -- Generate the store instruction based on the aux address for each result to write
  --

--
-- Produces a configuration for p4/interpreter_v1model.p4 from the given CRA as a list of commands for bm_CLI
--
-- TODO: this also need to take some kind of spec for the "filter_match" table...
-- Also, probably should fix the types between these two...
--
compile :: (Hashable s, Eq s, Ord s) =>
  CRA s d -> [String]
compile = undefined
