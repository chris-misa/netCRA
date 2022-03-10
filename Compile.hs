{- 
 - Functions for compiling CRA specifications to configurations for the switch hardware interpreter
 -}

module Compile where

import qualified Data.ByteString as B

import CRA

-- Need somewhere (perhaps here) a list of available fields and primitive operations that can be mapped into hardware...

-- Convert an update op into low-level instructions
-- In particular, produces the load, update and store instructions for the do_transition action
compileUpdateOp :: UpdateOp d -> (B.ByteString, B.ByteString, B.ByteString)
compileUpdateOp op = undefined
  -- Find the set of all registers read for the load command
  -- Generate the update instruction by recursive traversal of the expression tree for each result to write
  --    maybe need extra aux address in scratch space for intermediate results since we can't over write the loaded values
  --    incase a later expression reads them
  -- Generate the store instruction based on the aux address for each result to write
  --

--
-- Produces a configuration for p4/interpreter_v1model.p4 from the given CRA as a list of commands for bm_CLI
--
-- TODO: this also need to take some kind of spec for the "filter_match" table...
--
compile :: (Hashable s, Eq s, Ord s) =>
  CRA s d -> [String]
compile = undefined
