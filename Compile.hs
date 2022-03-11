{- 
 - Functions for compiling CRA specifications to configurations for the switch hardware interpreter
 -}

module Compile where

import Data.Function ((&))
import Data.Word
import Numeric (showHex)
import qualified Data.List as L
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

primDecr :: Prim Int
primDecr = Prim "decr" 1 (\[x] -> x + 1)

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

showByteString :: String -> B.ByteString -> String
showByteString inter = ("0x"++) . (concatMap (\x -> if x < 16 then "0" ++ showHex x inter else showHex x inter)) . B.unpack

unique = map head . L.group . L.sort

data UpdateIns = UpdateIns B.ByteString B.ByteString B.ByteString

instance Show UpdateIns where
  show (UpdateIns load update store) =
    "load: " ++ showByteString " " load ++ "\n" ++
    "update: " ++ showByteString " " update ++ "\n" ++
    "store: " ++ showByteString " " store ++ "\n"

--
-- Convert an update op into low-level instructions
-- In particular, produces the load, update and store instructions for the do_transition action
--
compileUpdateOp :: UpdateOp d -> UpdateIns
compileUpdateOp op =

  let getReads regs (PrimOp po children) = concatMap (getReads regs) children ++ regs
      getReads regs (RegRead i) = i:regs
      getReads regs CurVal = regs

      reads = M.foldl getReads [] op & unique
      loadMap = (reads `zip` ([1..] :: [Int])) & M.fromList
      loadOne ins reg addr = fromIntegral addr `B.cons` fromIntegral reg `B.cons` ins
      loads = M.foldlWithKey' loadOne B.empty loadMap

      initStoreMap = M.keys op & fmap (\reg -> (reg, M.lookupDefault 0 reg loadMap)) & M.fromList

      -- getUpdate returns a sequence of instructions that only use addresses >= tos and writes the result at tos
      getUpdate tos (PrimOp (Prim opId _ _) children) =
        let ((insL, addrL), (insR, addrR)) =
              case children of
                [l, r] -> (getUpdate (tos + 1) l, getUpdate tos r)
                [r] -> ((B.empty, 0), getUpdate tos r)
                [] -> ((B.empty, 0), (B.empty, 0))
                _ -> error "Bad number of children (we only support zero, one, or two for now)"
            opCode = primOpMap M.! opId
            ins = B.pack [fromIntegral tos, fromIntegral addrR, fromIntegral addrL, fromIntegral opCode] `B.append` insL `B.append` insR
        in (ins, tos)
      getUpdate tos (RegRead reg) = (B.empty, loadMap M.! reg)
      getUpdate tos CurVal = (B.empty, 0)

      updateOne (ins, tos, storeMap) reg expr =
        let (ins', addr) = getUpdate tos expr
            tos' = if addr == tos then tos + 1 else tos -- only incr tos if it was used to store the result
            storeMap' = M.adjust (\_ -> addr) reg storeMap
        in (ins' `B.append` ins, tos', storeMap')
      (updates, _, storeMap) = M.foldlWithKey' updateOne (B.empty, length reads + 1, initStoreMap) op

      storeOne ins addr reg = fromIntegral reg `B.cons` fromIntegral addr `B.cons` ins
      stores = M.foldlWithKey' storeOne B.empty storeMap


  in UpdateIns loads updates stores

--
-- Produces a configuration for p4/interpreter_v1model.p4 from the given CRA as a list of commands for bm_CLI
--
-- TODO: this also need to take some kind of spec for the "filter_match" table...
-- Also, probably should fix the types between these two...
--
-- compile :: (Hashable s, Eq s, Ord s) =>
--   CRA s d -> [String]
-- compile = undefined
