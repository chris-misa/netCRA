module Main where

import Data.Function ((&))

import CRA
import QRE

--
-- Example 3 from Alur et al., "Streamable Regular Transductions", Elsevier, 2019.
-- Counts the number of 'a's and 'b's emitting the count of the most recent tag.
--
testCRA1 :: CRA Char Int
testCRA1 =
  let initOp = buildUpdateOp [exprConst 0, exprConst 0]
      addA = buildUpdateOp [exprUOp (+1) (RegRead 1), RegRead 2]
      addB = buildUpdateOp [RegRead 1, exprUOp (+1) (RegRead 2)]
      initF = buildInitFunc [(1, initOp)]
      finalF = buildFinalFunc [(2, RegRead 1), (3, RegRead 2)]
      transitions = [
          Transition 1 'a' addA 2,
          Transition 1 'b' addB 3,
          Transition 2 'a' addA 2,
          Transition 2 'b' addB 3,
          Transition 3 'b' addB 3,
          Transition 3 'a' addA 2
        ]
  in buildCRA 3 2 transitions [] initF finalF

--
-- Example 5 from Alur et al., 2019.
-- Computes a sum of all 'a'-tagged elements between '#' tags 
-- and produces the maximum sum computed so far on each '#' tag.
--
testCRA2 :: CRA Char Int
testCRA2 =
  let initOp = buildUpdateOp [exprConst 0, exprConst 0]
      addVal = buildUpdateOp [exprBinOp (+) (RegRead 1) CurVal, RegRead 2]
      maxVal = buildUpdateOp [exprConst 0, exprBinOp max (RegRead 1) (RegRead 2)]
      initF = buildInitFunc [(1, initOp)]
      finalF = buildFinalFunc [(1, RegRead 2)]
      transitions = [
          Transition 1 'a' addVal 2,
          Transition 2 'a' addVal 2,
          Transition 2 '#' maxVal 1
        ]
  in buildCRA 2 2 transitions [] initF finalF

--
-- Example 6 from Alur et al., 2019.
-- Computes the maximum drawdown since the last 'b'-tagged element.
--
testCRA3 :: CRA Char Int
testCRA3 =
  let initOp = buildUpdateOp [exprConst 0, exprConst 0]
      theta = buildUpdateOp [exprBinOp max (RegRead 1) CurVal, exprBinOp max (RegRead 2) (exprBinOp (-) (exprBinOp max (RegRead 1) CurVal) CurVal)]
      initF = buildInitFunc [(1, initOp)]
      finalF = buildFinalFunc [(2, RegRead 2)]
      transitions = [
          Transition 1 'a' initOp 1,
          Transition 1 'b' initOp 1,
          Transition 1 'b' initOp 2,
          Transition 2 'a' theta 2
        ]
  in buildCRA 2 2 transitions [] initF finalF

--
-- Simple tests of termination
--
termOnB :: CRA Char Int
termOnB =
  let noop = buildUpdateOp []
      initF = buildInitFunc [(1, noop)]
      finalF = buildFinalFunc [(2, CurVal)]
      transitions = [
          Transition 1 'a' noop 1,
          Transition 1 'c' noop 1,
          Transition 1 'b' noop 2
        ]
  in buildCRA 2 0 transitions [] initF finalF

termOnC :: CRA Char Int
termOnC =
  let noop = buildUpdateOp []
      initF = buildInitFunc [(1, noop)]
      finalF = buildFinalFunc [(2, CurVal)]
      transitions = [
          Transition 1 'a' noop 1,
          Transition 1 'b' noop 1,
          Transition 1 'c' noop 2
        ]
  in buildCRA 2 0 transitions [] initF finalF


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
