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
      addA = buildUpdateOp [exprBinOp (+) (RegRead 1) CurVal, RegRead 2]
      addB = buildUpdateOp [RegRead 1, exprBinOp (+) (RegRead 2) CurVal]
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

--
-- Simple test of epsilon transitions
-- Accepts (a, x), {(a, _), (b, _), (c, _)} and produces x + 10, x + 100, or x + 50 depending on the second symbol
--
testETrans1 :: CRA Char Int
testETrans1 =
  let initOp = buildUpdateOp [exprConst 0]
      loadCurVal = buildUpdateOp [exprUOp id CurVal]
      addX x = buildUpdateOp [exprUOp (+ x) (RegRead 1)]
      noop = buildUpdateOp [exprUOp id (RegRead 1)]
      initF = buildInitFunc [(1, initOp)]
      finalF = buildFinalFunc [(6, RegRead 1)]
      transitions = [
          Transition 1 'a' loadCurVal 2,
          Transition 3 'a' noop 6,
          Transition 4 'b' noop 6,
          Transition 5 'c' noop 6
        ]
      etransitions = [
          ETransition 2 (addX 10) 3,
          ETransition 2 (addX 100) 4,
          ETransition 2 (addX 50) 5
        ]
  in buildCRA 6 1 transitions etransitions initF finalF

--
-- Example 4 from Alur et al., 2019.
-- Same as testCRA1, except expressed using epsilon transitions and non-determinism
--
testETrans2 :: CRA Char Int
testETrans2 =
  let initOp = buildUpdateOp [exprConst 0]
      addCurVal = buildUpdateOp [exprBinOp (+) (RegRead 1) CurVal]
      noop = buildUpdateOp [exprUOp id (RegRead 1)]
      initF = buildInitFunc [(1, initOp)]
      finalF = buildFinalFunc [(4, RegRead 1), (5, RegRead 1)]
      transitions = [
          Transition 2 'a' addCurVal 2,
          Transition 2 'a' addCurVal 4,
          Transition 2 'b' noop 2,
          Transition 3 'b' addCurVal 3,
          Transition 3 'b' addCurVal 5,
          Transition 3 'a' noop 3
        ]
      etransitions = [
          ETransition 1 initOp 2,
          ETransition 1 initOp 3
        ]
  in buildCRA 5 1 transitions etransitions initF finalF

--
-- Simple tests of atom with a single higher-order QRE combinator
--

--
-- Test `op` QRE combinators
-- Matches the prefix containing a single 'a' and adds 10.
--
testQRE1 :: CRA Char Int
testQRE1 = op (atom 'a' CurVal) (atom 'a' (exprConst 10)) (primBinary (+))

--
-- Test `ifelse` QRE combinator
-- Matches the prefix 'a' | 'b', adds 1 if the symbols is 'a', adds 10 if the symbol is 'b'
--
testQRE2 :: CRA Char Int
testQRE2 = ifelse (atom 'a' (exprUOp (+1) CurVal)) (atom 'b' (exprUOp (+10) CurVal))

--
-- Test `split` QRE combinator
-- Matches the prefix 'a' 'a' 'b' and returns the max value of all matches elements.
--
testQRE3 :: CRA Char Int
testQRE3 =
  let primMax = primBinary max
  in split (atom 'a' CurVal) (split (atom 'a' CurVal) (atom 'b' CurVal) primMax) primMax

--
-- Test the `iter` combinator
-- Matches the prefix 'a'* and sums all values
--
testQRE4 :: CRA Char Int
testQRE4 = iter (atom 'a' CurVal) 0 (primBinary (+))


--
-- Combined tests with multiple QRE combinators
--

--
-- Matches (ab*c)* and returns the difference between each a, c pair
--
testQRE5 :: CRA Char Int
testQRE5 =
  iter
    (split
      (atom 'a' CurVal)
      (split
        (iter (atom 'b' CurVal) 0 (primBinary (+)))
        (atom 'c' CurVal)
        (primBinary (curry snd)))
      (primBinary (\a c -> c - a)))
    0
    (primBinary (curry snd))

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
