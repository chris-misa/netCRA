module Main where

import CRA

--
-- Example 3 from Alur, et al., "Streamable Regular Transductions", Elsevier, 2019
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
  in buildCRA transitions initF finalF

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
  in buildCRA transitions initF finalF


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
