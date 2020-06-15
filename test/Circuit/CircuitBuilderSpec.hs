module CircuitBuilderSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


import Circuit.CircuitBuilder
import Circuit.CircuitIR


buildingBlockTests = testGroup "Testing the construction of circuit building blocks"
  [test1,test2,aluTest,newRegTest, newRegsTest]


-- This is defined again in MicroRAM.InterpreterSpec. Unify?
claimEqual :: (Eq a, Show a) => a -> a -> Either String String
claimEqual a b = 
   if a == b
     then Right "OK"
     else Left $  "Got " ++ show a ++ " but expected " ++ show b

-- | Generalest list of names based on a seed
lstNames :: String -> Int -> [String]
lstNames seed n = map (\n -> seed ++ show n) [0..(n-1)]



-- ** Test1 : muxFilter
-- test muxFiler C sig X out
muxFilterC :: Int -> TagCircuit Int String
muxFilterC c = TC ["sig", "X"] (evalTS $ muxFilter c "sig" "X" "out" )

test1 :: TestTree
test1 = testProperty "Testing muxFilter" $
  \c -> 
  \sig -> 
  \x ->
  evalCircuit (muxFilterC c) [sig,x] ["out"] == Ok [if sig == c then x else 0]



-- ** Test2 : multiplexer
-- test muxFiler C sig X out
inputNames :: Int -> [String]
inputNames = lstNames "input"

muxC :: Int -> TagCircuit Int String
muxC n = TC ("sig":inputs) (evalTS $ mux inputs "sig" "out" )
  where inputs = (inputNames n)
  
test2 :: TestTree
test2 = testProperty "Testing the multiplexer" $
  \inputs -> length inputs > 1 ==>
  \sig -> 0<= sig && sig < length inputs ==>
  claimEqual
    (evalCircuit (muxC (length inputs)) (sig:inputs) ["out"]) 
    (Ok [if length inputs < sig then 0 else inputs !! sig])


-- ** Test3 : Alu
-- test muxFiler C sig X out
posibleResults :: Int -> Int-> [Int]
posibleResults op1 op2 =
  [op2
  , op1 + op2
  , op1 - op2
  , op1 * op2
  , fromEnum (op1 == op2)
  , fromEnum (op1 > op2)
  ]

aluC :: TagCircuit Int String
aluC = TC ["op1", "op2", "inst"] (evalTS $ alu "op1" "op2" "inst" "out" )
  
aluTest :: TestTree
aluTest = testProperty "Testing the ALU" $
  \op1 -> 
  \op2 ->
    let results = posibleResults op1 op2 in
    \inst -> 0<= inst && inst < length results ==>
  claimEqual
    (evalCircuit aluC [op1, op2, inst] ["out"]) 
    (Ok [results !! inst])



-- ** Test4 : eqIf
-- tests the circuit that sets new registers
eqIfC :: Int -> TagCircuit Int String
eqIfC c = TC ["regN", "aluOut", "oldReg"] (evalTS $ eqIf c "regN" "aluOut" "oldReg" "newReg")

newRegTest :: TestTree
newRegTest = testProperty "Testing eqIf circuit" $
  \rn ->
  \regN -> 
  \aluOut -> 
  \oldReg ->
  evalCircuit (eqIfC rn) [regN,aluOut,oldReg] ["newReg"] == Ok [if rn == regN then aluOut else oldReg]



-- ** Test5 : new Registers
-- tests the circuit that sets new registers (all of them)
newNames = lstNames "newR"
oldNames = lstNames "oldR"

newRegsC :: Int -> TagCircuit Int String
newRegsC n = TC ("aluOut":"outReg":(oldNames n)) (evalTS $ newRegsCircuit (oldNames n) "aluOut" "outReg" (newNames n))

newRegsTestPure :: [Int] -> Int -> Int -> [Int]
newRegsTestPure [] _ _ = []
newRegsTestPure (_:ls) 0 a = (a:ls)
newRegsTestPure (hd:ls) n a = hd:(newRegsTestPure ls (n-1) a)


newRegsTest :: TestTree
newRegsTest = testProperty "Testing all new registers circuit" $
  \oldRegVals -> 
  \aluOut -> 
  \outReg -> 0 <= outReg ==>
  let n = length oldRegVals in
    let oldRegs = oldNames n in
      let newRegs = newNames n in
        claimEqual
        (evalCircuit (newRegsC n) (aluOut:outReg:oldRegVals) newRegs) $
        Ok (newRegsTestPure oldRegVals outReg aluOut)
