{-# LANGUAGE TypeOperators #-}
{-|
Module      : Register allocation
Description : RTL -> LTL
Maintainer  : 
Stability   : 


Description

-}
module Compiler.RegisterAlloc
    ( registerAlloc, --compileStraight
      trivialRegisterAlloc, -- FIXME : remove when reg alloc completed
    ) where

import qualified Data.Map as Map

import qualified MicroRAM.MicroRAM as MRAM

import Compiler.CompileErrors
import Compiler.IRs


registerAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
registerAlloc = undefined







-- * Triviall allocation: we provide a pass that erases the code. Usefull for early testing.
-- FIXME: remove this once registerAlloc is implemented and can be tested!

trivialRegisterAlloc :: Rprog () Word -> Hopefully $ Lprog () VReg Word
trivialRegisterAlloc = rtlToLtl . replacePhi




-- * TRIVIAL PHI REMOVIAL
-- ALL OF THIS SHOULD GO ONCE THE REAL REGISTER ALLOCATION IS READY
-- TODO: delete everything bellow this.

{- | Moves all `Phi`s back to the other block as Imov instruction
Example:
If you have `RPhi r2 (r1, block0)` we remove that instruction and add the following at the end of  `block0`: `Imove r2 r1`.

It works in two steps:
1.  Remove all the phi's and store the info of what register needs to move where, in what block.
2. Add, at the end of every block, the necessary move instructions calculated in the last step.

-}
-- | Map2 is a mpa that takes two keys and returns one value
type Map2 t1 t2 t3 = Map.Map t1 (Map.Map t2 t3) 

replacePhi :: Rprog () Word -> Rprog () Word
replacePhi prog = addPhiMoves $ abstractPhi prog

-- | removes all occurrences of Phi, and stores the move information in a map
-- (Name,Name) represents the name of the function and the name of the block.
type AbstractPhiProgram = (Rprog () Word, Map2 Name Name [(VReg, MRAM.MAOperand VReg Word)]) 
abstractPhi :: Rprog () Word -> AbstractPhiProgram 
abstractPhi (IRprog tenv globs code) =
  let (phiMap, code') = abstractPhiCode code in
    (IRprog tenv globs code', phiMap)

  where abstractPhiCode code = foldr abstractPhiFunc (Map.empty, []) code
 
        abstractPhiFunc (Function name ret args code) (phiMap, funcs) =
          let (phiMapF, code') = foldr abstractPhiBlock (Map.empty, [])  code in
            (Map.insert name phiMapF phiMap, Function name ret args code' : funcs)

        abstractPhiBlock (BB name code term dagd) (phiMap, blocks) =
          let (phiMapB, code') = abstractPhiBody code in
            (Map.unionWith (++) phiMapB phiMap, BB name code' term dagd : blocks)

        abstractPhiBody (IRI (RPhi vreg phiBacks) mdata: codeTl) =
          let (phiMapB, code') = abstractPhiBody codeTl in
            (Map.unionWith (++) (turnPhis vreg phiBacks) phiMapB, code') 
        abstractPhiBody code = (Map.empty,code)

        turnPhis vreg phiBacks = foldr (turnPhi vreg) Map.empty phiBacks
        turnPhi vreg (val, label) phiMap =
          Map.insertWith (++) label [(vreg,val)] phiMap 
        

          
-- | Adds all the abstract PHi information as `Imov` instructions
addPhiMoves :: AbstractPhiProgram -> Rprog () Word
addPhiMoves (prog, phiMap) = prog {code = map (addPhiFunc phiMap) $ code prog}
  where addPhiFunc phiMap (Function name ret args code) =
          Function name ret args (map (addPhiBlock (Map.lookup name phiMap)) code)

        addPhiBlock ::
          (Maybe $ Map.Map Name [(VReg, MRAM.MAOperand VReg Word)])
          -> BB $ RTLInstr () Word
          -> BB $ RTLInstr () Word
        addPhiBlock Nothing block = block
        addPhiBlock (Just phiMap) (BB name code term dagd) =
          (BB name (code ++ (phiMap2Instructions $ Map.lookup name phiMap)) term dagd)

        phiMap2Instructions ::
          Maybe [(VReg, MRAM.MAOperand VReg Word)]
          -> [RTLInstr () Word]
        phiMap2Instructions Nothing = []
        phiMap2Instructions (Just ls) = map phiPair2instr ls
        
        phiPair2instr (reg, op) = MRI (MRAM.Imov reg op) () 
        

