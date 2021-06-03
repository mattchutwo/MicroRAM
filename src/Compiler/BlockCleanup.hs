{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : BlockCleanup
Description : Cleanup passes for MAProgram blocks
Maintainer  : santiago@galois.com
Stability   : experimental

Currently implemented:

* Dead code elimination.  If a block is not the target of any jump, it is
  eliminated.  This cleans up entry blocks for undefined intrinsics, among
  other things.

* Basic jump threading.  If block A contains nothing but a jump to B, we
  replace each jump to A with a jump directly to B instead.

* Remove redundant moves (like `mov r1 r1`)

-}
module Compiler.BlockCleanup
    ( blockCleanup
    ) where


import Control.Monad.State

import MicroRAM
import Compiler.Common
import Compiler.IRs
import Compiler.Metadata
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Compiler.CompilationUnit
import Compiler.Errors

import Util.Util

-- | Removes redundant moves (like `mov r1 r1`).
redundantMovs :: forall md reg wrd . Eq reg => MAProgram md reg wrd -> Hopefully (MAProgram md reg wrd)
redundantMovs prog = return $ map updateBlock prog
  where
    updateBlock (NBlock name instrs) = NBlock name $ filter (not . isRedundant . fst) instrs

    isRedundant :: MAInstruction reg wrd -> Bool
    isRedundant (Imov r1 (AReg r2)) = r1 == r2
    isRedundant _                  = False


-- | Basic jump threading.  If block A contains nothing but a jump to B, then
-- replace each reference to `Label "A"` with `Label "B"`.  This should
-- correctly handle both direct and indirect jumps to A.
threadJumps :: MAProgram md regT wrdT -> Hopefully (MAProgram md regT wrdT)
threadJumps prog = return $ map updateBlock $ filter (not . isJumpSource) $ prog
  where
    -- Map from old labels to new ones.  If block A contains only a jump to B,
    -- then we record (A, B) in this map.
    jumpMap :: Map.Map Name Name 
    jumpMap = Map.fromList $ do
      NBlock (Just src) [(Ijmp (Label dest), _)] <- prog
      return (src, dest)

    -- Like jumpMap, but we resolve chains of jumps to a single destination.
    -- If jumpMap maps A to B and B to C, then jumpMap' maps A to C (and also
    -- maps B to C).
    --
    -- In case of cycles (empty infinite loops), we simply discard the entry:
    -- if jumpMap maps A to B and B to B, then jumpMap' contains entries for
    -- neither A nor B.  There are more effective solutions, but this one's
    -- easy and the situation should be quite rare anyway.
    jumpMap' :: Map.Map Name Name
    jumpMap' = Map.mapMaybe resolve jumpMap
      where
        resolve :: Name -> Maybe Name
        resolve l = resolve' 1000 l

        resolve' :: Int -> Name -> Maybe Name
        resolve' 0 _ = Nothing
        resolve' n l
          | Just l' <- Map.lookup l jumpMap = resolve' (n - 1) l'
          | otherwise = Just l

    isJumpSource (NBlock (Just name)_) = Map.member name jumpMap'
    isJumpSource _ = False

    updateBlock (NBlock name instrs) = NBlock name $ map (mapFst updateInstr) instrs

    updateInstr i = mapInstr id id updateOperand i

    updateOperand (Label l) | Just l' <- Map.lookup l jumpMap' = Label l'
    updateOperand x = x

-- | Eliminate any blocks that are not reachable from the first (entry point)
-- block.
elimDead :: (Show regT, Show wrdT) => MAProgram Metadata regT wrdT -> Hopefully (MAProgram Metadata regT wrdT)
elimDead [] = return []
elimDead prog = return [b | (i, b) <- zip [0..] prog, Set.member i liveBlocks]
  where
    progSeq = Seq.fromList prog

    nameMap :: Map.Map Name Int
    nameMap = Map.fromList [(name, i) | (i, NBlock (Just name) _) <- zip [0..] prog]

    blockDeps :: Int -> Set.Set Int
    -- If the last block falls through, consider the one-past-the-end block to
    -- have no dependencies.
    blockDeps i | i >= Seq.length progSeq = mempty
    blockDeps i = deps <> fallthroughDep
      where
        NBlock _ instrs = progSeq `Seq.index` i
        deps = mconcat $ map ((foldInstr (const mempty) (const mempty) labelSet) . fst) instrs

        labelSet (Label l) = case Map.lookup l nameMap of
          Nothing -> error $ "no definition of label " ++ show l
          Just j -> Set.singleton j
        labelSet _ = mempty

        fallthroughDep = case instrs of
          [] -> mempty
          _:_ -> case fst $ last instrs of
            Ijmp _ -> mempty
            _ -> Set.singleton (i + 1)

    -- | All the blocks that are at the beggining of a function.
    -- TODO: this is just an approximation of all blocks reachable from globals.
    -- If we could extract the function calls from the globals, we can use that instead.
    functionEntries :: [Int]
    functionEntries = do
      (i, NBlock _ instrs) <- zip [0..] prog
      guard $ not $ null instrs                     -- Block is not empty
      guard $ mdFunctionStart . snd $ (instrs !! 0) -- It's a function start
      return i
  
    -- | Add `cur` and all blocks it references (transitively) to the state.
    gather :: Int -> State (Set.Set Int) ()
    gather cur = do
      seen <- get
      when (not $ Set.member cur seen) $ do
        modify $ Set.insert cur
        let new = blockDeps cur
        let new' = Set.difference new seen
        mapM_ gather new'

    -- | Set of all blocks referenced transitively from `start`.
    liveBlocks :: Set.Set Int
    liveBlocks = execState (mapM_ gather $ 0:functionEntries) mempty


blockCleanup :: (Eq regT, Show regT, Show wrdT) => (CompilationUnit mem (MAProgram Metadata regT wrdT))
             -> Hopefully (CompilationUnit mem (MAProgram Metadata regT wrdT))
blockCleanup cu = do
  prog' <- return (pmProg $ programCU cu) >>=
    threadJumps >>=
    elimDead >>=
    redundantMovs >>=
    return
  return $ cu { programCU = (programCU cu) { pmProg = prog' } }
