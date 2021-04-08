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

-}
module Compiler.BlockCleanup
    ( blockCleanup
    ) where


import Control.Monad.State

import MicroRAM
import Compiler.IRs
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Compiler.CompilationUnit
import Compiler.Errors

import Util.Util

-- | Basic jump threading.  If block A contains nothing but a jump to B, then
-- replace each reference to `Label "A"` with `Label "B"`.  This should
-- correctly handle both direct and indirect jumps to A.
threadJumps :: MAProgram md regT wrdT -> Hopefully (MAProgram md regT wrdT)
threadJumps prog = return $ map updateBlock $ filter (not . isJumpSource) $ prog
  where
    -- Map from old labels to new ones.  If block A contains only a jump to B,
    -- then we record (A, B) in this map.
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
    jumpMap' = Map.mapMaybe resolve jumpMap
      where
        resolve l = resolve' 1000 l

        resolve' :: Int -> String -> Maybe String
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
elimDead :: (Show regT, Show wrdT) => MAProgram md regT wrdT -> Hopefully (MAProgram md regT wrdT)
elimDead [] = return []
elimDead prog = return [b | (i, b) <- zip [0..] prog, Set.member i liveBlocks]
  where
    progSeq = Seq.fromList prog

    nameMap :: Map.Map String Int
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
    liveBlocks = execState (gather 0) mempty


blockCleanup :: (Show regT, Show wrdT) => (CompilationUnit mem (MAProgram md regT wrdT))
             -> Hopefully (CompilationUnit mem (MAProgram md regT wrdT))
blockCleanup cu = do
  prog' <- return (programCU cu) >>=
    threadJumps >>=
    --elimDead >>=
    return
  return $ cu { programCU = prog' }
