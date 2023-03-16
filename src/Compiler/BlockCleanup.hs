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
import Data.Either (rights)

import MicroRAM
import Compiler.Common
import Compiler.IRs
import Compiler.LazyConstants
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
    updateBlock blk = blk { blockInstrs = filter (not . isRedundant . fst) (blockInstrs blk) }

    isRedundant :: MAInstruction reg wrd -> Bool
    isRedundant (Imov r1 (AReg r2)) = r1 == r2
    isRedundant _                  = False


-- | Basic jump threading.  If block A contains nothing but a jump to B, then
-- replace each reference to `Label "A"` with `Label "B"`.  This should
-- correctly handle both direct and indirect jumps to A.
threadJumps :: (Show regT, Show wrdT) => MAProgram Metadata regT wrdT -> Hopefully (MAProgram Metadata regT wrdT)
threadJumps prog = return $ map (updateStart . updateBlock) $ filter (not . isJumpSource) prog
  where

    -- If removed block (src) was a starting block, mark the dest in jumpMap' as a starting block.
    updateStart blk
      | shouldStart (blockName blk)
      , i:instrs <- blockInstrs blk
      = blk { blockInstrs = setStartInst i : instrs }
    updateStart b                                           = b

    shouldStart (Just name) =
      let startingDests = Set.fromList $ map snd $ filter (\(src, _dest) ->
              -- Check if the src is a starting block.
              case Map.lookup src blockMap of
                Just (NamedBlock { blockInstrs = i:_ }) -> mdFunctionStart $ snd i
                _               -> False
            ) $ Map.toList jumpMap'
      in
      Set.member name startingDests
    shouldStart Nothing     = False

    setStartInst (i,md) = (i, md {mdFunctionStart = True})

    -- Map from names to blocks.
    blockMap = Map.fromList $ do
      blk <- prog
      Just name <- [blockName blk]
      return (name, blk)

    -- Map from old labels to new ones.  If block A contains only a jump to B,
    -- then we record (A, B) in this map.
    jumpMap :: Map.Map Name Name 
    jumpMap = Map.fromList $ do
      blk <- prog
      Just src <- [blockName blk]
      [(Ijmp (Label dest), md)] <- [blockInstrs blk]
      -- TODO: This temporarily disables thread jumping for blocks that start functions. This leads to errors if globals reference the function and the names aren't updated.
      guard $ not $ mdFunctionStart md

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

    isJumpSource blk | Just name <- blockName blk = Map.member name jumpMap'
    isJumpSource _ = False

    updateBlock blk = blk { blockInstrs = map (mapFst updateInstr) (blockInstrs blk) }

    updateInstr i = mapInstr id id updateOperand i

    updateOperand (Label l) | Just l' <- Map.lookup l jumpMap' = Label l'
    updateOperand x = x

-- | Eliminate any blocks that are not reachable from the first (entry point)
-- block. It follows global pointers, function pointers, jumps and fallthroughs.
elimDead :: (Show regT, Show wrdT) => [GlobalVariable wrdT] -> MAProgram Metadata regT wrdT -> Hopefully (MAProgram Metadata regT wrdT)
elimDead _ [] = return []
elimDead globals prog = return [b | (i, b) <- indexedProg, Set.member i liveBlocks]
  where
    globalMap = Map.fromList [(name, g) | g <- globals, (name, _, _) <- entryPoints g]
    progSeq = Seq.fromList prog

    reservedNames = Set.fromList [pcName]
    indexedProg = zip [0..] prog

    nameMap :: Map.Map Name Int
    nameMap = Map.fromList $ [(name, i) | (i, blk) <- indexedProg, Just name <- [blockName blk]]

    premainIndex = Map.findWithDefault (error "unreachable: block for premain not found") premainName nameMap

    -- `Either Name Int` stands for the union of global names and block indexes, respectively.
    nameDeps :: Either Name Int -> Set.Set (Either Name Int)
    nameDeps (Left n) = globalDeps n
    nameDeps (Right i) = blockDeps i


    globalDeps :: Name -> Set.Set (Either Name Int)
    globalDeps n | Just g <- Map.lookup n globalMap = case initializer g of
      Nothing -> mempty
      Just is -> Set.unions $ map lazyDeps is
    -- As a special case, reserved names have no dependencies
    globalDeps n | Set.member n reservedNames = mempty
    globalDeps n = error $ "no global for name " <> show n

    lazyDeps :: LazyConst a -> Set.Set (Either Name Int)
    lazyDeps (LConst _ ds) = Set.fromList [x | d <- Set.toList ds, Just x <- [nameToIndex d]]
    lazyDeps (SConst _)    = mempty

    -- | Convert a name to a dependency key.  `Left n` means the global named
    -- `n`, and `Right i` means the block starting at address `i`.  Returns
    -- `Nothing` if the name is not defined.
    nameToIndex :: Name -> Maybe (Either Name Int)
    nameToIndex n = case Map.lookup n nameMap of
      Just j -> Just $ Right j
      Nothing -> if Set.member n reservedNames || Map.member n globalMap then
          Just $ Left n
        else       
          Nothing

    blockDeps :: Int -> Set.Set (Either Name Int)
    -- If the last block falls through, consider the one-past-the-end block to
    -- have no dependencies.
    blockDeps i | i >= Seq.length progSeq = mempty
    blockDeps i = deps <> fallthroughDep
      where
        instrs = blockInstrs $ progSeq `Seq.index` i
        deps = mconcat $ map ((foldInstr (const mempty) (const mempty) opDep) . fst) instrs

        opDep (Label l) = case nameToIndex l of
          Just x -> Set.singleton x
          Nothing -> Set.empty
        opDep (LImm li) = lazyDeps li
        opDep (AReg _) = mempty

        fallthroughDep = case instrs of
          [] -> Set.singleton $ Right (i + 1)
          _:_ -> case fst $ last instrs of
            Ijmp _ -> mempty
            _ -> Set.singleton $ Right (i + 1)

    -- | Add `cur` and all blocks it references (transitively) to the state.
    gather :: Either Name Int -> State (Set.Set (Either Name Int)) ()
    gather cur = do
      seen <- get
      when (not $ Set.member cur seen) $ do
        modify $ Set.insert cur
        let new = nameDeps cur
        let new' = Set.difference new seen
        mapM_ gather new'

    -- | Set of all blocks referenced transitively from `start`.
    liveBlocks :: Set.Set Int
    liveBlocks = Set.fromList $ rights $ Set.toList $ execState (gather $ Right premainIndex) mempty


blockCleanup :: (Eq regT, Show regT, Show wrdT) => (CompilationUnit [GlobalVariable wrdT] (MAProgram Metadata regT wrdT))
             -> Hopefully (CompilationUnit [GlobalVariable wrdT] (MAProgram Metadata regT wrdT))
blockCleanup cu = do
  prog' <- return (pmProg $ programCU cu) >>=
    threadJumps >>=
    elimDead (intermediateInfo cu) >>=
    redundantMovs >>=
    return
  return $ cu { programCU = (programCU cu) { pmProg = prog' } }
