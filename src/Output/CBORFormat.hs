{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : CBOR Format
Description : Format the output of the compiler and interpreter as CBOR
              to communicate down with the Circuit generator.
Maintainer  : santiago@galois.com
Stability   : experimental

Format for compiler units

-}

module Output.CBORFormat where
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm, FlatTerm)
import qualified Data.Map as Map
import GHC.Generics

import Codec.Serialise

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Write
import Codec.CBOR.Read
import Codec.CBOR.Pretty
import qualified Data.ByteString.Lazy                  as L

import Compiler.Sparsity
import Compiler.CompilationUnit
import Compiler.Registers
import Compiler.Tainted
import Compiler.IRs

import MicroRAM.MRAMInterpreter
import MicroRAM

-- Get version number
import Paths_MicroRAM (version)
import Data.Version (Version(..))

import qualified Data.Text as TXT 


import Output.Output


-- * Full Output

encodeOutput :: Serialise reg => Output reg -> Encoding
encodeOutput (SecretOutput prog params initM trc adv) =
  map2CBOR $ 
  [ ("program", encode prog)
  , ("params", encode params)
  , ("init_mem", encode initM)
  , ("trace", encode trc)
  , ("advice", encode adv)
  ]
encodeOutput (PublicOutput prog params initM) =
  map2CBOR $ 
  [ ("program", encode prog)
  , ("params", encode params)
  , ("init_mem", encode initM)
  ]

decodeOutput :: Serialise reg => Decoder s (Output reg)
decodeOutput = do
  len <- decodeMapLen
  case len of
    5 -> SecretOutput <$> tagDecode <*> tagDecode <*> tagDecode <*> tagDecode <*> tagDecode
    3 -> PublicOutput <$> tagDecode <*> tagDecode  <*> tagDecode
    n -> fail $ "Only lengths for output are 3 and 5 (Public and Secret). Insted found: " ++ show n 
    
instance Serialise reg => Serialise (Output reg) where 
    encode = encodeOutput
    decode = decodeOutput

-- * Utils

deriving instance Generic Int
deriving instance Generic Word

lengthW :: Foldable t => t a -> Word
lengthW = fromIntegral . length

list2CBOR :: [Encoding] -> Encoding
list2CBOR ls = foldr (<>) mempty (encodeListLen (lengthW ls) : ls )

tagDecode :: Serialise t => Decoder s t
tagDecode = decodeString *> decode

map2CBOR :: [(TXT.Text, Encoding)] -> Encoding
map2CBOR ls =
     foldr (<>) mempty $ (encodeMapLen len) : map encodeField ls
    where len = lengthW ls

          encodeField :: (TXT.Text, Encoding) -> Encoding
          encodeField (str, enc) = encodeString str <> enc
  


-- * Public Output
-- Public output is generated "statically" (without knowing the input). It can be obtained
-- by the verifier and the prover and has the following elements:
-- 1. Program
-- 2. Parameters
--    * Number of registers
--    * Trace length
--    * Sparcity


-- ** Program 

-- It is enough to show an instance of `Serialise (Instruction r w)` to get an instance for
-- full programs (given the instance `Serialise a => Serialise [a]`).



encodeOperand' :: (Serialise regT, Serialise wrdT) => Operand regT wrdT -> [Encoding]
encodeOperand' (Reg r) =  [encodeBool False , encode r] 
encodeOperand' (Const c) =  [encodeBool True, encode c] 

encodeOperand :: (Serialise regT, Serialise wrdT) => Operand regT wrdT -> Encoding
encodeOperand  = list2CBOR . encodeOperand'

decodeOperand' :: (Serialise regT, Serialise wrdT) => Decoder s (Operand regT wrdT)
decodeOperand' = do
  kind <- decodeBool
  case kind of
    False -> Reg <$> decode
    True  -> Const <$> decode

decodeOperand :: (Serialise regT, Serialise wrdT) => Decoder s (Operand regT wrdT)
decodeOperand = do
  _ <- decodeListLen
  kind <- decodeBool
  case kind of
    False -> Reg <$> decode
    True  -> Const <$> decode

instance (Serialise regT, Serialise wrdT) => Serialise (Operand regT wrdT) where
    encode = encodeOperand
    decode = decodeOperand


encodeInstr :: forall regT wrdT. (Serialise regT, Serialise wrdT) =>
  Instruction regT wrdT -> Encoding
encodeInstr (Iand r1 r2 operand  ) = list2CBOR $ encodeString "and"    : encode r1  : encode r2  : (encodeOperand' operand)
encodeInstr (Ior r1 r2 operand   ) = list2CBOR $ encodeString "or"     : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ixor r1 r2 operand  ) = list2CBOR $ encodeString "xor"    : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Inot r1 operand     ) = list2CBOR $ encodeString "not"    : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Iadd r1 r2 operand  ) = list2CBOR $ encodeString "add"    : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Isub r1 r2 operand  ) = list2CBOR $ encodeString "sub"    : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Imull r1 r2 operand ) = list2CBOR $ encodeString "mull"   : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iumulh r1 r2 operand) = list2CBOR $ encodeString "umulh"  : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ismulh r1 r2 operand) = list2CBOR $ encodeString "smulh"  : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iudiv r1 r2 operand ) = list2CBOR $ encodeString "udiv"   : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iumod r1 r2 operand ) = list2CBOR $ encodeString "umod"   : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ishl r1 r2 operand  ) = list2CBOR $ encodeString "shl"    : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ishr r1 r2 operand  ) = list2CBOR $ encodeString "shr"    : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpe r2 operand    ) = list2CBOR $ encodeString "cmpe"   : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpa r2 operand    ) = list2CBOR $ encodeString "cmpa"   : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpae r2 operand   ) = list2CBOR $ encodeString "cmpae"  : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpg r2 operand    ) = list2CBOR $ encodeString "cmpg"   : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpge r2 operand   ) = list2CBOR $ encodeString "cmpge"  : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Imov r1 operand     ) = list2CBOR $ encodeString "mov"    : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Icmov r1 operand    ) = list2CBOR $ encodeString "cmov"   : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Ijmp operand        ) = list2CBOR $ encodeString "jmp"    : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Icjmp operand       ) = list2CBOR $ encodeString "cjmp"   : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Icnjmp operand      ) = list2CBOR $ encodeString "cnjmp"  : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Istore operand r2   ) = list2CBOR $ encodeString "store"  : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Iload r1 operand    ) = list2CBOR $ encodeString "load"   : encode r1  : encodeNull : (encodeOperand' operand)
encodeInstr (Iread r1 operand    ) = list2CBOR $ encodeString "read"   : encode r1  : encodeNull : (encodeOperand' operand)
encodeInstr (Ianswer operand     ) = list2CBOR $ encodeString "answer" : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Ipoison operand r2  ) = list2CBOR $ encodeString "poison" : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Iadvise r1          ) = list2CBOR $ encodeString "advise" : encode r1  : encodeNull : [encode False, encodeNull]
encodeInstr (Itaint r2 operand   ) = list2CBOR $ encodeString "taint"  : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Isink r2 operand    ) = list2CBOR $ encodeString "sink"   : encodeNull : encode r2  : (encodeOperand' operand) 
-- `Iext` and `Iextval` should have been compiled away by a previous pass, but
-- it's sometimes useful for debugging to include them in the output CBOR.  The
-- witness checker generator doesn't support these instructions at all, so how
-- we encode the operands doesn't really matter - it's only for human
-- consumption.
encodeInstr (Iext name ops       ) = list2CBOR $ encodeString "ext" : encodeString name : concatMap encodeOperand' ops
encodeInstr (Iextval name rd ops ) = list2CBOR $ encodeString "ext" : encodeString name : encode rd : concatMap encodeOperand' ops
-- `Iextadvise` is `Iadvise` plus a hint to the interpreter.  We serialize it
-- just like a plain `Iadvise`.
encodeInstr (Iextadvise _ r1 _   ) = encodeInstr @regT @wrdT (Iadvise r1)

decodeOperands :: (Serialise regT, Serialise wrdT) => Int -> Decoder s ([regT], Operand regT wrdT)
decodeOperands 0 = fail "invalid number of operands: 0"
decodeOperands 1 = fail "invalid number of operands: 1"
decodeOperands 2 = do
  oper <- decodeOperand
  return $ ([], oper)
decodeOperands n = do
  op <- decode
  (ops, oper) <- decodeOperands (n-1) 
  return $ (op : ops, oper)

decodeInstr :: (Serialise regT, Serialise ops) => Decoder s (Instruction regT ops)
decodeInstr = do
    _ <- decodeListLen
    tag <- decodeString
    case tag of 
      "and"     -> Iand    <$> decode     <*> decode     <*> decodeOperand' 
      "or"      -> Ior     <$> decode     <*> decode     <*> decodeOperand' 
      "xor"     -> Ixor    <$> decode     <*> decode     <*> decodeOperand' 
      "not"     -> Inot    <$> decode     <*  decodeNull <*> decodeOperand' 
      "add"     -> Iadd    <$> decode     <*> decode     <*> decodeOperand' 
      "sub"     -> Isub    <$> decode     <*> decode     <*> decodeOperand' 
      "mull"    -> Imull   <$> decode     <*> decode     <*> decodeOperand' 
      "umulh"   -> Iumulh  <$> decode     <*> decode     <*> decodeOperand' 
      "smulh"   -> Ismulh  <$> decode     <*> decode     <*> decodeOperand' 
      "udiv"    -> Iudiv   <$> decode     <*> decode     <*> decodeOperand' 
      "umod"    -> Iumod   <$> decode     <*> decode     <*> decodeOperand' 
      "shl"     -> Ishl    <$> decode     <*> decode     <*> decodeOperand' 
      "shr"     -> Ishr    <$> decode     <*> decode     <*> decodeOperand' 
      "cmpe"    -> Icmpe   <$  decodeNull <*> decode     <*> decodeOperand' 
      "cmpa"    -> Icmpa   <$  decodeNull <*> decode     <*> decodeOperand' 
      "cmpae"   -> Icmpae  <$  decodeNull <*> decode     <*> decodeOperand' 
      "cmpg"    -> Icmpg   <$  decodeNull <*> decode     <*> decodeOperand' 
      "cmpge"   -> Icmpge  <$  decodeNull <*> decode     <*> decodeOperand' 
      "mov"     -> Imov    <$> decode     <*  decodeNull <*> decodeOperand' 
      "cmov"    -> Icmov   <$> decode     <*  decodeNull <*> decodeOperand' 
      "jmp"     -> Ijmp    <$  decodeNull <*  decodeNull <*> decodeOperand' 
      "cjmp"    -> Icjmp   <$  decodeNull <*  decodeNull <*> decodeOperand' 
      "cnjmp"   -> Icnjmp  <$  decodeNull <*  decodeNull <*> decodeOperand' 
      "store"   -> flip Istore  <$  decodeNull <*> decode     <*> decodeOperand' 
      "load"    -> Iload   <$> decode     <*  decodeNull <*> decodeOperand'
      "read"    -> Iread   <$> decode     <*  decodeNull <*> decodeOperand'
      "answer"  -> Ianswer <$  decodeNull <*  decodeNull <*> decodeOperand'
      "poison"  -> flip Ipoison  <$  decodeNull <*> decode     <*> decodeOperand'  
      "advise"  -> Iadvise <$> decode     <*  decodeNull <*  decodeBool <* decodeNull
      _ -> fail $ "invalid instruction encoding. Tag: " ++ show tag ++ "."

instance (Serialise regT, Serialise ops) => Serialise (Instruction regT ops) where
    encode = encodeInstr
    decode = decodeInstr

{- Quick test:-}
a :: Program Word MWord
a = [Istore (Reg 77) 1, Ijmp (Reg 42),Iadd 2 3 (Const 4)]
x :: Either String (Instruction' Word Word (Operand Word MWord))
x = fromFlatTerm decode $ toFlatTerm $ encode a

b :: Instruction Word MWord
b = Istore (Reg 0) 0
y :: L.ByteString
y = serialise b


-- ** Parameters

encodeInstrKind :: InstrKind -> Encoding
encodeInstrKind ik = encodeString $ TXT.pack $ show ik

decodeInstrKind :: Decoder s InstrKind
decodeInstrKind = do
  txt <- decodeString
  return $ read $ TXT.unpack txt

instance Serialise InstrKind where
  encode = encodeInstrKind
  decode = decodeInstrKind

encodeParams :: CircuitParameters -> Encoding 
encodeParams (CircuitParameters numRegs len sparc ) = 
  map2CBOR $
  [ ("num_regs", encodeWord numRegs)
  , ("trace_len", encodeWord len)
  , ("sparcity", encode sparc)
  ]

decodeParams :: Decoder s CircuitParameters 
decodeParams = do
  len <- decodeMapLen
  case len of
    3 -> CircuitParameters <$ decodeString <*> decodeWord
         <* decodeString <*> decodeWord
         <* decodeString <*> decode
    _ -> fail $ "invalid parameters encoding. Length should be 3 but found " ++ show len


instance Serialise CircuitParameters where
  encode = encodeParams
  decode = decodeParams


-- ** Initial Mem
-- Some parts of this are public and some private


encodeInitMemSegment :: InitMemSegment -> Encoding
encodeInitMemSegment (InitMemSegment secret read start len datas labels) =
  map2CBOR $
  [ ("secret", encodeBool secret) 
  , ("read_only", encodeBool read)
  , ("start", encode start)
  , ("len", encode len)
  ] ++  encodeMaybeContent "data" datas
    ++  encodeMaybeContent "tainted" labels

encodeMaybeContent :: Serialise a => TXT.Text -> Maybe a -> [(TXT.Text,Encoding)]
encodeMaybeContent _ Nothing = []
encodeMaybeContent s (Just content) = return (s,encode content)


decodeInitMemSegment :: Decoder s InitMemSegment
decodeInitMemSegment = do
    len <- decodeMapLen
    case len of
      4 -> InitMemSegment <$> tagDecode <*> tagDecode <*> tagDecode <*> tagDecode <*> pure Nothing <*> pure Nothing
      6 -> InitMemSegment <$> tagDecode <*> tagDecode <*>
           tagDecode <*> tagDecode <*> fmap Just tagDecode <*> fmap Just tagDecode
      _ -> fail $ "invalid state encoding. Length should be 4 or 5 but found " ++ show len

instance Serialise InitMemSegment where
  decode = decodeInitMemSegment
  encode = encodeInitMemSegment

-- * Secret Output
-- Public output is generated "statically" (without knowing the input). It can be obtained
-- by the verifier and the prover and has the following elements:
-- 1. Trace
-- 2. Advice
-- 3. Initial Memory



-- ** Traces 

-- *** State Out 

encodeStateOut :: StateOut -> Encoding
encodeStateOut (StateOut flag pc regs regLabels) =
  map2CBOR $
  [ ("flag", encodeBool flag) 
  , ("pc", encode pc)
  , ("regs", encode regs)
  , ("tainted_regs", encode regLabels)
  ]

decodeStateOut :: Decoder s StateOut
decodeStateOut = do
    len <- decodeMapLen
    case len of
      4 -> StateOut <$ decodeString <*> decodeBool
                    <* decodeString <*> decode
                    <* decodeString <*> decode
                    <* decodeString <*> decode
      _ -> fail $ "invalid state encoding. Length should be 3 but found " ++ show len

instance Serialise StateOut where
  decode = decodeStateOut
  encode = encodeStateOut


-- ** Advice

encodeMemOpType :: MemOpType -> Encoding
encodeMemOpType MOStore = encodeString "write"
encodeMemOpType MOLoad = encodeString "read"
encodeMemOpType MOPoison = encodeString "poison"

decodeMemOpType :: Decoder s MemOpType
decodeMemOpType = do
  memOp <- decodeString
  case memOp of
    "write" -> return MOStore
    "read" -> return MOLoad
    "poison" -> return MOPoison
    t -> fail $ "Memory operation not known: " ++ show t 

instance Serialise MemOpType where
  decode = decodeMemOpType
  encode = encodeMemOpType


encodeAdvice :: Advice -> Encoding 
encodeAdvice  (MemOp addr val opTyp label) =
  encodeListLen 5
  <> encodeString "MemOp"
  <> encode addr
  <> encode val
  <> encode opTyp
  <> encode label

encodeAdvice (Advise w) =
  encodeListLen 2
  <> encodeString "Advise"
  <> encode w

encodeAdvice  Stutter =
  encodeListLen 1 <>
  encodeString "Stutter"
  
decodeAdvice :: Decoder s Advice
decodeAdvice = do
  ln <- decodeListLen
  name <- decodeString
  case (ln,name) of
    (5, "MemOp") -> MemOp <$> decode <*> decode <*> decode <*> decode
    (1, "Stutter") -> return Stutter
    (ln,name) -> fail $ "Found bad advice of length " ++ show ln ++ " and name: " ++ show name 

instance Serialise Advice where
  decode = decodeAdvice
  encode = encodeAdvice



-- ** Initial memory

-- Serialise is derived from lists and Words.



-- * Patch work
-- We prove the instance of Words to be serialisable
-- The hack here is that is not revertible names will go into Word's

encodeName :: Name -> Encoding
encodeName =  encodeWord . toWord

decodeName :: Decoder s Name
decodeName = do
  wrd <- decodeWord
  return $ fromWord wrd 

instance Serialise Name where
  decode = decodeName
  encode = encodeName

-- * Serialisations and other pretty printing formats

serialOutput :: Serialise reg => Output reg -> [String] -> L.ByteString
serialOutput out features = toLazyByteString $ (encode $ (versionBranch version, features, out))

serialInput :: Serialise reg => L.ByteString -> Either DeserialiseFailure (L.ByteString, Output reg)
serialInput string = deserialiseFromBytes (decodeOutput) string 

ppHexOutput :: Serialise reg => Output reg -> String
ppHexOutput out = prettyHexEnc $ encode out

flatOutput :: Serialise reg => Output reg -> FlatTerm
flatOutput out = toFlatTerm $ encode out

data OutFormat =
    StdHex
  | PHex
  | Flat
  deriving (Eq, Ord, Show)


-- NOTE: ONLY StdHEX records version number. The others are for debugging only
printOutputWithFormat :: Serialise reg => OutFormat -> Output reg -> [String] -> String
printOutputWithFormat StdHex out features = show $ (serialOutput out features) 
printOutputWithFormat PHex out _ = ppHexOutput out
printOutputWithFormat Flat out _ = show . flatOutput $ out


c :: Output Word
c = PublicOutput {program = [Ishr 1 0 (Reg 1)], params =
                     CircuitParameters {numRegs = 1, traceLength = 0, sparcity = Map.fromList [(Kjumps,1)]}, initMem = [InitMemSegment {isSecret = False, isReadOnly = True, location = 1, segmentLen = 1, content = Just [1], labels = Just [untainted]}]}

d :: Output Word
d = SecretOutput {program = [Ishr 1 0 (Reg 1)], params =
                     CircuitParameters {numRegs = 1, traceLength = 0, sparcity = Map.fromList [(Kjumps,1)]}, initMem = [InitMemSegment {isSecret = False, isReadOnly = True, location = 1, segmentLen = 1, content = Just [1], labels = Just [untainted]}],
                   trace = [], adviceOut = Map.empty}
 
