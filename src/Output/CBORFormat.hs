{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Codec.CBOR
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Write
import Codec.CBOR.Read
import Codec.CBOR.Pretty
import qualified Data.ByteString.Lazy                  as L

import Compiler.Sparsity
import Compiler.CompilationUnit
import Compiler.Registers
import Compiler.IRs

import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM

import qualified Data.Text as TXT 
import qualified Data.Text.Internal  as TXT

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


encodeInstr :: (Serialise regT, Serialise wrdT) => Instruction regT wrdT -> Encoding
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
encodeInitMemSegment (InitMemSegment secret read start len datas) =
  map2CBOR $
  [ ("secret", encodeBool secret) 
  , ("read_only", encodeBool read)
  , ("start", encode start)
  , ("len", encode len)
  ] ++  encodeMaybeContent datas

encodeMaybeContent :: Maybe [MWord] -> [(TXT.Text,Encoding)]
encodeMaybeContent Nothing = []
encodeMaybeContent (Just content) = return $ ("data",encode content)


decodeInitMemSegment :: Decoder s InitMemSegment
decodeInitMemSegment = do
    len <- decodeMapLen
    case len of
      4 -> InitMemSegment <$> tagDecode <*> tagDecode <*> tagDecode <*> tagDecode <*> (return Nothing)
      5 -> InitMemSegment <$> tagDecode <*> tagDecode <*>
           tagDecode <*> tagDecode <*> do { content <- tagDecode; return $ Just content} 
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
encodeStateOut (StateOut flag pc regs) =
  map2CBOR $
  [ ("flag", encodeBool flag) 
  , ("pc", encode pc)
  , ("regs", encode regs)
  ]

decodeStateOut :: Decoder s StateOut
decodeStateOut = do
    len <- decodeMapLen
    case len of
      3 -> StateOut <$ decodeString <*> decodeBool
                    <* decodeString <*> decode
                    <* decodeString <*> decode
      _ -> fail $ "invalid state encoding. Length should be 3 but found " ++ show len

instance Serialise StateOut where
  decode = decodeStateOut
  encode = encodeStateOut


-- ** Advice

encodeMemOpType :: MemOpType -> Encoding
encodeMemOpType MOStore = encodeBool True
encodeMemOpType MOLoad = encodeBool False

decodeMemOpType :: Decoder s MemOpType
decodeMemOpType = do
  b <- decodeBool
  return $ if b then MOStore else MOLoad

instance Serialise MemOpType where
  decode = decodeMemOpType
  encode = encodeMemOpType


encodeAdvice :: Advice -> Encoding 
encodeAdvice  (MemOp addr val opTyp) =
  encodeListLen 4
  <> encodeString "MemOp"
  <> encode addr
  <> encode val
  <> encode opTyp
  
encodeAdvice  Stutter =
  encodeListLen 1 <>
  encodeString "Stutter"
  
decodeAdvice :: Decoder s Advice
decodeAdvice = do
  ln <- decodeListLen
  name <- decodeString
  case (ln,name) of
    (4, "MemOp") -> MemOp <$> decode <*> decode <*> decode
    (1, "Stutter") -> return Stutter

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

serialOutput :: Serialise reg => Output reg -> L.ByteString
serialOutput out = toLazyByteString $ (encode out)

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

printOutputWithFormat :: Serialise reg => OutFormat -> Output reg -> String
printOutputWithFormat StdHex = show . serialOutput
printOutputWithFormat PHex = ppHexOutput
printOutputWithFormat Flat = show . flatOutput


c :: Output Word
c = PublicOutput {program = [Ishr 1 0 (Reg 1)], params =
                     CircuitParameters {numRegs = 1, traceLength = 0, sparcity = Map.fromList [(Kjumps,1)]}, initMem = [InitMemSegment {isSecret = False, isReadOnly = True, location = 1, segmentLen = 1, content = Just [1]}]}

d :: Output Word
d = SecretOutput {program = [Ishr 1 0 (Reg 1)], params =
                     CircuitParameters {numRegs = 1, traceLength = 0, sparcity = Map.fromList [(Kjumps,1)]}, initMem = [InitMemSegment {isSecret = False, isReadOnly = True, location = 1, segmentLen = 1, content = Just [1]}],
                   trace = [], adviceOut = Map.empty}
 
