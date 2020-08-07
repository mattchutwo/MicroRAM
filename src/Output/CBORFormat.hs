{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : JSONFormat
Description : Format the output of the compiler for JSON
Maintainer  : santiago@galois.com
Stability   : experimental

Format for compiler units

-}

module Output.CBORFormat where
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)
import qualified Data.Map as Map
import GHC.Generics

import Codec.Serialise
import Codec.CBOR
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
  
import Compiler.Sparsity
import Compiler.CompilationUnit
import Compiler.Registers

import MicroRAM.MRAMInterpreter
import MicroRAM.MicroRAM

-- * Public output

-- ** Program 

-- It is enough to show an instance of `Serialise (Instruction r w)` to get an instance for
-- full programs (given the instance `Serialise a => Serialise [a]`).

deriving instance Generic Int
deriving instance Generic Word

data OpKind = KReg | KImm

instance Serialise OpKind where
    encode = undefined
    decode = undefined

class EncodeAsWord t where
  toWord :: t -> Word
  fromWord :: OpKind -> Word -> t
    
  kindOfWord :: t -> OpKind
  
instance EncodeAsWord Word where
  toWord w = w
  fromWord _ w = w

  kindOfWord _ = KReg

{-
instance (EncodeAsWord regT, EncodeAsWord wrdT) =>  EncodeAsWord (Operand regT wrdT) where
  toWord (Reg r) = toWord r
  toWord (Const c) = toWord c

  fromWord KReg r = Reg $ fromWord KReg r
  fromWord KImm c = Const $ fromWord KImm c

  kindOfWord (Reg r) = KReg
  kindOfWord (Const r) = KImm
-}

lengthW :: Foldable t => t a -> Word
lengthW = fromIntegral . length

list2CBOR :: [Encoding] -> Encoding
list2CBOR ls = foldr (<>) mempty (encodeListLen (lengthW ls) : ls )


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
encodeInstr (Iand r1 r2 operand  ) = list2CBOR $ encodeWord  0 : encode r1  : encode r2  : (encodeOperand' operand)
encodeInstr (Ior r1 r2 operand   ) = list2CBOR $ encodeWord  1 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ixor r1 r2 operand  ) = list2CBOR $ encodeWord  2 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Inot r1 operand     ) = list2CBOR $ encodeWord  3 : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Iadd r1 r2 operand  ) = list2CBOR $ encodeWord  4 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Isub r1 r2 operand  ) = list2CBOR $ encodeWord  5 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Imull r1 r2 operand ) = list2CBOR $ encodeWord  6 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iumulh r1 r2 operand) = list2CBOR $ encodeWord  7 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ismulh r1 r2 operand) = list2CBOR $ encodeWord  8 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iudiv r1 r2 operand ) = list2CBOR $ encodeWord  9 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Iumod r1 r2 operand ) = list2CBOR $ encodeWord 10 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ishl r1 r2 operand  ) = list2CBOR $ encodeWord 11 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Ishr r1 r2 operand  ) = list2CBOR $ encodeWord 12 : encode r1  : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpe r2 operand    ) = list2CBOR $ encodeWord 13 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpa r2 operand    ) = list2CBOR $ encodeWord 14 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpae r2 operand   ) = list2CBOR $ encodeWord 15 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpg r2 operand    ) = list2CBOR $ encodeWord 16 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Icmpge r2 operand   ) = list2CBOR $ encodeWord 17 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Imov r1 operand     ) = list2CBOR $ encodeWord 18 : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Icmov r1 operand    ) = list2CBOR $ encodeWord 19 : encode r1  : encodeNull : (encodeOperand' operand) 
encodeInstr (Ijmp operand        ) = list2CBOR $ encodeWord 20 : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Icjmp operand       ) = list2CBOR $ encodeWord 21 : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Icnjmp operand      ) = list2CBOR $ encodeWord 22 : encodeNull : encodeNull : (encodeOperand' operand) 
encodeInstr (Istore operand r2   ) = list2CBOR $ encodeWord 23 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Iload r2 operand    ) = list2CBOR $ encodeWord 24 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Iread r2 operand    ) = list2CBOR $ encodeWord 25 : encodeNull : encode r2  : (encodeOperand' operand) 
encodeInstr (Ianswer operand     ) = list2CBOR $ encodeWord 26 : encodeNull : encodeNull : (encodeOperand' operand) 

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
    tag <- decodeWord
    case tag of 
      0  -> Iand    <$> decode     <*> decode     <*> decodeOperand' 
      1  -> Ior     <$> decode     <*> decode     <*> decodeOperand' 
      2  -> Ixor    <$> decode     <*> decode     <*> decodeOperand' 
      3  -> Inot    <$> decode     <*  decodeNull <*> decodeOperand' 
      4  -> Iadd    <$> decode     <*> decode     <*> decodeOperand' 
      5  -> Isub    <$> decode     <*> decode     <*> decodeOperand' 
      6  -> Imull   <$> decode     <*> decode     <*> decodeOperand' 
      7  -> Iumulh  <$> decode     <*> decode     <*> decodeOperand' 
      8  -> Ismulh  <$> decode     <*> decode     <*> decodeOperand' 
      9  -> Iudiv   <$> decode     <*> decode     <*> decodeOperand' 
      10 -> Iumod   <$> decode     <*> decode     <*> decodeOperand' 
      11 -> Ishl    <$> decode     <*> decode     <*> decodeOperand' 
      12 -> Ishr    <$> decode     <*> decode     <*> decodeOperand' 
      13 -> Icmpe   <$  decodeNull <*> decode     <*> decodeOperand' 
      14 -> Icmpa   <$  decodeNull <*> decode     <*> decodeOperand' 
      15 -> Icmpae  <$  decodeNull <*> decode     <*> decodeOperand' 
      16 -> Icmpg   <$  decodeNull <*> decode     <*> decodeOperand' 
      17 -> Icmpge  <$  decodeNull <*> decode     <*> decodeOperand' 
      18 -> Imov    <$> decode     <*  decodeNull <*> decodeOperand' 
      19 -> Icmov   <$> decode     <*  decodeNull <*> decodeOperand' 
      20 -> Ijmp    <$  decodeNull <*  decodeNull <*> decodeOperand' 
      21 -> Icjmp   <$  decodeNull <*  decodeNull <*> decodeOperand' 
      22 -> Icnjmp  <$  decodeNull <*  decodeNull <*> decodeOperand' 
      23 -> flip Istore  <$  decodeNull <*> decode     <*> decodeOperand' 
      24 -> Iload   <$  decodeNull <*> decode     <*> decodeOperand' 
      25 -> Iread   <$  decodeNull <*> decode     <*> decodeOperand' 
      26 -> Ianswer <$  decodeNull <*  decodeNull <*> decodeOperand' 
      _ -> fail $ "invalid instruction encoding. Tag: " ++ show tag ++ "."
  
instance (Serialise regT, Serialise ops) => Serialise (Instruction regT ops) where
    encode = encodeInstr
    decode = decodeInstr

{- Quick test:-}
a :: Program Word Word
a = [Istore (Reg 77) 1, Ijmp (Reg 42),Iadd 2 3 (Const 4)]
x :: Either String (Instruction' Word (Operand Word Word))
x = fromFlatTerm decode $ toFlatTerm $ encode a

b :: Instruction Word Word
b = Istore (Reg 0) 0
y = serialise b


-- ** Traces 

-- | State with only the parts passed to the output.

data StateOut = StateOut
  { flagOut :: Bool
  , pcOut   :: Word 
  , regsOut :: [Word]
  } deriving (Eq, Show, Generic)

state2out :: Regs mreg => Word -> State mreg -> StateOut
state2out bound (State pc regs _ _ flag _ _) = StateOut flag pc (regToList bound regs)

encodeStateOut :: StateOut -> Encoding
encodeStateOut (StateOut flag pc regs) =
  encodeMapLen 3
  <> encodeString "flag" <> encodeBool flag 
  <> encodeString "pc"   <> encodeWord pc
  <> encodeString "regs" <> encode regs

decodeStateOut :: Decoder s StateOut
decodeStateOut = do
    len <- decodeMapLen
    case len of
      3 -> StateOut <$ decodeString <*> decodeBool
                    <* decodeString <*> decodeWord
                    <* decodeString <*> decode
      _ -> fail $ "invalid state encoding. Length should be 3 but found " ++ show len

instance Serialise StateOut where
  decode = decodeStateOut
  encode = encodeStateOut


-- Compiler/interpreter output
type SparcityInfo = Word


data CircuitParameters = CircuitParameters
  { numRegs :: Word
  , traceLength :: Word
  , sparcity :: Map.Map InstrKind SparcityInfo
  } deriving (Eq, Show, Generic)
      
data Output reg  = Output
  { program :: Program reg Word
  , parms :: CircuitParameters
  , advice :: Map.Map Word Advice
  , trace :: Maybe [StateOut]
  , initMem :: Maybe [Word]
  } deriving (Eq, Show, Generic)
