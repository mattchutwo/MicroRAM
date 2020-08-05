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

import Codec.Serialise
import Codec.CBOR
import Codec.CBOR.Decoding (Decoder, decodeWord, decodeListLen, decodeBool)
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord, encodeBool)
  
import Compiler.CompilationUnit
import GHC.Generics

import MicroRAM.MicroRAM
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)

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

instance (EncodeAsWord regT, EncodeAsWord wrdT) =>  EncodeAsWord (Operand regT wrdT) where
  toWord (Reg r) = toWord r
  toWord (Const c) = toWord c

  fromWord KReg r = Reg $ fromWord KReg r
  fromWord KImm c = Const $ fromWord KImm c

  kindOfWord (Reg r) = KReg
  kindOfWord (Const r) = KImm

lengthW :: Foldable t => t a -> Word
lengthW = fromIntegral . length

list2CBOR :: [Encoding] -> Encoding
list2CBOR ls = foldr (<>) mempty (encodeListLen (lengthW ls) : ls )

encodeOperand :: (Serialise regT, Serialise wrdT) => Operand regT wrdT -> Encoding
encodeOperand (Reg r) = encodeBool False <> encode r 
encodeOperand (Const c) = encodeBool True <> encode c 

decodeOperand :: (Serialise regT, Serialise wrdT) => Decoder s (Operand regT wrdT)
decodeOperand = do
  kind <- decodeBool
  case kind of
    False -> Reg <$> decode
    True  -> Const <$> decode

instance (Serialise regT, Serialise wrdT) => Serialise (Operand regT wrdT) where
    encode = encodeOperand
    decode = decodeOperand
  

encodeInstr :: (Serialise regT, Serialise ops) => Instruction' regT ops -> Encoding
encodeInstr (Iand r1 r2 operand  ) = list2CBOR $ encodeWord  0 : encode r1 : encode r2 : encode operand : []
encodeInstr (Ior r1 r2 operand   ) = list2CBOR $ encodeWord  1 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Ixor r1 r2 operand  ) = list2CBOR $ encodeWord  2 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Inot r1 operand     ) = list2CBOR $ encodeWord  3 : encode r1             : encode operand : [] 
encodeInstr (Iadd r1 r2 operand  ) = list2CBOR $ encodeWord  4 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Isub r1 r2 operand  ) = list2CBOR $ encodeWord  5 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Imull r1 r2 operand ) = list2CBOR $ encodeWord  6 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Iumulh r1 r2 operand) = list2CBOR $ encodeWord  7 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Ismulh r1 r2 operand) = list2CBOR $ encodeWord  8 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Iudiv r1 r2 operand ) = list2CBOR $ encodeWord  9 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Iumod r1 r2 operand ) = list2CBOR $ encodeWord 10 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Ishl r1 r2 operand  ) = list2CBOR $ encodeWord 11 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Ishr r1 r2 operand  ) = list2CBOR $ encodeWord 12 : encode r1 : encode r2 : encode operand : [] 
encodeInstr (Icmpe r1 operand    ) = list2CBOR $ encodeWord 13 : encode r1             : encode operand : [] 
encodeInstr (Icmpa r1 operand    ) = list2CBOR $ encodeWord 14 : encode r1             : encode operand : [] 
encodeInstr (Icmpae r1 operand   ) = list2CBOR $ encodeWord 15 : encode r1             : encode operand : [] 
encodeInstr (Icmpg r1 operand    ) = list2CBOR $ encodeWord 16 : encode r1             : encode operand : [] 
encodeInstr (Icmpge r1 operand   ) = list2CBOR $ encodeWord 17 : encode r1             : encode operand : [] 
encodeInstr (Imov r1 operand     ) = list2CBOR $ encodeWord 18 : encode r1             : encode operand : [] 
encodeInstr (Icmov r1 operand    ) = list2CBOR $ encodeWord 19 : encode r1             : encode operand : [] 
encodeInstr (Ijmp operand        ) = list2CBOR $ encodeWord 20                         : encode operand : [] 
encodeInstr (Icjmp operand       ) = list2CBOR $ encodeWord 21                         : encode operand : [] 
encodeInstr (Icnjmp operand      ) = list2CBOR $ encodeWord 22                         : encode operand : [] 
encodeInstr (Istore operand r1   ) = list2CBOR $ encodeWord 23 : encode r1             : encode operand : [] 
encodeInstr (Iload r1 operand    ) = list2CBOR $ encodeWord 24 : encode r1             : encode operand : [] 
encodeInstr (Iread r1 operand    ) = list2CBOR $ encodeWord 25 : encode r1             : encode operand : [] 
encodeInstr (Ianswer operand     ) = list2CBOR $ encodeWord 26                         : encode operand : [] 

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
    len <- decodeListLen
    tag <- decodeWord
    operands <- decodeOperands len
    case (tag,operands) of 
      ( 0 , ([r1, r2], operan)) -> return $ Iand   r1 r2 operan
      ( 1 , ([r1, r2], operan)) -> return $ Ior    r1 r2 operan
      ( 2 , ([r1, r2], operan)) -> return $ Ixor   r1 r2 operan
      ( 3 , ([r1    ], operan)) -> return $ Inot   r1    operan
      ( 4 , ([r1, r2], operan)) -> return $ Iadd   r1 r2 operan
      ( 5 , ([r1, r2], operan)) -> return $ Isub   r1 r2 operan
      ( 6 , ([r1, r2], operan)) -> return $ Imull  r1 r2 operan
      ( 7 , ([r1, r2], operan)) -> return $ Iumulh r1 r2 operan
      ( 8 , ([r1, r2], operan)) -> return $ Ismulh r1 r2 operan
      ( 9 , ([r1, r2], operan)) -> return $ Iudiv  r1 r2 operan
      (10 , ([r1, r2], operan)) -> return $ Iumod  r1 r2 operan
      (11 , ([r1, r2], operan)) -> return $ Ishl   r1 r2 operan
      (12 , ([r1, r2], operan)) -> return $ Ishr   r1 r2 operan
      (13 , ([r1    ], operan)) -> return $ Icmpe  r1    operan
      (14 , ([r1    ], operan)) -> return $ Icmpa  r1    operan
      (15 , ([r1    ], operan)) -> return $ Icmpae r1    operan
      (16 , ([r1    ], operan)) -> return $ Icmpg  r1    operan
      (17 , ([r1    ], operan)) -> return $ Icmpge r1    operan
      (18 , ([r1    ], operan)) -> return $ Imov   r1    operan
      (19 , ([r1    ], operan)) -> return $ Icmov  r1    operan
      (20 , ([      ], operan)) -> return $ Ijmp         operan
      (21 , ([      ], operan)) -> return $ Icjmp        operan
      (22 , ([      ], operan)) -> return $ Icnjmp       operan
      (23 , ([r1    ], operan)) -> return $ Istore       operan r1
      (24 , ([r1    ], operan)) -> return $ Iload  r1    operan
      (25 , ([r1    ], operan)) -> return $ Iread  r1    operan
      (26 , ([      ], operan)) -> return $ Ianswer      operan
      _ -> fail $ "invalid instruction encoding. Tag: " ++ show tag ++ "."
  
instance (Serialise regT, Serialise ops) => Serialise (Instruction regT ops) where
    encode = encodeInstr
    decode = decodeInstr

{- Quick test:
a :: Instruction' Word (Operand Word Word)
a = Ijmp (Reg 0)
x :: Either String (Instruction' Word (Operand Word Word))
x = fromFlatTerm decode $ toFlatTerm $ encode a
-}




