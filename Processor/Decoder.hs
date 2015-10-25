module Decoder(decoder) where 

import CLaSH.Prelude
import Prog 


decoder :: Unsigned 21 -> Instr
decoder d = case instPart of
                  0  -> Push dataPart
                  1  -> Duplicate
                  2  -> Swap
                  3  -> Pop
                  4  -> Add
                  5  -> Sub
                  6  -> Mul
                  7  -> Div
                  9  -> Mod
                  10 -> OutChar
                  11 -> OutInt
                  12 -> ReadChar
                  13 -> ReadInt
                  14 -> Jump     dataPart
                  15 -> JumpZero dataPart
                  16 -> JumpNeg  dataPart
                  17 -> ProgEnd
                  _  -> ProgEnd
  where bits     = pack d :: BitVector 21
        instPart = unpack $ slice d20 d16 bits :: Unsigned 5
        dataPart = unpack $ slice d15 d0  bits :: Unsigned 16
