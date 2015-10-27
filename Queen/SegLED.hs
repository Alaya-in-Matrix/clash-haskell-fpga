module SegLED where
import CLaSH.Prelude

type SegDisp = Unsigned 8 -- type for display of seg

segEncoder :: (Integral a) => a -> SegDisp
segEncoder n = case n of
                 0 -> 0b00111111
                 1 -> 0b00000110
                 2 -> 0b01011011
                 3 -> 0b01001111
                 4 -> 0b01100110
                 5 -> 0b01101101
                 6 -> 0b01111101
                 7 -> 0b00000111
                 8 -> 0b01111111
                 9 -> 0b01101111
                 _ -> 0b00111111

segV = map segEncoder
