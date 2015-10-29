module QInt where

import CLaSH.Prelude
type MaxSize = 5 -- denote the max size allowed for a vertor(list)
type Size    = Unsigned 4 -- size of QVec
type QInt    = Unsigned 4 -- regular int 
indexVec :: Vec MaxSize QInt
indexVec = 1:>2:>3:>4:>5:>Nil
