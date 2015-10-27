module QVec where
import CLaSH.Prelude
import qualified Data.List as L
import SegLED

(+++)   = (L.++)
q <~~ e = push e q -- won't check index violation

type MaxSize = 5 -- denote the max size allowed for a vertor(list)
type Size    = Unsigned 4 -- size of QVec
type QInt    = Unsigned 4 -- regular int 
indexVec :: Vec MaxSize QInt
indexVec = 1:>2:>3:>4:>5:>Nil

data QVec a  = QV {
    list  :: Vec MaxSize a
    , len :: Size
} deriving(Eq)

instance (Default a) => Default (QVec a) where
    def = QV def 0

instance (Show a) => Show (QVec a) where
    show (QV vec len) = "QVec " +++ (show $ L.take (fromIntegral len) (toList vec))

topEle :: QVec a -> a -- won't check empty stack
topEle qv = (list qv) !! (len qv - 1)

pop :: QVec a -> QVec a  -- won't check empty stack
pop (QV list len) = QV list (len - 1)

push :: a -> QVec a -> QVec a  -- won't check full stack
push ele (QV list len) = QV newList (len+1)
  where newList = replace len ele list

hwFilterL :: (Default a) => (a->Bool)->QVec a-> QVec a
hwFilterL pred qv@(QV list len) = 
    let filtered = imap (\i e -> (len > fromIntegral i) && pred e) list
     in foldl (\qs (e,b) -> if b then qs <~~ e else qs) def $ zip list filtered