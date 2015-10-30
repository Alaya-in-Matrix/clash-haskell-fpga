module QVec where
import CLaSH.Prelude
import QInt
import qualified Data.List as L

(+++)   = (L.++)
q <~~ e = push e q -- won't check index violation

data QVec a  = QV {
    list  :: Vec MaxSize a
    , len :: Size
} deriving(Eq)

instance (Default a) => Default (QVec a) where
    def = QV def 0

instance (Show a) => Show (QVec a) where
    show (QV vec len) = "QVec " +++ (show $ L.take (fromIntegral len) (toList vec))
isEmpty :: QVec a -> Bool
isEmpty v = len v == 0

top :: QVec a -> a -- won't check empty stack
top qv = (list qv) !! (len qv - 1)

pop :: (Default a) => QVec a -> QVec a  -- won't check empty stack
pop (QV list len) = QV (replace (len-1) def list) (len - 1)

push :: a -> QVec a -> QVec a  -- won't check full stack
push ele (QV list len) = QV newList (len+1)
  where newList = replace len ele list

hwFilterL :: (Default a) => (a->Bool)->QVec a-> QVec a
hwFilterL pred qv@(QV list len) = 
    let filtered = imap (\i e -> (len > fromIntegral i) && pred e) list
     in foldl (\qs (e,b) -> if b then qs <~~ e else qs) def $ zip list filtered

qmap :: (a->b) -> QVec a -> QVec b
qmap f (QV v l) = QV (map f v) l

qfoldl :: (a->b->a)->a->(QVec b)-> a
qfoldl f x qv@(QV vec sz) = ifoldl newf x vec
    where newf curr idx newv = if (fromIntegral idx) < sz then (f curr newv) else curr
