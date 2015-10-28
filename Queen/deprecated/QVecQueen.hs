module QVec where
import CLaSH.Prelude
import qualified Data.List as L
import SegLED

(+++)   = (L.++)
q <~~ e = push e q -- won't check index violation

type Size    = Unsigned 4 -- size of QVec
type QInt    = Unsigned 4 -- regular int 
-- indexVec :: Vec MaxSize QInt
-- indexVec = 1:>2:>3:>4:>5:>Nil
-- maxLen   = 5

data QVec n a  = QV {
    list  :: Vec n a
    , len :: Size
} deriving(Eq)

instance (KnownNat n, Default a) => Default (QVec n a) where
    def = QV def 0

instance (KnownNat n, Show a) => Show (QVec n a) where
    show (QV vec len) = "QVec " +++ (show $ L.take (fromIntegral len) (toList vec))

top :: (KnownNat n) => QVec n a -> a -- won't check empty stack
top qv = (list qv) !! (len qv - 1)

pop :: (KnownNat n) => QVec n a -> QVec n a  -- won't check empty stack
pop (QV list len) = QV list (len - 1)

push :: (KnownNat n) => a -> QVec n a -> QVec n a  -- won't check full stack
push ele (QV list len) = QV newList (len+1)
  where newList = replace len ele list

hwFilterL :: (Default a, KnownNat n) => (a->Bool)->QVec n a-> QVec n a
hwFilterL pred qv@(QV list len) = 
    let filtered = imap (\i e -> (len > fromIntegral i) && pred e) list
     in foldl (\qs (e,b) -> if b then qs <~~ e else qs) def $ zip list filtered

qmap :: (KnownNat n) => (a->b) -> QVec n a -> QVec n b
qmap f (QV qv len) = QV (map f qv) len

qfoldl :: (KnownNat n) => (a -> b -> a) -> a -> QVec n b -> a
qfoldl f x qv@(QV vec sz) = ifoldl newf x vec
  where newf = \curr id newv -> if (fromIntegral id) < sz then (f curr newv) else curr


qzipWith :: (KnownNat n) => (a->b->c) -> QVec n a -> QVec n b -> QVec n c
qzipWith f (QV xs xLen) (QV ys yLen) 
    | xLen < yLen = QV (imap f1 xs) xLen
    | otherwise   = QV (imap f2 ys) yLen 
        where f1 idx ele = f ele (ys !! fromIntegral idx)
              f2 idx ele = f (xs !! fromIntegral idx) ele

revIdx :: (KnownNat n) => QVec n a -> QVec n (Signed 16)
revIdx qv@(QV v l) = QV (iterateI (\x->x-1) (fromIntegral l)) l


safeAll :: (KnownNat n) => QVec n QInt -> QInt -> Bool
safeAll qs p = foldl (&&) True mapped -- perhaps I need to use foldl?
    where mapped = imap isafe (list qs)
          isafe idx q | fromIntegral idx >= qlen = True
                      | otherwise = (p /= q && (delta /= qlen - fromIntegral idx))
                          where delta = max p q - min p q
                                qlen  = len qs


qextensions :: (KnownNat n1, KnownNat n2) => QInt -> QVec n1 QInt -> QVec n2 (QVec n1 QInt)
qextensions boardSize qs = qmap (qs <~~) $ hwFilterL (safeAll qs) (QV (iterateI (+1) 1) boardSize)


(+.+) :: (KnownNat n) => QVec n a -> QVec n a -> QVec n a
(+.+) va vb = qfoldl (<~~) va vb

qconcat :: (KnownNat n1, KnownNat n2, Default a) => QVec n1 (QVec n2 a) -> QVec n2 a -- won't check maxLen violation
qconcat qqs = qfoldl (+.+) def qqs

f :: QVec 1000 (QVec 5 QInt) -> QVec 1000 (QVec 5 QInt)
f = qconcat . (qmap (qextensions 5))

g :: QVec 1000 (QVec 5 QInt) -> QVec 1000 (QVec 1000 (QVec 5 QInt))
g = qmap (qextensions 5)

sinit :: QVec 1000 (QVec 5 QInt)
sinit = def <~~ def
