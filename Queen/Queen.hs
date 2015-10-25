module Queen where

import CLaSH.Prelude

type MaxSize = 8
type IntData = (Signed 5)
type QNbr     = Signed 5
type QVec a   = Vec MaxSize a
type StackElm = ( QVec QNbr
                , QNbr
                , QVec QNbr
                , QNbr
                , QNbr)
type Stack = (Signed 5, Vec MaxSize StackElm)
data Cmd = Run | Stop deriving(Eq, Show)
data Out = Out {
    solution :: Maybe (QVec QNbr)
    , finish :: Bool
} deriving(Eq, Show)

boardSize    = 8 :: IntData

(<~~) :: (KnownNat n) => Vec n a -> (IntData, a) -> Vec n a
mem <~~ (idx,ele) = replace idx ele mem

(<-<) :: (KnownNat n) => (Vec n a, IntData) -> (a, Bool) -> (Vec n a, IntData)
(mem,n) <-< (_, False) = (mem,n)
(mem,n) <-< (e, True)  = (mem <~~ (n,e), (n+1))

hwFilterL :: (Default a, KnownNat n) => (a->Bool)->Vec n a -> (Vec n a, IntData)
hwFilterL pred vec =
    let zipped = zip vec $ map pred vec
     in foldl (<-<) (def, 0) zipped


indexVec :: (Num a) => Vec MaxSize a
indexVec = iterateI (+1) 1 

size :: Stack -> IntData
size = (+1) . fst
getTop :: Stack -> StackElm
getTop (idx, vec) = vec !! idx
getRest :: Stack -> Stack
getRest = pop
pop :: Stack -> Stack
pop (idx, vec) = (idx-1, vec)
push :: StackElm -> Stack -> Stack
push ele (idx, vec) = (newIdx, vec <~~ (newIdx,ele)) 
  where newIdx = idx+1


safeF p q d = d <= 0 || (p /= q && abs(p-q) /= d)

safeFAll :: (QVec QNbr, QNbr) -> QNbr -> Bool
safeFAll (qs, n) p = foldl (&&) True $ zipWith (safeF p) qs ds
    where ds = iterateI (\x->(x-1)) n

queensM :: Stack -> Cmd -> (Stack, Out)
queensM st@(-1,_) _ = (st,Out Nothing True)
queensM stack     _ = (stack', out)
  where 
    top    = getTop stack  :: StackElm
    rest   = getRest stack :: Stack
    (qs, n, ps, m, k) = top
    (qs', n') = (qs <~~ (n, ps!!k), (n+1))
    (ps', m') = hwFilterL (safeFAll (qs',n')) indexVec
    top'      = (qs, n, ps, m, (k+1))
    nexttop   = (qs',(n+1),ps',m',0)
    stack' 
      | n' == (boardSize-1) && k == (m-1)             = rest
      | n' == (boardSize-1) && k <  (m-1)             = push top' rest
      | n' <  (boardSize-1) && k == (m-1) && m' == 0  = rest
      | n' <  (boardSize-1) && k == (m-1) && m' >  0  = push nexttop rest 
      | n' <  (boardSize-1) && k <  (m-1) && m' == 0  = push top' rest
      | n' <  (boardSize-1) && k <  (m-1) && m' >  0  = push nexttop $ push top' rest
    out 
      | n == (boardSize-2) && m' == 1 = Out (Just $ (qs' <~~ (n', ps' !! 0))) False
      | otherwise                     = Out Nothing False

initStack = (0, repeat (def,0,(iterateI (+1) 1),boardSize,0)) :: Stack

topEntity = queensM `mealy` initStack

testInput :: Signal Cmd
testInput = signal Run

samp sampNum = sampleN sampNum $ topEntity testInput

fuck n = mapM_ print $ filter ((/= Nothing).solution) $ samp n
