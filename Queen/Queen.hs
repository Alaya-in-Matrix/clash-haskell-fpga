module Queen where

import CLaSH.Prelude
import CLaSH.Sized.Vector
import Debug.Trace

type MaxSize = 8 -- denote the max size allowed for a vertor(list)
type Size    = Unsigned 4 -- size of QVec
type QInt    = Unsigned 4 -- regular int type
data QVec a  = QV {
    list  :: Vec MaxSize a
    , len :: Size
} deriving(Eq, Show)

instance (Default a) => Default (QVec a) where
    def = QV def 0

indexVec :: Vec MaxSize QInt
indexVec = 1:>2:>3:>4:>5:>6:>7:>8:>Nil

testQv :: QVec Int
testQv = QV $(v [3::Int,1,4,1,5,9,2,6]) 8

topEle :: QVec a -> a -- won't check empty stack
topEle qv = (list qv) !! (len qv - 1)

pop :: QVec a -> QVec a  -- won't check empty stack
pop (QV list len) = QV list (len - 1)

push :: a -> QVec a -> QVec a  -- won't check full stack
push ele (QV list len) = QV newList (len+1)
  where newList = replace len ele list

q <~~ e         = push e q -- won't check index violation
q <-< (e, b)    = if b then q <~~ e else q

hwFilterL :: (Default a) => (a->Bool)->QVec a-> QVec a
hwFilterL pred qv@(QV list len) = 
    let filtered = imap (\i e -> (len > fromIntegral i) && pred e) list
     in foldl (<-<) def $ zip list filtered

topEntity :: QVec QInt -> QVec QInt
topEntity = hwFilterL odd

qmap :: (a->b) -> QVec a -> QVec b
qmap f (QV qv len) = QV (map f qv) len

qfoldl :: (a -> b -> a) -> a -> QVec b -> a
qfoldl f x qv@(QV vec sz) = ifoldl newf x vec
  where newf = \curr id newv -> if (fromIntegral id) < sz then f curr newv
                                                          else curr
safeAll :: QVec QInt -> QInt -> Bool
-- safeAll qs@(QV qlist qlen) p = foldl (&&) True mapped -- perhaps I need to use foldl?
safeAll qs@(QV qlist qlen) p = fold (&&) (True:>mapped) -- perhaps I need to use foldl?
    where mapped = imap isafe qlist
          isafe idx q | fromIntegral idx >= qlen = True
                      | otherwise = (p /= q && abs (p-q) /= qlen - fromIntegral idx)

testSafe :: QVec QInt
testSafe = def <~~ 1 <~~ 4

data Out = Out {
    solution :: Maybe (QVec QInt)
    , finish :: Bool
} deriving(Eq, Show)
-- further: data State = State Stack Size(boardSize)

type Stack =  QVec (QVec QInt, QVec QInt)
data QState = QS {boardSize :: Size, stack::Stack} deriving(Eq, Show)

initQState iSize = QS iSize (def <~~ (def, QV indexVec iSize))

queenStateM :: QState -> Size -> (QState, Out)
queenStateM qst@(QS boardSize stack) newSize
  | boardSize /= newSize = (initQState newSize, Out Nothing False)
  | len stack == 0       = (qst, Out Nothing True)
  | otherwise            = (QS boardSize stack', out)
      where (qs,ps) = topEle stack
            rest    = pop    stack
            qs'     = qs <~~ (topEle ps)
            ps'     = hwFilterL (safeAll qs') (QV indexVec boardSize)
            top'    = (qs', pop ps)
            newtop  = (qs', ps')
            out 
              | len qs' == boardSize = Out (return qs') False
              | otherwise            = Out Nothing False
            stack'
              | len qs' >= boardSize = if (len ps <= 1) then rest else (rest <~~ top')
              | len ps <= 1 = if(len ps' == 0) then (rest         ) else (rest <~~ newtop)
              | otherwise   = if(len ps' == 0) then (rest <~~ top') else (rest <~~ top' <~~ newtop)
