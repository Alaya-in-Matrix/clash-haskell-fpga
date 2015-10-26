module Queen where

import CLaSH.Prelude
import CLaSH.Sized.Vector
import Debug.Trace

type MaxSize = 8 -- denote the max size allowed for a vertor(list)
type QInt    = Unsigned 4 -- regular int type
data QVec a  = QV {
    list  :: Vec MaxSize a
    , len :: QInt
} deriving(Eq, Show)

instance (Default a) => Default (QVec a) where
    def = QV def 0

testQv :: QVec QInt
testQv = QV $(v [3::(Unsigned 4),1,4,1,5,9,2,6]) 8

size :: QVec a -> QInt
size = len

topEle :: QVec a -> a -- won't check empty stack
topEle qv = (list qv) !! (len qv - 1)

pop :: QVec a -> QVec a  -- won't check empty stack
pop (QV list len) = QV list (len - 1)

rest = pop

push :: a -> QVec a -> QVec a  -- won't check full stack
push ele (QV list len) = QV newList (len+1)
  where newList = replace len ele list

q <~~ e         = push e q -- won't check index violation
q <-< (e, b)    = if b then q <~~ e else q
q =. (idx,ele)  = QV newList newLen
  where newList = replace idx ele (list q)
        newLen  = max (idx+1) (len q)

hwFilterL :: (Default a) => (a->Bool)->QVec a-> QVec a
hwFilterL pred qv@(QV list len) = 
    let withIdx  = zip list (iterateI (+1) 0)
        newPred  = \(ele,idx)->(pred ele) && (idx < len)
        filtered = map newPred withIdx :: Vec MaxSize Bool
        zipped   = zip list filtered 
     in foldl (<-<) def zipped

topEntity :: (QInt->Bool) -> QVec QInt -> QVec QInt
topEntity = hwFilterL


safe :: QInt->QInt->QInt->Bool
safe p q d = undefined
safeAll :: QVec QInt -> QInt -> Bool
safeAll qs = undefined
