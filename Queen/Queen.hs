module Queen where

import CLaSH.Prelude
import CLaSH.Sized.Vector
import Debug.Trace
import Data.List((++))

type MaxSize = 5 -- denote the max size allowed for a vertor(list)
indexVec :: Vec MaxSize QInt
indexVec = 1:>2:>3:>4:>5:>Nil


type Size    = Unsigned 4 -- size of QVec
type QInt    = Unsigned 4 -- regular int type
type SegDisp = Unsigned 8 -- type for display of seg
data QVec a  = QV {
    list  :: Vec MaxSize a
    , len :: Size
} deriving(Eq, Show)

instance (Default a) => Default (QVec a) where
    def = QV def 0

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


qmap :: (a->b) -> QVec a -> QVec b
qmap f (QV qv len) = QV (map f qv) len

qfoldl :: (a -> b -> a) -> a -> QVec b -> a
qfoldl f x qv@(QV vec sz) = ifoldl newf x vec
  where newf = \curr id newv -> if (fromIntegral id) < sz then f curr newv
                                                          else curr
safeAll :: QVec QInt -> QInt -> Bool
-- safeAll qs@(QV qlist qlen) p = foldl (&&) True mapped -- perhaps I need to use foldl?
safeAll qs@(QV qlist qlen) p = fold (&&) mapped -- perhaps I need to use foldl?
    where mapped = imap isafe qlist
          isafe idx q | fromIntegral idx >= qlen = True
                      | otherwise = (p /= q && (delta /= qlen - fromIntegral idx))
                          where delta = max p q - min p q

data Out = Out {
    solution :: Maybe (QVec QInt)
    , finish :: Bool
    , counter :: Unsigned 8
} deriving(Eq, Show)
-- further: data State = State Stack Size(boardSize)

type Stack =  QVec (QVec QInt, QVec QInt)
data QState = QS {boardSize :: Size, stack::Stack, solNum::Unsigned 8} deriving(Eq, Show)

initQState iSize = QS iSize (def <~~ (def, QV indexVec iSize)) 0


queenStateM :: QState -> Size -> (QState, Out)
queenStateM qst@(QS boardSize stack solNum) newSize
  | boardSize /= newSize = (initQState newSize, Out Nothing False 0)
  | len stack == 0       = (qst, Out Nothing True solNum)
  | otherwise            = (QS boardSize stack' solNum', out)
      where (qs,ps) = topEle stack
            rest    = pop    stack
            qs'     = qs <~~ (topEle ps)
            ps'     = hwFilterL (safeAll qs') (QV indexVec boardSize)
            top'    = (qs, pop ps)
            newtop  = (qs', ps')
            solNum'
              | len qs' == boardSize = solNum + 1
              | otherwise            = solNum
            out 
              | len qs' == boardSize = Out (return qs') False solNum'
              | otherwise            = Out Nothing False solNum'
            stack' 
              | len qs' >= boardSize = if (len ps <= 1)  then (rest         ) else (rest <~~ top')   -- find a valid solution
              | len ps <= 1          = if (len ps' == 0) then (rest         ) else (rest <~~ newtop) -- find a partial configuration, and no more ps
              | otherwise            = if (len ps' == 0) then (rest <~~ top') else (rest <~~ top' <~~ newtop) -- find a partial configuration, and thera're still some elements in ps
            -- out 
            --   | len qs' == (boardSize-1) && (len ps' == 1) = Out (Just (qs' <~~ (topEle ps'))) False
            --   | otherwise = Out Nothing False
            -- stack' 
            --   | len qs' == (boardSize-1) && len ps == 1                 = rest
            --   | len qs' == (boardSize-1) && len ps >  1                 = rest <~~ top'
            --   | len qs' <  (boardSize-1) && len ps == 1 && len ps' == 0 = rest
            --   | len qs' <  (boardSize-1) && len ps == 1 && len ps' >  0 = rest <~~ newtop
            --   | len qs' <  (boardSize-1) && len ps >  1 && len ps' == 0 = rest <~~ top'
            --   | len qs' <  (boardSize-1) && len ps >  1 && len ps' >  0 = rest <~~ top' <~~ newtop
            --   | otherwise = error $ show (qs', ps, ps')




queens = queenStateM `mealy` (initQState 0)


-- data Out = Out {
--     solution :: Maybe (QVec QInt)
--     , finish :: Bool
--     , counter :: Unsigned 8
-- } deriving(Eq, Show)


-- {-# ANN topEntity
--   (defTop
--      { t_name = "Queen"
--      , t_outputs = [ "finished"

--                    , "digit1"
--                    , "digit2"
--                    , "digit3"
--                    , "digit4"
--                    , "digit5"

--                    , "solution_number"] }) #-}

topEntity :: Signal (Vec MaxSize SegDisp)
topEntity = fetchOut $ queens (signal 5)
-- topEntity :: Signal Size -> Signal Out
-- topEntity = (fmap trans $ fetchOut $ queens size) $ (signal 5)


fetchOut :: Signal Out -> Signal (Vec MaxSize SegDisp)
fetchOut = fetchSM `mealy` (segEncoder def)

fetchSM :: (Vec MaxSize SegDisp) -> Out -> (Vec MaxSize SegDisp, Vec MaxSize SegDisp)
fetchSM state (Out _ True _)     = (state, state)
fetchSM state (Out Nothing _ _)  = (state, state)
fetchSM state (Out (Just v) _ _) = (seg,seg)
    where seg = segEncoder v

segEncoder :: QVec QInt -> Vec MaxSize SegDisp
segEncoder (QV vec _) = map encode vec
  where encode n 
          | n == 0    = 0b11111100
          | n == 1    = 0b01100000
          | n == 2    = 0b11011010
          | n == 3    = 0b11110010
          | n == 4    = 0b01100110
          | n == 5    = 0b10110110
          | n == 6    = 0b10111110
          | n == 7    = 0b11100000
          | otherwise = 0b11111100 -- 0

-- samp n = sampleN n $ topEntity 
-- fuck n = mapM_ print $ filter ((/= Nothing) . solution) $ sampleN n $ topEntity 
-- suck n = mapM_ print $ sampleN n $ topEntity 
-- duck n = mapM_ (\i->print "") $ sampleN n $ topEntity 
