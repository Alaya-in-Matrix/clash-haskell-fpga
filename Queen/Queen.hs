module Queen where

import CLaSH.Prelude
import QVec
import SegLED

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

type Stack  = QVec (QVec QInt, QVec QInt)
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




queens = queenStateM `mealy` (initQState 0)

topEntity :: Signal (Vec MaxSize SegDisp)
topEntity = fetchOut $ queens (signal 5)

fetchOut :: Signal Out -> Signal (Vec MaxSize SegDisp)
fetchOut = fetchSM `mealy` def

fetchSM :: (Vec MaxSize QInt) -> Out -> (Vec MaxSize QInt, Vec MaxSize SegDisp)
fetchSM state (Out (Just v) False    _) = (vec,    segV vec)
    where vec = list v
fetchSM state _ = (state, segV state)
