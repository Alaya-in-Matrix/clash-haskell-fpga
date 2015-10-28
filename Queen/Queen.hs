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

type Stack  = QVec (QVec QInt, QVec QInt)
data QState = QS {
    boardSize :: Maybe Size
    , stack::Stack
    , flag :: Bool
} deriving(Eq, Show)
data QOut   = QOut {
    solution  :: Maybe (QVec QInt)
    , flagOut  :: Bool
} deriving(Eq, Show)
type QIn = Maybe Size

instance Default QState where
    def = QS { boardSize = Nothing
             , stack   = def
             , flag = False 
             }
instance Default QOut where
    def = QOut { solution  = Nothing
               , flagOut  = False 
               }


-- After "Reset" is pressed, qsm waits for input, if input is Nothing, then continue waiting
-- once the input is (Just size), then initialize state, and ignore further input
queenMealyM :: QState -> QIn -> (QState, QOut)
queenMealyM qs@(QS _       _  True)  _        = (def{flag=True},  def{flagOut=True}) -- We got errors!
queenMealyM qs@(QS Nothing _  False) Nothing  = (def,def)
queenMealyM qs@(QS Nothing _  False) (Just s) = (initState, def)
  where initState = def{boardSize = Just s, stack = (def <~~ (def, QV indexVec s))}
queenMealyM qs@(QS (Just bSz) stack False) _  
  | len stack == 0 = (def, def) -- finished
  | otherwise      =
      let (qs, ps) = top stack
          rest     = pop stack
          qs'      = qs <~~ (top ps)
          ps'      = hwFilterL (safeAll qs') (QV indexVec bSz)
          top'     = (qs,pop ps)
          newtop   = (qs', ps')
          (err, stack') 
            | len qs' == bSz && (len ps == 1)                   = (False, rest)
            | len qs' == bSz && (len ps >  1)                   = (False, rest <~~ top')
            | len qs' <  bSz && (len ps == 1) && (len ps' == 0) = (False, rest)
            | len qs' <  bSz && (len ps == 1) && (len ps' >  0) = (False, rest <~~ newtop)
            | len qs' <  bSz && (len ps >  1) && (len ps' == 0) = (False, rest <~~ top')
            | len qs' <  bSz && (len ps >  1) && (len ps' >  0) = (False, rest <~~ top' <~~ newtop)
            | otherwise = (True, def)
          out  
            | len qs' == bSz = QOut{solution = Just qs', flagOut = err}
            | otherwise      = QOut{solution = Nothing,  flagOut = err}
          state' = QS (Just bSz) stack' err
       in (state', out)

queens    = queenMealyM `mealy` def
testIn    = foldr register (signal (Just 4 :: QIn)) $ replicate d10 Nothing
topEntity = trans <$> queens testIn
    where trans :: QOut -> (Bool, Vec MaxSize SegDisp)
          trans (QOut Nothing  err) = (err, segV (def::Vec MaxSize QInt))
          trans (QOut (Just v) err) = (err, segV $ list v)

