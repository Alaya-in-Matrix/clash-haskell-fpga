module Queen where

import CLaSH.Prelude
import QInt
import QVec
import Display


safeAll :: QVec QInt -> QInt -> Bool
safeAll qv@(QV qlist qlen) p = foldl (&&) True $ zipped
  where zipped :: Vec MaxSize Bool
        zipped = zipWith (safe qlen p) qlist indexVec

safe :: Size -> QInt -> QInt -> QInt -> Bool
safe qlen p q idx  = (idx > qlen) || (p /= q && (delta p q) /= (qlen - idx + 1))
  where delta a b = max a b - min a b

type Stack  = QVec (QVec QInt, QVec QInt)
data QState = QS {
    boardSize :: Maybe Size
    , stack::Stack
    , flag :: Bool
} deriving(Eq, Show)
data QOut   = QOut {
    solution  :: Maybe (Vec MaxSize QInt)
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

-- moore :: (s -> i -> s) -> (s -> o) -> s -> Signal i -> Signal o
-- if flag == True, that means we have finished searching
queenMooreS :: QState -> QIn -> QState
queenMooreS qst@QS{flag = True} _ = def{flag=True} -- finished
queenMooreS qst@QS{boardSize = Nothing, flag = False} Nothing  = def
queenMooreS qst@QS{boardSize = Nothing, flag = False} (Just s) = QS{boardSize = Just s
                                                                   ,stack     = (def <~~ (def,QV indexVec s))
                                                                   , flag     = False }
queenMooreS qst@(QS (Just bSz) stack False) _ 
    | isEmpty stack = def{flag=True} -- finished
    | otherwise     =
        let (qs, ps) = top stack
            rest     = pop stack
            qs'      = qs <~~ (top ps)
            ps'      = hwFilterL (safeAll qs') (QV indexVec bSz)
            top'     = (qs,pop ps)
            newtop   = (qs', ps')
            stack' 
              | len qs' == bSz && (len ps == 1) = rest
              | len qs' == bSz && (len ps >  1) = rest <~~ top'
              | len qs' <  bSz && (len ps == 1) && len ps' == 0 = rest
              | len qs' <  bSz && (len ps == 1) && len ps' >  0 = rest <~~ newtop
              | len qs' <  bSz && (len ps >  1) && len ps' == 0 = rest <~~ top'
              | len qs' <  bSz && (len ps >  1) && len ps' >  0 = rest <~~ top' <~~ newtop
           in qst{stack = stack'}
queenMooreO :: QState -> QOut
queenMooreO qst@(QS _         _ True)  = def{flagOut = True} -- finished
queenMooreO qst@(QS Nothing   _ False) = def
queenMooreO qst@(QS (Just bs) s False)
    | isEmpty s = def
    | otherwise =
        let (qs, ps) = top s
         in if (len qs == (bs - 1)) 
               then def{solution = (list <$> Just (qs <~~ (top ps)))}
               else def

queensMoore = moore queenMooreS queenMooreO def


-- After "Reset" is pressed, qsm waits for input, if input is Nothing, then continue waiting
-- once the input is (Just size), then initialize state, and ignore further input
queenMealyM :: QState -> QIn -> (QState, QOut)
queenMealyM qs@(QS _       _  True)  _        = (def{flag=True},  def{flagOut=True}) -- We got errors!
queenMealyM qs@(QS Nothing _  False) Nothing  = (def,def)                            -- waiting
queenMealyM qs@(QS Nothing _  False) (Just s) = (initState, def)                     -- user set boardSize
  where initState = def{boardSize = Just s, stack = (def <~~ (def, QV indexVec s))}
queenMealyM qs@(QS (Just bSz) stack False) _  
  | len stack == 0 = (def{flag = True}, def{flagOut = True}) -- finished
  | otherwise      =
      let (qs, ps) = top stack
          rest     = pop stack
          qs'      = qs <~~ (top ps)
          ps'      = hwFilterL (safeAll qs') (QV indexVec bSz)
          top'     = (qs,pop ps)
          newtop   = (qs', ps')
          (flag, stack') 
            | len qs' == bSz && (len ps == 1)                   = (False, rest)
            | len qs' == bSz && (len ps >  1)                   = (False, rest <~~ top')
            | len qs' <  bSz && (len ps == 1) && (len ps' == 0) = (False, rest)
            | len qs' <  bSz && (len ps == 1) && (len ps' >  0) = (False, rest <~~ newtop)
            | len qs' <  bSz && (len ps >  1) && (len ps' == 0) = (False, rest <~~ top')
            | len qs' <  bSz && (len ps >  1) && (len ps' >  0) = (False, rest <~~ top' <~~ newtop)
            | otherwise = (True, def)
          out  
            | len qs' == bSz = QOut{solution = Just (list qs'), flagOut = False}
            | otherwise      = QOut{solution = Nothing,  flagOut = False}
          state' = QS (Just bSz) stack' flag
       in (state', out)

queensMealy    = queenMealyM `mealy` def

testIn1 = foldr register (signal (Just 5 :: QIn)) $ replicate d10 Nothing
testIn2 = foldr register (signal Nothing) $ (replicate d5 Nothing) ++ (replicate d4 (Just 5 :: QIn))

testInput = register invalid $ register invalid $ register (True, True, False, True, True) $ signal invalid
    where invalid = (True, True, True, True, True)

topEntity :: Signal (Bool,Bool,Bool,Bool,Bool) -> Signal (Bool, Vec 5 SegDisp)
topEntity input = trans <$> queensMealy (transIn <$> input)
  where trans (QOut Nothing  flag) = (flag,(takeI.segV) (def::Vec MaxSize QInt))
        trans (QOut (Just v) flag) = (flag,(takeI.segV) v)
        transIn :: (Bool,Bool,Bool,Bool,Bool) -> QIn
        transIn (b4, b5, b6, b7, b8) = case (b4, b5, b6, b7, b8) of
                                         (False, True, True, True, True) -> Just 4
                                         (True, False, True, True, True) -> Just 5
                                         (True, True, False, True, True) -> Just 6
                                         (True, True, True, False, True) -> Just 7
                                         (True, True, True, True, False) -> Just 8
                                         _                               -> Nothing 

fuck n = mapM_ print $ filter pred $ sampleN n $ topEntity testInput
  where pred (b,sol) = sol /= (repeat 63)

-- topEntity = trans <$> queensMealy testIn2
--     where trans :: QOut -> (Bool, Vec MaxSize SegDisp)
--           trans (QOut Nothing  err) = (err, segV (def::Vec MaxSize QInt))
--           trans (QOut (Just v) err) = (err, segV v)
