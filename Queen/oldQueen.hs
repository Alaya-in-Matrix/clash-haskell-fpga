module OldQueen where

import CLaSH.Prelude

type MaxSize = 5
boardSize    = 5:: IntData
type IntData = (Signed 4)
type QNbr     = Signed 4
type QVec a   = Vec MaxSize a
type StackElm = ( QVec QNbr
                , QNbr
                , QVec QNbr
                , QNbr
                , QNbr)
type Stack   = (IntData, Vec MaxSize StackElm)
type SegDisp = Unsigned 8
data Cmd = Run | Stop deriving(Eq, Show)
data Out = Out {
    solution :: Maybe (QVec QNbr)
    , finish :: Bool
} deriving(Eq, Show)


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



topEntity = fmap trans $ fetchOut $ queens $ signal Run
    where trans :: QVec QNbr -> QVec SegDisp
          trans = map segDecoder
queens    = queensM `mealy` initStack

fetchOut :: Signal Out -> Signal (QVec QNbr)
fetchOut = fetchSM `mealy` def


-- data Out = Out {
--     solution :: Maybe (QVec QNbr)
--     , finish :: Bool
-- } deriving(Eq, Show)

fetchSM :: QVec QNbr -> Out -> (QVec QNbr, QVec QNbr)
fetchSM state (Out _ True)         = (state, state)
fetchSM state (Out Nothing  False) = (state, state)
fetchSM state (Out (Just v) False) = (v,v)

segDecoder :: QNbr -> SegDisp
segDecoder n 
  | n == 0    = 0b11111100
  | n == 1    = 0b01100000
  | n == 2    = 0b11011010
  | n == 3    = 0b11110010
  | n == 4    = 0b01100110
  | n == 5    = 0b10110110
  | n == 6    = 0b10111110
  | n == 7    = 0b11100000
  | otherwise = 0

-- data RecvState  = RS {
--     isFinish :: Bool                -- whether finished
--     , freqDivid :: (Unsigned 10)    -- factor to divide clock
--     , solNum :: IntData             -- how many solutions are there
--     , dispCounter :: IntData        -- which solution to display
--     , solMem :: Vec 10 (QVec QNbr)  -- mem to store all solutions, 
-- } deriving(Eq, Show)
-- type RecvOut = (Bool, (QVec QNbr), IntData)

-- defRecvOut = (False, def, 0)
-- defFreqDiv = 999


-- recvStateMachine :: RecvState -> Out -> (RecvState, RecvOut)
-- recvStateMachine rs@(RS False fd sn dp sm) rIn@(Out Nothing  False) = (rs,  defRecvOut) -- calc not finished, no new solution
-- recvStateMachine rs@(RS False fd sn dp sm) rIn@(Out (Just s) False) = (rs', defRecvOut) -- calc not finished, find new solution store solutions, add solution counter
--   where rs' = RS False fd (sn+1) 0 (replace sn s sm)
-- recvStateMachine rs@(RS False _  sn _ sm) rIn@(Out _ True) = (rs', out) -- go into display circle
--   where rs' = RS True defFreqDiv sn 0 sm
--         out = (True, (sm !! 0), sn)
-- recvStateMachine rs@(RS True 0  sn dp sm) _ = (rs', out) -- display new solution
--     where rs'   = RS True defFreqDiv sn newDp sm
--           newDp = if (dp == (sn-1)) then 0 else (dp + 1)
--           out   = (True, (sm !! dp), sn)
-- recvStateMachine rs@(RS True fd sn dp sm) _ = (rs', out) 
--     where rs' = RS True (fd-1) sn dp sm
--           out = (True, (sm !! dp), sn)

-- instance Default RecvState where
--     def = RS False defFreqDiv 0 0 def

-- receiver = unbundle . (recvStateMachine `mealy` def)




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

-- topEntity = fmap2nd changeWidth $ receiver solutions 

-- solutions = (queensM `mealy` initStack) (signal Run) :: Signal Out

-- fmap2nd f (a, b, c) = (a, (fmap f b), c)

-- changeWidth :: QVec QNbr -> QVec (Unsigned 8)
-- changeWidth = map (toEnum . fromEnum)

-- f0 (a,b,c) = a
-- f1 (a,b,c) = b
-- f2 (a,b,c) = c

-- testInput :: Signal Cmd
-- testInput = signal Run

-- samp sampNum = sampleN sampNum $ topEntity 

-- fuck n = mapM_ print $ filter ((/= Nothing).solution) $ samp n

-- suck n = mapM_ print $ samp n
