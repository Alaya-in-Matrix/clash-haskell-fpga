module Queen where
boardSize = 8 :: Int

safe p q d = p /= q && abs (p-q) /= d

safeAll qs p = foldl (&&) True $ zipWith (safe p) qs ds
  where n  = length qs
        ds = reverse $ [1..n]

(<.)::[a]->a->[a]
(<.) xs x = xs ++ [x]

itn f a 0 = a
itn f a n = itn f (f a) (n-1)



-- trivial version
queens1 0 = [[]]
queens1 n = [qs <. p |  qs <- queens1 (n-1) , p <- [1..boardSize], safeAll qs p]

-- Removing explicit list :comprehension
extentions qs = map (qs <.) $ filter (safeAll qs) [1..boardSize]

queens2 0 = [[]]
queens2 n = concat $ map extentions $ queens2 (n-1)

-- Removing explicit recursion
queens3 n = itn (concat . map extentions) [[]] n



-- fixed length list
-- two filter function that could be implemented by hardware

hwFilter  pred xs = zip xs $ map pred xs

hwFilterL pred xs =
    let yes = filter pred xs
        no  = filter (not . pred) xs
     in (yes ++ no, length yes)

safeF p q d = d <= 0 || (p /= q && abs (p-q) /= d)

safeFAll (qs,n) p = foldl (&&) True $ zipWith (safeF p) qs ds
  where ds = reverse [n-boardSize+1..n]

(<..) :: ([Int],Int)->(Int,Bool)->([Int],Int)
(qs,n) <.. (val, False) = (qs,-1)
(qs,n) <.. (val, True)  = (replace n val qs, (n+1))

replace idx val xs 
  | idx >= length xs = error $ "replace " ++ show idx ++ " " ++ show val ++ " " ++ show xs
  | otherwise        = take (idx) xs ++ [val] ++ drop (idx+1) xs

extentionsF (qs,n) = map ((qs,n) <..) $ hwFilter (safeFAll (qs,n)) [1..boardSize]
queens4 n          = itn (concat . map extentionsF) [(replicate boardSize 0,0)] n

f = map fst $ filter ((== boardSize) . snd) $ queens4 boardSize


sim :: (s->i->(s,o)) -> s -> [i] -> [o]
sim f s [] = []
sim f s (i:is) = z : sim f s' is
    where (s',z) = f s i

queensM1 s _ = (f s, s)
    where f = concat . map extentionsF

testM1 = sim queensM1 initState [0..boardSize]
    where initState = [(replicate boardSize 0, 0)]


type Stack = [([Int],[Int])]
queensMh :: Stack -> i -> (Stack, Maybe [Int])
queensMh []         _ = ([],Nothing)
queensMh (top:rest) _ = (stack', out)
    where 
        (qs, ps) = top -- qs ++ ps make a partial configuration
        (n,  m)  = (length qs, length ps)
        qs'      = qs <. (head ps) :: [Int]
        ps'      = filter (safeAll qs') [1..boardSize] :: [Int]
        (n', m') = (length qs', length ps')
        top'     = (qs, tail ps)
        nexttop  = (qs', ps')
        stack' 
          | n' == boardSize - 1 && m == 1            = rest                     -- all solutions based on qs is found, pop top out
          | n' == boardSize - 1 && m >  1            = top' : rest              -- all solutions based on (qs <. (head ps)) is  founc, remove (head ps)
          | n' <  boardSize - 1 && m == 1 && m' == 0 = rest                     -- no solutions  based on qs', so, pop top out
          | n' <  boardSize - 1 && m == 1 && m' >  0 = nexttop : rest           -- solutions based on (qs,ps) is equal to solutions based on (qs' ps')
          | n' <  boardSize - 1 && m >  1 && m' == 0 = top': rest               -- no solutions based on qs', but qs is not empty, continue searching
          | n' <  boardSize - 1 && m >  1 && m' >  0 = nexttop : top' : rest    -- some possibly new solutions
        out 
          | n' == boardSize - 1 && m' == 1  = Just $ qs' ++ ps'
          | otherwise                       = Nothing

testMh = filter (/= Nothing) $ sim queensMh initStack [1..50000]
    where initStack = [([],[1..boardSize])]
