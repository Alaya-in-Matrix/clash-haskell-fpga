module Core(coreStateOutTrans) where
import CLaSH.Prelude
import Prog 
import Decoder 
import Debug.Trace

setStack ns    (State s t p d disp) = State ns t p d disp
setTop   nt    (State s t p d disp) = State s nt p d disp
setPC    np    (State s t p d disp) = State s t np d disp
setDelay nd    (State s t p d disp) = State s t p nd disp
setDisp  ndisp (State s t p d disp) = State s t p d ndisp

memDelay = 1

initState = State (repeat 0) 0 0 0 Nothing

topEntity :: Signal CoreInput -> Signal CoreOutput
topEntity = (coreStateOutTrans coreStateTrans) `mealy` initState

-- I still need a LCD controller and a keyboard controller
coreStateOutTrans :: (CoreState->CoreInput->CoreState)
                  -> CoreState
                  -> CoreInput
                  -> (CoreState,CoreOutput)
coreStateOutTrans sf s i = let newState = sf s i :: CoreState
                               out      = Out (pc newState) (display newState)
                            in (newState,out)


coreStateTrans :: CoreState -> CoreInput -> CoreState
coreStateTrans st@(State stack top pc delay out) inp@(In instr kbIn ready) = 
    case instr of
      Push n    -> processPush st n
      Duplicate -> processDup  st
      Swap      -> processSwp  st
      Pop       -> processPop  st

      Add -> processArith (+) st
      Sub -> processArith (-) st
      Mul -> processArith (*) st
      Div -> processArith div st
      Mod -> processArith mod st

      OutChar  -> processOut CharOut st ready
      OutInt   -> processOut IntOut  st ready
      ReadChar -> processIn CharOut  st kbIn
      ReadInt  -> processIn IntOut   st kbIn

      Jump     addr -> processJump (\t->True) addr st
      JumpZero addr -> processJump (== 0)     addr st
      JumpNeg  addr -> processJump (<  0)     addr st

      ProgEnd -> st
-- delay > 0，说明instruction的功能已经执行完毕，进入访存阶段
processPush :: CoreState -> IntData -> CoreState
processPush st@(State stack top pc delay _) num 
    | delay > 0 = State stack top pc (delay-1) Nothing  -- Waiting memory
    | otherwise = 
        let newStack = replace (top+1) num stack
            newTop   = top + 1
            newPC    = pc  + 1
         in State newStack newTop newPC memDelay Nothing

processDup :: CoreState -> CoreState
processDup st@(State stack top pc delay _) 
    | delay > 0 = State stack top pc (delay-1) Nothing
    | otherwise = State newStack (top+1) (pc+1) memDelay Nothing
        where newStack = replace (top+1) (stack !! top) stack
         
processSwp ::CoreState -> CoreState
processSwp st@(State stack top pc delay _)
    | delay > 0 = State stack top pc (delay-1) Nothing
    | otherwise = State newStack top (pc+1) memDelay Nothing
        where newStack = let e1 = stack !! top
                             e2 = stack !! (top-1)
                          in replace top e2 $ replace (top-1) e1 stack

processPop :: CoreState -> CoreState
processPop st@(State stack top pc delay _)
    | delay > 0 = setDelay (delay-1) st
    | otherwise = State stack (top-1) (pc+1) memDelay  Nothing
                
processArith :: (IntData->IntData->IntData) -> CoreState -> CoreState
processArith op st@(State stack top pc delay _) 
    | delay > 0 = setDelay (delay-1) st
    | otherwise = let e1       = stack !! top
                      e2       = stack !! (top-1)
                      newStack = replace (top-1) (e1 `op` e2) stack
                   in State newStack (top-1) (pc+1) memDelay Nothing


-- LCD controller 在 data 从 Nothing 变到 Just _ 时，把 status 设为 Busy，开始工作
-- 工作完成后，把 status 设置为 Success，当 data 变回 Nothing 后，把 status 设为 Idle
-- LCD 的输入为Nothing时，状态为Idle, 输入从Nothing变为Just时，状态立即变为Busy，并开始
-- 工作，工作过程中，要求输入保持不变，工作完成后，状态变为Success，当输入又变为Nothing时，
-- LCD状态变回Idle
processOut :: DisplayType->CoreState->LCD_status->CoreState
processOut dt st@(State stack top pc delay disp) lcdS 
    | delay > 0 = State stack top pc (delay-1) Nothing
    | otherwise = case lcdS of
                    Idle    -> (State stack top pc 0 (Just (stack !! top, dt)))
                    Busy    -> st
                    Success -> (State stack top pc memDelay Nothing)

-- 目前就处理整数？
processIn :: DisplayType->CoreState->(Maybe IntData)->CoreState
processIn _ st@(State stack top pc delay _) keyboard
    | delay > 0 = State stack top pc (delay-1) Nothing
    | otherwise = case keyboard of
                    Nothing -> st
                    Just v  -> let newStack = replace (top+1) v stack
                                in State newStack (top+1) (pc+1) memDelay Nothing

processJump :: (IntData->Bool) -> Addr -> CoreState -> CoreState
processJump pred addr st@(State stack top pc delay disp)
    | delay > 0 = State stack top pc (delay-1) Nothing
    | otherwise = State stack top newPC memDelay Nothing
        where newPC = if (pred $ stack !! top) then addr else (pc+1)
