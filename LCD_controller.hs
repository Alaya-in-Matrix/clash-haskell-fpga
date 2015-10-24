module LCD_controller(lcdCtrl) where
import CLaSH.Prelude
import Prog 

-- connect to LCD
data LCD_interface_out

-- data from LCD
data LCD_interface_in

-- data LCD_status  = Idle | Busy | Success deriving(Eq, Show)
-- data DisplayType = CharOut | IntOut deriving(Eq, Show)

-- LCD controller 在 data 从 Nothing 变到 Just _ 时，把 status 设为 Busy，开始工作
-- 工作完成后，把 status 设置为 Success，当 data 变回 Nothing 后，把 status 设为 Idle
-- LCD 的输入为Nothing时，状态为Idle, 输入从Nothing变为Just时，状态立即变为Busy，并开始
-- 工作，工作过程中，要求输入保持不变，工作完成后，状态变为Success，当输入又变为Nothing时，
-- LCD状态变回Idle


-- data LCD_state = LS {
--     status :: LCD_status
--     , data :: IntData
--     } deriving(Eq, Show)
-- lcdCtrl :: Signal (Maybe (IntData, DisplayType)) -> Signal LCD_interface_in -> Signal (LCD_interface_out, LCD_status)
-- lcdCtrl = undefined

-- lcdStateTrans :: LCD_state -> LCD
