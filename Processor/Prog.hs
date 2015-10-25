module Prog where
import CLaSH.Prelude
type Addr    = Unsigned 16
type IntData = Unsigned 16
data Instr   = Push IntData
             | Duplicate
             | Swap
             | Pop
             | Add
             | Sub
             | Mul
             | Div
             | Mod
             | OutChar
             | OutInt
             | ReadChar
             | ReadInt
             | Jump Addr
             | JumpZero Addr
             | JumpNeg Addr
             | ProgEnd deriving(Eq, Show)

data DisplayType = CharOut | IntOut deriving(Eq, Show)

data LCD_status  = Idle | Busy | Success deriving(Eq, Show)

type StackSize   = 100

data CoreInput = In {
    instruction     :: Instr
    , keyBoardInput :: Maybe IntData
    , lcdStatus     :: LCD_status
} deriving(Eq,Show)

data CoreOutput = Out {
    addr :: Addr
    , lcd_display :: Maybe (IntData, DisplayType)
} deriving(Eq,Show)

data CoreState = State { -- won't check for stackoverflow
    stack :: Vec StackSize IntData
    , top :: IntData
    , pc  :: Addr
    , delayCounter :: Unsigned 4 -- whether core is waiting for memory
    , display :: Maybe (IntData, DisplayType)
} deriving(Eq, Show)


