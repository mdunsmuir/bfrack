import qualified Grid as G

data Direction = Up | Down | Left | Right deriving Show
data Instruction = Add | Subtract | Multiply | Divide | Modulo
                 | Not | Greater
                 | Move Direction | Random | Trampoline
                 | Pop Direction Direction | Discard | Duplicate | Swap
                 | StringDelim
                 | Put | Get | IntInput | CharInput | IntOutput | CharOutput
                 | Halt
                 deriving Show
