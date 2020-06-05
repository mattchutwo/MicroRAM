module MicroRAM.MRAMutil where

import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter
import Data.Sequence as Seq

  -- defaultMain (testGroup "Our Library Tests" testSuite) -- testSuit defined at eof
k = 5 -- 5 registers

-- We are treating the first register as the return
-- To get ouptu to get output provide a program and a number of steps to get the result after that long
-- Execute gets the trace and then looks at the first register after t steps
get_trace prog input advice = run k input advice prog
exec prog input advice steps = Seq.index (see (get_trace prog input advice) steps) 0 -- this throws an error if there is no register 0
simpl_exec prog steps = exec prog [] [] steps -- when there are no inputs

-- The tester setup

get_regs :: State -> Regs
get_regs = regs

see:: Trace -> Int -> Regs
see t n = regs (t !! n)

reg_trace::Trace -> [Regs]
reg_trace t= map regs t

pc_trace::Trace -> [Wrd]
pc_trace t= map pc t

flag_trace::Trace -> [Bool]
flag_trace t= map flag t


fib = [Iadd 0 1 (Const 1), Iadd 2 0 (Const 0), Iadd 0 0 (Reg 1), Iadd 1 2 (Const 0), Ijmp (Const 1)]
