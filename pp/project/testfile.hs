import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Debug ("Beginning program!!!")
    , ReadInstr (DirAddr (65536))
    , Receive (6)
    , Load (ImmValue (0)) (2)
    , Load (ImmValue (1)) (3)
    , Compute (Gt) (2) (6) (4)
    , Branch (4) (Abs (13))
    , WriteInstr (2) (DirAddr (65536))
    , Compute (Add) (2) (3) (2)
    , Compute (Gt) (3) (6) (4)
    , Branch (4) (Abs (13))
    , WriteInstr (3) (DirAddr (65536))
    , Compute (Add) (2) (3) (3)
    , Jump (Rel (-8))
    , EndProg
    ]

main = run [prog,prog]