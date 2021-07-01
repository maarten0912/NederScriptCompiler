import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Branch (1) (Rel (4))
    , Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Jump (Rel (8))
    , ReadInstr (IndAddr (1))
    , Receive (2)
    , Compute (Equal) (2) (0) (3)
    , Branch (3) (Rel (-3))
    , Load (ImmValue (10)) (3)
    , Compute (Add) (2) (3) (3)
    , Jump (Ind (3))
    , Load (ImmValue (8)) (2)
    , Push (2)
    , Load (ImmValue (7)) (2)
    , Push (2)
    , Load (ImmValue (6)) (2)
    , Push (2)
    , Load (ImmValue (5)) (2)
    , Push (2)
    , Load (ImmValue (4)) (2)
    , Push (2)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Compute (Incr) (2) (0) (4)
    , Store (4) (IndAddr (3))
    , Compute (Incr) (3) (0) (3)
    , Jump (Rel (5))
    , Pop (4)
    , Store (4) (IndAddr (3))
    , Compute (Incr) (3) (0) (3)
    , Compute (Decr) (2) (0) (2)
    , Branch (2) (Rel (-4))
    , Load (ImmValue (2)) (2)
    , Push (2)
    , Pop (2)
    , Store (2) (DirAddr (6))
    , Load (ImmValue (1)) (2)
    , Load (DirAddr (6)) (3)
    , Compute (Add) (2) (3) (2)
    , Compute (Incr) (2) (0) (2)
    , Load (IndAddr (2)) (3)
    , Push (3)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    ]

main = run [prog]