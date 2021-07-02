import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Branch (1) (Rel (4))
    , Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Jump (Rel (18))
    , ReadInstr (IndAddr (1))
    , Receive (2)
    , Compute (Equal) (2) (0) (3)
    , Branch (3) (Rel (-3))
    , Load (ImmValue (10)) (3)
    , Compute (Add) (2) (3) (3)
    , Jump (Ind (3))
    , Load (ImmValue (1)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (6))
    , ReadInstr (DirAddr (5))
    , Receive (3)
    , Load (ImmValue (1)) (4)
    , Compute (Sub) (3) (4) (3)
    , WriteInstr (3) (DirAddr (5))
    , EndProg
    , ReadInstr (DirAddr (5))
    , Receive (3)
    , Load (ImmValue (1)) (4)
    , Compute (Add) (3) (4) (3)
    , WriteInstr (3) (DirAddr (5))
    , Load (ImmValue (1)) (2)
    , WriteInstr (2) (DirAddr (1))
    , ReadInstr (DirAddr (6))
    , Receive (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    ]

main = run [prog,prog]