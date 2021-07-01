import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Branch (1) (Rel (4))
    , Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Jump (Rel (16))
    , ReadInstr (IndAddr (1))
    , Receive (2)
    , Compute (Equal) (2) (0) (3)
    , Branch (3) (Rel (-3))
    , Jump (Ind (2))
    , Load (ImmValue (2)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    , Load (ImmValue (3)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    , Load (ImmValue (1)) (2)
    , WriteInstr (2) (DirAddr (1))
    , Load (ImmValue (1)) (2)
    , WriteInstr (2) (DirAddr (2))
    , Load (ImmValue (1)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    ]

main = run [prog,prog,prog]