import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Branch (regSprID) (Rel (4))
    , Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Jump (Rel (6))
    , Load (ImmValue (1)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    , Load (ImmValue (2)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    ]

main = run [prog,prog]