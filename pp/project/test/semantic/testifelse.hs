import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Load (ImmValue (0)) (2)
    , Push (2)
    , Pop (2)
    , Store (2) (DirAddr (1))
    , Load (ImmValue (0)) (2)
    , Push (2)
    , Pop (2)
    , Store (2) (DirAddr (2))
    , Load (DirAddr (2)) (2)
    , Push (2)
    , Load (ImmValue (10)) (2)
    , Push (2)
    , Pop (3)
    , Pop (2)
    , Compute (Lt) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Compute (Sub) (3) (2) (2)
    , Branch (2) (Rel (24))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Load (DirAddr (2)) (2)
    , Push (2)
    , Pop (2)
    , Pop (3)
    , Compute (Add) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Store (2) (DirAddr (1))
    , Store (2) (DirAddr (1))
    , Load (DirAddr (2)) (2)
    , Push (2)
    , Load (ImmValue (1)) (2)
    , Push (2)
    , Pop (2)
    , Pop (3)
    , Compute (Add) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Store (2) (DirAddr (2))
    , Store (2) (DirAddr (2))
    , Jump (Rel (-34))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Load (ImmValue (5)) (2)
    , Push (2)
    , Pop (3)
    , Pop (2)
    , Compute (Gt) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Compute (Sub) (3) (2) (2)
    , Branch (2) (Rel (13))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Load (ImmValue (2)) (2)
    , Push (2)
    , Pop (2)
    , Pop (3)
    , Compute (Sub) (3) (2) (4)
    , Push (4)
    , Pop (2)
    , Store (2) (DirAddr (1))
    , Store (2) (DirAddr (1))
    , Jump (Rel (-23))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Load (ImmValue (5)) (2)
    , Push (2)
    , Pop (3)
    , Pop (2)
    , Compute (Equal) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Branch (2) (Rel (2))
    , Jump (Rel (5))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Pop (2)
    , WriteInstr (2) (DirAddr (65536))
    , EndProg
    ]

main = run [prog]