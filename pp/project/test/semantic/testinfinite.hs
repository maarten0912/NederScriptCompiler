import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Load (ImmValue (1)) (2)
    , Push (2)
    , Pop (2)
    , Store (2) (DirAddr (1))
    , Load (DirAddr (1)) (2)
    , Push (2)
    , Load (ImmValue (0)) (2)
    , Push (2)
    , Pop (3)
    , Pop (2)
    , Compute (Gt) (2) (3) (4)
    , Push (4)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Compute (Sub) (3) (2) (2)
    , Branch (2) (Rel (6))
    , Load (ImmValue (0)) (2)
    , Push (2)
    , Pop (2)
    , Store (2) (DirAddr (2))
    , Jump (Rel (-16))
    , EndProg
    ]

main = run [prog]