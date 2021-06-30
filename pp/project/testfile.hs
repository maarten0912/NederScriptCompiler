import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Load (ImmValue (116)) (2)
    , Push (2)
    , Load (ImmValue (115)) (2)
    , Push (2)
    , Load (ImmValue (101)) (2)
    , Push (2)
    , Load (ImmValue (116)) (2)
    , Push (2)
    , Load (ImmValue (4)) (2)
    , Push (2)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Load (ImmValue (1)) (4)
    , Compute (Add) (2) (3) (5)
    , Store (5) (IndAddr (4))
    , Compute (Add) (4) (3) (4)
    , Jump (Rel (5))
    , Pop (5)
    , Store (5) (IndAddr (4))
    , Compute (Add) (4) (3) (4)
    , Compute (Sub) (2) (3) (2)
    , Branch (2) (Rel (-4))
    , Load (ImmValue (1)) (2)
    , Load (IndAddr (2)) (3)
    , Compute (Add) (2) (3) (3)
    , Compute (Decr) (3) (0) (3)
    , Compute (GtE) (2) (3) (4)
    , Branch (4) (Rel (5))
    , Load (IndAddr (3)) (5)
    , Push (5)
    , Compute (Decr) (3) (0) (3)
    , Jump (Rel (-5))
    , Load (IndAddr (3)) (5)
    , Compute (Decr) (5) (0) (5)
    , Push (5)
    , Pop (2)
    , Load (ImmValue (1)) (3)
    , Jump (Rel (4))
    , Pop (4)
    , WriteInstr (4) (DirAddr (65537))
    , Compute (Sub) (2) (3) (2)
    , Branch (2) (Rel (-3))
    , Load (ImmValue (10)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , EndProg
    ]

main = run [prog]