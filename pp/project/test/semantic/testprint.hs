import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
    Load (ImmValue (1)) (2)
    , Store (2) (DirAddr (0))
    , Load (ImmValue (33)) (2)
    , Push (2)
    , Load (ImmValue (100)) (2)
    , Push (2)
    , Load (ImmValue (108)) (2)
    , Push (2)
    , Load (ImmValue (101)) (2)
    , Push (2)
    , Load (ImmValue (114)) (2)
    , Push (2)
    , Load (ImmValue (101)) (2)
    , Push (2)
    , Load (ImmValue (119)) (2)
    , Push (2)
    , Load (ImmValue (32)) (2)
    , Push (2)
    , Load (ImmValue (111)) (2)
    , Push (2)
    , Load (ImmValue (108)) (2)
    , Push (2)
    , Load (ImmValue (108)) (2)
    , Push (2)
    , Load (ImmValue (97)) (2)
    , Push (2)
    , Load (ImmValue (72)) (2)
    , Push (2)
    , Load (ImmValue (13)) (2)
    , Push (2)
    , Load (ImmValue (83)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (112)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (114)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (111)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (99)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (107)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (101)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (108)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (108)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (32)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (48)) (7)
    , Compute (Add) (7) (1) (7)
    , WriteInstr (7) (DirAddr (65537))
    , Load (ImmValue (32)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (115)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (97)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (121)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (115)) (2)
    , WriteInstr (2) (DirAddr (65537))
    , Load (ImmValue (32)) (2)
    , WriteInstr (2) (DirAddr (65537))
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