import Sprockell 

{- This file was automatically generated from a NederScript file.
NederScript is a custom language created by Maarten Meijer and Pepijn Visser 
-} 

prog :: [Instruction] 
prog = [ 
	ReadInstr (DirAddr (MemAddr 5))
	, Compute (Add) (1) (2) (3)
	, EndProg
	]

main = run [prog]