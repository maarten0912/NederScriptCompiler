{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module FP_Core where

import GHC.Generics
import FPPrac.Trees

{-
Extension of CoreIntro.hs:
- instructions as program *in* the processor,
- stack now is list of fixed length,i
- clock added
- program counter + stack pointer added,
- instruction EndProg added,
- update operaton (<~) added,
-}

-- ========================================================================

type Stack  = [Int]

type Heap = [Int]

data Op     = Add | Mul | Sub
            deriving (Show, Generic, ToRoseTree)




data Tick = Tick

-- ========================================================================
-- Processor functions

xs <~ (i,a) = take i xs ++ [a] ++ drop (i+1) xs
                -- Put value a on position i in list xs

alu op = case op of
                Add -> (+)
                Mul -> (*)
                Sub -> (-)


-- ========================================================================
-- example Program for expression: (((2*10) + (3*(4+11))) * (12+5))

-- Tree of this expression of type Expr (result of parsing):
expr = BinExpr Mul
          (BinExpr Add
              (BinExpr Mul
                  (Const 2)
                  (Const 10))
              (BinExpr Mul
                  (Const 3)
                  (BinExpr Add
                      (Const 4)
                      (Const 11))))
          (BinExpr Add
              (Const 12)
              (Const 5))

-- The program that results in the value of the expression (1105):
prog = [ PushConst 2
       , PushConst 10
       , Calc Mul
       , PushConst 3
       , PushConst 4
       , PushConst 11
       , Calc Add
       , Calc Mul
       , Calc Add
       , PushConst 12
       , PushConst 5
       , Calc Add
       , Calc Mul
       , EndProg
       ]

-- Testing
clock      = repeat Tick
emptyStack = replicate 8 0
emptyHeap  = replicate 8 0
test       = putStr
           $ unlines
           $ map show
           $ takeWhile (\(pc,_,_,_) -> pc /= -1)
           $ scanl (core prog) (0,0,emptyHeap,emptyStack) clock


--4-FP.2
codeGenExpr :: Expr -> [Instr]
codeGenExpr (Const c) = [PushConst c]
codeGenExpr (BinExpr op ex1 ex2) = (codeGen ex1) ++ (codeGen ex2) ++ [Calc op] 
codeGenExpr (Variable n) = [PushAddr n]

--4-FP.4

type Variable = Int

class CodeGen a where
    codeGen :: a -> [Instr]

instance CodeGen Expr where
    codeGen e = codeGenExpr e

instance CodeGen Stmnt where
    codeGen e = codeGenStmnt e

compile :: [Stmnt] -> [Instr]
compile xs = (concat $ codeGen <$> xs) ++ [EndProg]



core :: [Instr] -> (Int,Int,Heap,Stack) -> Tick -> (Int,Int,Heap,Stack)
core instrs (pc,sp,heap,stack) tick =  case instrs!!pc of
        PushConst n   -> (pc+1, sp+1 , heap , stack <~ (sp,n))

        Calc op  -> (pc+1, sp-1, heap, stack <~ (sp-2,v))
                 where
                   v = alu op (stack!!(sp-2)) (stack!!(sp-1))

        PushAddr n -> (pc+1, sp+1, heap, stack <~ (sp, heap!!n))

        Store n -> (pc+1, sp-1, heap <~ (n, stack!!(sp-1)), stack)

        PushPC   -> (pc+1, sp+1 , heap , stack <~ (sp,pc))

        EndRep ->  if stack!!(sp-2) == 1 then (pc+1, sp-2, heap, stack) else (stack!!(sp-1) + 1, sp, heap, stack <~ (sp-2, stack!!(sp-2) - 1))

        EndProg  -> (-1, sp, heap, stack)


codeGenStmnt :: Stmnt -> [Instr]
codeGenStmnt (Assign n e) = codeGen e ++ [Store n]
codeGenStmnt (Repeat e xs) = codeGen e ++ [PushPC] ++ (concat $ codeGen <$> xs) ++ [EndRep]

data Stmnt = Assign Variable Expr
           | Repeat Expr [Stmnt]
            deriving (Show, Generic, ToRoseTree)

data Expr = Const Int                   -- for constants
          | BinExpr Op Expr Expr        -- for ``binary expressions''
          | Variable Variable
            deriving (Show, Generic, ToRoseTree)

data Instr  = PushConst Int
            | Calc Op
            | EndProg
            | PushAddr Int
            | Store Int
            | PushPC
            | EndRep
            deriving (Show, Generic, ToRoseTree)


counterProgram :: [Stmnt]
counterProgram = [
    Assign 0 (Const 0),
    Assign 1 (Const 0),
    Repeat (Const 10) [
        Assign 1 (BinExpr Add (Variable 1) (Const 1)),
        Assign 0 (BinExpr Add (Variable 0) (Variable 1))
        ]
    ]

-- R0 = 0
-- R1 = 0
-- loop 10 {
--     R0 <- R0 + 1
--     R1 <- R0 + R1
-- }


compiled = compile counterProgram

runProgram inputProgram = putStr
                $ unlines
                $ map show
                $ takeWhile (\(pc,_,_,_) -> pc /= -1)
                $ scanl (core inputProgram) (0,0,emptyHeap,emptyStack) clock
                where
                        clock      = repeat Tick
                        emptyStack = replicate 15 0
                        emptyHeap  = replicate 15 0
   