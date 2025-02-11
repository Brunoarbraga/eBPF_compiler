{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}

module Compiler.Type_Definitions where

import Control.Monad.Except
import Control.Monad.State
import Ebpf.Asm
import Data.Int (Int64)

--Environments: Represent the memory and all the registers, respectively
type MemoryEnv = [Int64]
type VarEnv = [Reg]

data Env = Env {
    instructionNumber :: Int64 ,
    instructionList :: [Instruction] ,
    memoryEnv :: [Int64] ,
    registers :: [Int64] 
} 

--Monads
type InterpM a = ExceptT String (StateT Env IO) a

-- Monad interpreter function
runInterp :: InterpM a -> Env -> IO (Either String a, Env)
runInterp ev st = runStateT (runExceptT ev) st

