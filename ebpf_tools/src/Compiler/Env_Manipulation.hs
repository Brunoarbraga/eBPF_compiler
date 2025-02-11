module Compiler.Env_Manipulation where

import Compiler.Type_Definitions
import Ebpf.Asm
import Control.Monad.State
import Control.Monad.Except (throwError)
import Data.Int (Int64)


-- increments the current instruction number by a given number, 
-- usefull for jump instruction and instruction fetching
incrementInstructionNumber :: Int64 -> InterpM()
incrementInstructionNumber n = do
    env <- get
    let newNumber = instructionNumber env + n
    put env {instructionNumber = newNumber}

getInstruction :: InterpM Instruction
getInstruction = do
    currentInstruction <- gets instructionNumber
    instructions <- gets instructionList
    return (instructions !! fromIntegral currentInstruction)


getRegVal :: Reg -> InterpM Int64
getRegVal (Reg r) = do
    env <- get
    let regs = registers env  
    if r < length regs
        then return $ regs !! r
        else throwError $ "Register out of bounds: " ++ show r


setRegVal :: Reg -> Int64 -> InterpM ()
setRegVal (Reg r) val = do
    env <- get
    let regs = registers env
    if r < length regs
        then 
            let updatedRegs = take r regs ++ [val] ++ drop (r+1) regs
             in put env { registers = updatedRegs }
        else throwError $ "Register out of bounds: " ++ show r

getMemVal :: Int64 -> InterpM Int64
getMemVal position = do
    env <- get
    let pos = fromIntegral position
    let mem = memoryEnv env  
    if pos < length mem || pos > length mem
        then return $ mem !! pos
        else throwError $ "Invalid memmory position: " ++ show pos

setMemVal :: Int -> Int64 -> InterpM ()
setMemVal pos val = do
    env <- get
    let mem = memoryEnv env
    if pos < length mem || pos > length mem
        then 
            let updatedMem = take pos mem ++ [val] ++ drop (pos+1) mem
             in put env { memoryEnv = updatedMem }
        else throwError $ "Register out of bounds: " ++ show pos

