{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}


module Compiler.Evaluators where


-- Imports
import Compiler.Env_Manipulation
import Compiler.Type_Definitions
import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import Control.Monad.Except (throwError)
import Control.Monad.State.Lazy (gets)






import Ebpf.Asm




interpInstruction :: Instruction -> InterpM ()
interpInstruction  (Binary _ Add dst src) = do --TODO: Correct the function to support each Int size (B64, B32...)

    dstVal <- getRegVal dst
    -- Binary OPs can be done with either register or immediate values, so the source could be either
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal + srcVal
    setRegVal dst result


interpInstruction  (Binary _ Sub dst src) = do 

    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal - srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Mul dst src) = do 

    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal * srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Div dst src) = do     

    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal `div` srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Or dst src) = do 


    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal .|. srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ And dst src) = do 


    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal .&. srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Lsh dst src) = do 


    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal `shiftL` fromIntegral srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Rsh dst src) = do 


    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal `shiftR` fromIntegral srcVal
    setRegVal dst result
   

interpInstruction  (Binary _ Mod dst src) = do 


    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal `mod` fromIntegral srcVal
    setRegVal dst result
    

interpInstruction  (Binary _ Xor dst src) = do 

    dstVal <- getRegVal dst
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         


    let result = dstVal `xor` fromIntegral srcVal
    setRegVal dst result
   

interpInstruction  (Binary _ Mov dst src) = do 
    
    srcVal <- case src of
        R reg   -> getRegVal reg      
        Imm imm -> return imm         

    setRegVal dst srcVal



interpInstruction (Unary _ Neg reg) = do
    
    val <- getRegVal reg
    let result = negate val
    setRegVal reg result


interpInstruction (Store _ dst maybeOffset regImm) = do
 
    baseAddress <- getRegVal dst

    let offset = maybe 0 id maybeOffset
    let finalAddress = baseAddress + offset

    valueToStore <- case regImm of
        R regSrc -> getRegVal regSrc      
        Imm imm  -> return imm              

   
    setMemVal (fromIntegral finalAddress) valueToStore   


interpInstruction (Jmp offset) = do
    incrementInstructionNumber offset

interpInstruction (JCond Jeq reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 == val2
        then incrementInstructionNumber offset  
        else return ()

interpInstruction (JCond Jgt reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 > val2
        then incrementInstructionNumber offset  
        else return ()

interpInstruction (JCond Jge reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 >= val2
        then incrementInstructionNumber offset  
        else return ()

interpInstruction (JCond Jlt reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 < val2
        then incrementInstructionNumber offset  
        else return ()

interpInstruction (JCond Jle reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 <= val2
        then incrementInstructionNumber offset  
        else return ()

interpInstruction (JCond Jne reg regimm offset) = do

    val1 <- getRegVal reg

    val2 <- case regimm of
        R r -> getRegVal r  
        Imm imm -> return imm  

  
    if val1 /= val2
        then incrementInstructionNumber offset  
        else return ()


interpInstruction (Load _ dst src memmoryOffset) = do

    baseAddr <- getRegVal src
    
    let addr = case memmoryOffset of
                 Just offset -> baseAddr + fromIntegral offset
                 Nothing     -> baseAddr
    
    value <- getMemVal addr
    
    setRegVal dst value

interpInstruction (LoadImm reg imm) = do
    setRegVal reg (fromIntegral imm)

interpInstruction (LoadAbs _ imm) = do

    value <- getMemVal (fromIntegral imm)
    setRegVal (Reg 0) value -- dando uma pesquisada, o loadabs sempre armazena no R0, checar isso

interpInstruction (LoadInd _ src imm) = do
    -- 
    baseAddr <- getRegVal src
    let addr = baseAddr + fromIntegral imm
    
    value <- getMemVal addr
    
    setRegVal src value

interpInstruction Exit = do
    result <- getRegVal (Reg 0) -- O Exit realmente retorna valor, ou só para a execução? 
    throwError $ "Program exited with result: " ++ show result -- Para a execução

initializeEnv :: [Instruction] -> Env
initializeEnv instr = Env 0 
                          instr 
                          [0,0,0,0]
                          [0,10,20,30,40,50,60,70,80,90,100]

-- exampleInstructions :: [Instruction]
-- exampleInstructions =
--     [
--     Load B64 (Reg 2) (Reg 1) (Just 0),       
--     Binary B64 Mov (Reg 1) (Imm 42),
--     Store B64 (Reg 3) (Just 0) (R (Reg 1)),
--     Jmp 1,
--     Binary B64 Mov (Reg 1) (Imm 10000000),
--     JCond Jeq (Reg 1) (Imm 42) 2,
--     Binary B64 Mov (Reg 1) (Imm 10000000),
--     Binary B64 Mov (Reg 1) (Imm 10000000)
--     ]

runInterpreter :: InterpM ()
runInterpreter = do
    instructionNum <- gets instructionNumber
    instructions <- gets instructionList
    let currentInstruction = (instructions !! fromIntegral instructionNum)
    if fromIntegral instructionNum < length instructions
        then do
            interpInstruction currentInstruction
            incrementInstructionNumber 1
            runInterpreter
        else do
            return ()

