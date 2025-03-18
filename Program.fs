module programsss

open Interpreter
open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Language


let [<EntryPoint>] main _ =
    mkState 0 None |> 
    stmntEval (Print ([Num 7; Num 6; Mul (Num 7, Num 6)], "% times % is %, which is the answer to life, the universe, and everything"))
    
    mkState 0 None |> 
    stmntEval (Print ([Num 7; Num 6; Mul (Num 7, Num 6)],
                    "% times % is 42, which is the answer to life, the universe, and everything"))
    0 
    
    

// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort