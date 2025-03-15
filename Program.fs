// For more information see https://aka.ms/fsharp-console-apps

open Interpreter
open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Language


let runProgram prog =
    let st = mkState 0 None 
    let lst = [Num 7; Num 6; Mul (Num 7, Num 6)]
    let s = "% times % is %, which is the answer to life, the universe, and everything"
    
    
    printfn "%A" (mergeStrings lst s st) 
     

// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort