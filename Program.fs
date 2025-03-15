// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open 

let runProgram prog =
    let st = mkState 0 (Some 42)
    random st

// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort