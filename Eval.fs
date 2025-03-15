module Interpreter.Eval

    open Result
    open Language
    open State
    
    let (>>=) x f = x |> Result.bind f
    
      
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str    
    let rec readInt () = // This doesn't work in rider for some reason? But works in visual studio code.
        let input = readFromConsole()
        let (success, result) = tryParseInt(input)
        match success with
        |true -> result
        |false-> printfn($"{input} is not an integer") ; readInt() 
        
    let rec arithEval a st  =
        match a with
        | Num x -> Ok x
        | Var v -> getVar v st
        | Add(b,c) -> 
            (arithEval b st) >>= (fun x ->
            (arithEval c st ) >>= (fun y ->
            Ok(x + y)))
            
        | Mul(b,c) ->
            (arithEval b st) >>= (fun x ->
            (arithEval c st ) >>= (fun y ->
            Ok(x * y)))
            
        | Div(b,c) ->
            match arithEval c st with
            | Ok 0 -> Error(DivisionByZero)
            | _ -> 
                (arithEval b st) >>= (fun x ->
                (arithEval c st ) >>= (fun y ->
                Ok(x / y)))
        
        | Mod(b,c) ->
            match arithEval c st with
            | Ok 0 -> Error(DivisionByZero)
            | _ -> 
                (arithEval b st) >>= (fun x ->
                (arithEval c st ) >>= (fun y ->
                Ok(x % y)))
        | MemRead e1 ->
            arithEval e1 st >>= fun x -> getMem x st
        | Random -> Ok(random st)
        | Read -> Ok(readInt())
        | Cond(b,a1,a2) -> Ok(1)
           
    
    let rec boolEval b st =
        match b with
        | TT -> Ok true
        | Eq(a,c) -> (arithEval a st) >>= (fun x ->
            (arithEval c st ) >>= (fun y ->
            Ok(x = y)))
        
        | Lt(a,c) -> (arithEval a st) >>= (fun x ->
            (arithEval c st ) >>= (fun y ->
            Ok(x < y)))
        
        | Conj(a,c) ->
            (boolEval a st) >>= (fun x ->
            (boolEval c st ) >>= (fun y ->
            Ok(x && y)))
            
        | Not a ->
            (boolEval a st) >>= (fun y -> Ok(not y))
        
            
      
    
    let rec stmntEval s st =
        match s with
        | Skip -> Ok st
        | Declare v -> declare v st
        | Assign(v,a) ->     
            match arithEval a st with
            | Ok x -> setVar v x st
            | Error e -> Error e 
            
            
            
        | Seq(s1,s2) ->
            match stmntEval s1 st with
            | Ok st' -> stmntEval s2 st'
            | Error e -> Error e
            
        | If(guard,s1,s2) ->
            match boolEval guard st with
            | Ok x ->
                match x with
                | true -> stmntEval s1 st 
                | false -> stmntEval s2 st 
            | Error e -> Error e 
            
        | While(guard, s') ->
            match boolEval guard st with
            | Ok x ->
                match x with
                | true ->
                    let eval =  stmntEval s' st
                    match eval with
                    | Ok st' -> stmntEval (While(guard,s')) st'
                    | Error e -> Error e 
                | false -> Ok st 
            | Error e -> Error e
        
        | Alloc(x,e) ->
            let size = arithEval e st
            Result.bind (fun size -> alloc x size st) size
            
        | Free (e1,e2) ->
            let ptr = arithEval e1 st 
            let size = arithEval e2 st
            
            Result.bind (fun ptr2 -> Result.bind (fun size2 -> free ptr2 size2 st) size) ptr
         
        | MemWrite(e1,e2) ->
            
            (arithEval e1 st) >>= (fun x ->
            (arithEval e2 st ) >>= (fun y ->
            setMem x y st ))
        | _ -> Ok(st)
        
  
