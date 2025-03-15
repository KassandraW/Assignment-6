module Interpreter.Eval

    open Result
    open Language
    open State
    
    let (>>=) x f = x |> Result.bind f
    
      
    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str    
    let rec readInt () = 
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
            size >>= (fun size -> alloc x size st)
            
        | Free (e1,e2) ->
            let ptr = arithEval e1 st 
            let size = arithEval e2 st
            
            ptr >>= (fun ptr2 -> size >>= (fun size2 -> free ptr2 size2 st))
         
        | MemWrite(e1,e2) ->
            
            (arithEval e1 st) >>= (fun x ->
            (arithEval e2 st ) >>= (fun y ->
            setMem x y st ))
        | Print(es, s) -> Ok(st)
        
    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList
    let mergeStrings (es : aexpr list) (s : string) (st : state) : Result<string,error> =
        let s1 = split s " "
        
        let rec mergeStringsA (aexprlist : aexpr list) (stringlist : string list) (acc : string list) : Result<string list, error>  =
            match stringlist with
                | [] ->  Ok acc
                | x :: stringlist2 when x = "%"  ->
                    match aexprlist with
                    | [] -> Ok acc
                    | y :: aexprlist2 ->
                        let expr = arithEval y st
                        match expr with
                        | Error e -> Error e
                        | Ok v -> mergeStringsA aexprlist2 stringlist2 (string v :: acc)
                | z :: stringlist2 -> mergeStringsA aexprlist stringlist2 (string z :: acc)
                    
        
        let result = (mergeStringsA es s1 [])
        
        match result with
        | Error e -> Error e
        | Ok x -> Ok (String.concat " " x)
        
        
        
   
        
                   
            
                
                
                
                
            
            
        
        
        
        
        
  
