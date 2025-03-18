module Assignment6.Optional

let sumAcc m n =
    let rec sumA counter acc =
        match counter with
        | x when x = n -> acc + (m + n)
        | _ -> sumA (counter + 1) (acc + (m + counter))
    sumA 0 0
    
let listLengthAcc lst : int =
    let rec listLengthA alst acc =
        match alst with
        | []-> acc
        | _ :: xs -> listLengthA xs (acc+1)
    
    listLengthA lst 0

let sumCont m n =
    let rec sumC counter c =
        match counter with
        | x when x = n -> c m+n 
        | _ -> sumC (counter+1) (fun result -> c (result + (m + counter)))
    sumC 0 id
    
let listLengthCont lst : int =
    let rec listLengthC aLst c =
        match aLst with
        | [] -> c 0 
        | _ :: xs -> listLengthC xs (fun result -> c(result + 1))
    
    listLengthC lst id 
    
    

    
