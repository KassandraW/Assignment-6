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


//let sumCont m n =
    //let rec sumC xs
    
    

    
