module rosalind.fib

let rec rabbit (n: int) (k: int) =
    match n with
    | 1 -> 1
    | 2 -> 1
    | _ -> (rabbit (n-1) k) + (k * (rabbit (n-2) k))