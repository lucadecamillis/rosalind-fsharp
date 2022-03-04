module rosalind.revc

let complement (c: char) =
    match c with
    | 'A' -> 'T'
    | 'T' -> 'A'
    | 'C' -> 'G'
    | 'G' -> 'C'
    | _ -> failwith "unexpected input " + c

let reverseComplement (sequence: string) =
    sequence.ToCharArray()
        |> Array.rev
        |> Seq.map complement
        |> Seq.toArray
        |> System.String