module rosalind.revc

let complement (c: char) =
    match c with
    | 'A' -> 'T'
    | 'T' -> 'A'
    | 'C' -> 'G'
    | 'G' -> 'C'
    | _ -> c

let reverseComplement (sequence: string) =
    sequence.ToCharArray()
        |> Array.rev
        |> Seq.map complement
        |> Seq.toArray
        |> System.String