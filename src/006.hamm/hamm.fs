module rosalind.hamm

let hammingDistance (s: string) (t: string) =
    let charDiffer (c1, c2) = c1 <> c2
    let diff =
        Seq.zip s t
        |> Seq.filter charDiffer
        |> Seq.length
    diff