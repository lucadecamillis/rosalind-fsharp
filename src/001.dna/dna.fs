module rosalind.dna

let countNucleos (sequence: string) =
    sequence
        |> Seq.groupBy id
        |> Seq.sortBy (fun (k,v) -> k)
        |> Seq.map (fun (k,v) -> v |> Seq.length)
        |> Seq.toArray

let countNucleosSimple (sequence: string) =
    sequence
        |> Seq.countBy id
        |> Seq.sortBy (fun (k,v) -> k)
        |> Seq.map snd
        |> Seq.toArray