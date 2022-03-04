module rosalind.rna

let transcribe2Rna (sequence: string) =
    sequence.Replace("T", "U")