module rosalind.rna_tests

open Xunit

[<Fact>]
let transcribe_DNA_2_RNA () =
    let sequence = "GATGGAACTTGACTACGTAAATT"
    let r = rna.transcribe2Rna sequence
    Assert.Equal("GAUGGAACUUGACUACGUAAAUU", r)