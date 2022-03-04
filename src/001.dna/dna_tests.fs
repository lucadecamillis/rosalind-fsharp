module rosalind.dna_tests

open Xunit

[<Fact>]
let counting_DNA_Nucleotides () =
    let sequence = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    let r = dna.countNucleos sequence
    Assert.Equal<int>([|20; 12; 17; 21|], r)