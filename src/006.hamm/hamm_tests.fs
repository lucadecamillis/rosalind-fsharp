module rosalind.hamm_tests

open Xunit

[<Fact>]
let hammingDistance () =
    let s = "GAGCCTACTAACGGGAT"
    let t = "CATCGTAATGACGGCCT"
    let diff = hamm.hammingDistance s t
    Assert.Equal(7, diff)