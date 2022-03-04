module rosalind.revc_tests

open Xunit

[<Fact>]
let complement_strand_DNA () =
    let sequence = "AAAACCCGGT"
    let r = revc.reverseComplement sequence
    Assert.Equal("ACCGGGTTTT", r)