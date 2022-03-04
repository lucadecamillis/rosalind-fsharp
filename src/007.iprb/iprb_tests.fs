module rosalind.iprb_tests

open Xunit

[<Fact>]
let mendel_prob () =
    let result = iprb.compute (2, 2, 2)
    Assert.True(abs (0.78333 - result) < 0.0001)