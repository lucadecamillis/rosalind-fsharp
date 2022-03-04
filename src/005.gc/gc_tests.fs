module rosalind.gc_tests

open Xunit

[<Fact>]
let computing_GC_content () =
    let sequence = "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"
    let gc = gc.gcContent sequence
    Assert.True(abs (60.919540 - gc) < 0.0001)

[<Fact>]
let parse_GC_data () =
    let input = [">1"; "ABV"; "GVB"; "DSL"; ">2"; "EPER"; "ERW"; "SVF"; ">3"; "WPL"; "BFG"; "KAS"]
    let r = gc.parseGCdata input
    Assert.Equal(3, r.Length)