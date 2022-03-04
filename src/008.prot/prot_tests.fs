module rosalind.prot_tests

open Xunit

[<Fact>]
let RNA_2_PROTEIN () =
    let input =
        "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"

    let protein = prot.rnaToProtein input
    Assert.Equal("MAMAPRTEINSTRING", protein)