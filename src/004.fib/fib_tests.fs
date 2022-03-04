module rosalind.fib_tests

open Xunit

[<Fact>]
let rabbits_recurrence_relations () =
    let result = fib.rabbit 5 3
    Assert.Equal(19, result)