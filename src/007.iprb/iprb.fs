module rosalind.iprb

let xx (nx: int) (tot: int) =
    (nx * (nx - 1) |> float) / (tot * (tot - 1) |> float)

let xy (nx: int) (ny: int) (tot: int) =
    (nx * ny |> float) / (tot * (tot - 1) |> float)

let compute (k, m, n) =
    let km = 1.0
    let kn = 1.0
    let mn = 0.5

    let kk = 1.0
    let mm = 0.75
    let nn = 0.0

    let tot = k + m + n

    let result =
        ((xy k m tot) * km) * 2.0 +
        ((xy k n tot) * kn) * 2.0 +
        ((xy m n tot) * mn) * 2.0 +
        (xx k tot) * kk +
        (xx m tot) * mm
    result