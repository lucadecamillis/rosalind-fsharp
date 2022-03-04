module rosalind.prot

type RNA =
    | A
    | C
    | G
    | U

type AminoAcid =
    | A
    | R
    | N
    | D
    | C
    | Q
    | E
    | G
    | H
    | I
    | L
    | K
    | M
    | F
    | P
    | S
    | T
    | W
    | Y
    | V
    | Stop

let RNAtoAminoAcid rnaCodon =
    match rnaCodon with
    | (RNA.G, RNA.C, RNA.U)
    | (RNA.G, RNA.C, RNA.C)
    | (RNA.G, RNA.C, RNA.A)
    | (RNA.G, RNA.C, RNA.G) -> AminoAcid.A
    | (RNA.U, RNA.G, RNA.U)
    | (RNA.U, RNA.G, RNA.C) -> AminoAcid.C
    | (RNA.G, RNA.A, RNA.U)
    | (RNA.G, RNA.A, RNA.C) -> AminoAcid.D
    | (RNA.G, RNA.A, RNA.A)
    | (RNA.G, RNA.A, RNA.G) -> AminoAcid.E
    | (RNA.U, RNA.U, RNA.U)
    | (RNA.U, RNA.U, RNA.C) -> AminoAcid.F
    | (RNA.G, RNA.G, RNA.U)
    | (RNA.G, RNA.G, RNA.C)
    | (RNA.G, RNA.G, RNA.A)
    | (RNA.G, RNA.G, RNA.G) -> AminoAcid.G
    | (RNA.C, RNA.A, RNA.U)
    | (RNA.C, RNA.A, RNA.C) -> AminoAcid.H
    | (RNA.A, RNA.U, RNA.U)
    | (RNA.A, RNA.U, RNA.C)
    | (RNA.A, RNA.U, RNA.A) -> AminoAcid.I
    | (RNA.A, RNA.A, RNA.A)
    | (RNA.A, RNA.A, RNA.G) -> AminoAcid.K
    | (RNA.U, RNA.U, RNA.A)
    | (RNA.U, RNA.U, RNA.G)
    | (RNA.C, RNA.U, RNA.U)
    | (RNA.C, RNA.U, RNA.C)
    | (RNA.C, RNA.U, RNA.A)
    | (RNA.C, RNA.U, RNA.G) -> AminoAcid.L
    | (RNA.A, RNA.U, RNA.G) -> AminoAcid.M // start codon
    | (RNA.A, RNA.A, RNA.U)
    | (RNA.A, RNA.A, RNA.C) -> AminoAcid.N
    | (RNA.C, RNA.C, RNA.U)
    | (RNA.C, RNA.C, RNA.C)
    | (RNA.C, RNA.C, RNA.A)
    | (RNA.C, RNA.C, RNA.G) -> AminoAcid.P
    | (RNA.C, RNA.A, RNA.A)
    | (RNA.C, RNA.A, RNA.G) -> AminoAcid.Q
    | (RNA.C, RNA.G, RNA.U)
    | (RNA.C, RNA.G, RNA.C)
    | (RNA.C, RNA.G, RNA.A)
    | (RNA.C, RNA.G, RNA.G)
    | (RNA.A, RNA.G, RNA.A)
    | (RNA.A, RNA.G, RNA.G) -> AminoAcid.R
    | (RNA.U, RNA.C, RNA.U)
    | (RNA.U, RNA.C, RNA.C)
    | (RNA.U, RNA.C, RNA.A)
    | (RNA.U, RNA.C, RNA.G)
    | (RNA.A, RNA.G, RNA.U)
    | (RNA.A, RNA.G, RNA.C) -> AminoAcid.S
    | (RNA.A, RNA.C, RNA.U)
    | (RNA.A, RNA.C, RNA.C)
    | (RNA.A, RNA.C, RNA.A)
    | (RNA.A, RNA.C, RNA.G) -> AminoAcid.T
    | (RNA.G, RNA.U, RNA.U)
    | (RNA.G, RNA.U, RNA.C)
    | (RNA.G, RNA.U, RNA.A)
    | (RNA.G, RNA.U, RNA.G) -> AminoAcid.V
    | (RNA.U, RNA.G, RNA.G) -> AminoAcid.W
    | (RNA.U, RNA.A, RNA.U)
    | (RNA.U, RNA.A, RNA.C) -> AminoAcid.Y
    | (RNA.U, RNA.A, RNA.A)
    | (RNA.U, RNA.A, RNA.G)
    | (RNA.U, RNA.G, RNA.A) -> AminoAcid.Stop

let tryParseRNACharacter c =
    match c with
    | 'A' -> (true, Some(RNA.A))
    | 'C' -> (true, Some(RNA.C))
    | 'G' -> (true, Some(RNA.G))
    | 'U' -> (true, Some(RNA.U))
    | _ -> (false, None)

let parseRNACharacter c =
    match tryParseRNACharacter c with
    | (true, Some s) -> s
    | _ -> raise (new System.ArgumentException("Invalid character " + c.ToString()))

let rnaToProtein (sequence: string) =
    let assignIdx (idx: int) e = (idx / 3, e)

    let firstThreeToTuple (a: seq<'T>) =
        (Seq.item 0 a, Seq.item 1 a, Seq.item 2 a)

    let aminoAcidSeq =
        sequence
        |> Seq.map parseRNACharacter // parse characters into RNA values
        |> Seq.mapi assignIdx // assign index to each amino acid
        |> Seq.groupBy (fun t -> fst t) // Group sequences by the amino acid index
        |> Seq.map snd
        |> Seq.map (Seq.map snd)
        |> Seq.filter (fun e -> (e |> Seq.length) = 3)
        |> Seq.map firstThreeToTuple
        |> Seq.map RNAtoAminoAcid
        |> Seq.takeWhile (fun s -> s <> AminoAcid.Stop)

    let r =
        aminoAcidSeq
        |> Seq.map (fun e -> e.ToString())
        |> String.concat ""
    r