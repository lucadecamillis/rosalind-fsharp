module rosalind.gc

open System.IO

let gcContent (sequence: string) =
    let isCG c = match c with 'C' -> true | 'G' -> true | _ -> false
    let nrCG =
        sequence.ToCharArray()
        |> Seq.filter isCG
        |> Seq.length
    ((nrCG * 100) |> double) / (sequence.Length |> double)


let parseGCdata lines =
    // reverse list of list
    let reverseLists list = list |> List.map List.rev

    // Add the item to the first list
    let addToFirstList acc item =
        match acc with
        | h::t -> (item::h)::t
        | _ -> acc

    // Split the list
    let rec splitData (lines: list<string>) acc =
        match lines with
        | [] -> reverseLists acc
        | h::t ->
            let iAcc =
                match h with
                | h when h.StartsWith(">") -> [h]::acc
                | _ -> addToFirstList acc h
            splitData t iAcc

    splitData lines []

let loadGCdata () =
    let readLines =
        "~/rosalind_gc.txt"
        |> File.ReadAllLines
        |> List.ofArray
    readLines

let gcContentEx () =
    let parseGC lines =
        match lines with
        | [] -> failwith "unexpected empty line"
        | h::t ->
            let sequence = t |> List.fold (+) ""
            let gcContent = gcContent sequence
            (h, gcContent)

    let data = loadGCdata ()
    let lines = parseGCdata data
    let top =
        lines
        |> Seq.map parseGC
        |> Seq.maxBy (fun (k,v) -> v)
    top