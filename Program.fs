module Problem9 =
    let isAnswer n a b =
        let c = n - a - b
        a * a + b * b = c * c

    let optionalAnswer n a b =
        if isAnswer n a b then
            Some(a, b, n - a - b)
        else
            None

    let sequence n =
        let oneToN = seq { 1 .. n }
        Seq.allPairs oneToN oneToN

    module TailRecursiveSolution =
        let rec iterate curr stop f =
            if curr > stop then
                None
            else
                match f curr with
                | Some result -> Some result
                | None -> iterate (curr + 1) stop f

        let findTriplet n =
            iterate 1 n (fun a -> iterate a (n - a) <| optionalAnswer n a)

    module ReduceSolution =
        let findTriplet n =
            sequence n
            |> Seq.filter (fun (a, b) -> a + b <= n)
            |> Seq.tryPick (fun (a, b) -> optionalAnswer n a b)

    module MapSolution =
        let findTriplet n =
            sequence n
            |> Seq.map (fun (a, b) -> (a, b, n - a - b))
            |> Seq.filter (fun (_, _, c) -> c >= 0)
            |> Seq.tryFind (fun (a, b, c) -> a * a + b * b = c * c)

    let solve triplet =
        match triplet with
        | Some (a, b, c) -> Some(a * b * c)
        | None -> None

module Problem22 =
    let names () =
        System.IO.File.ReadAllText("names.txt").Split ','
        |> Seq.map (fun s -> s.Trim('"').ToUpper())
        |> Seq.sort

    let characterPos (c: char) = int c - int 'A' + 1

    module MapSolution =
        let scoreOfName pos name =
            name |> Seq.sumBy characterPos |> (*) pos

        let solve names =
            names
            |> Seq.zip (Seq.initInfinite id)
            |> Seq.sumBy (fun (i, name) -> scoreOfName (i + 1) name)

[<EntryPoint>]
let main _ =
    printfn "Special Pythagorean Triplet"
    let limit = 1000

    // Problem9.TailRecursiveSolution.findTriplet limit
    // |> Problem9.solve
    // |> printfn "Tail recursive solution: %A"

    // Problem9.ReduceSolution.findTriplet limit
    // |> Problem9.solve
    // |> printfn "Reduce solution: %A"

    // Problem9.MapSolution.findTriplet limit
    // |> Problem9.solve
    // |> printfn "Map solution: %A"

    printfn "\nNames scores"
    let names = Problem22.names ()
    printfn "Map solution: %A" (Problem22.MapSolution.solve names)
    0
