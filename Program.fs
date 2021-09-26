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

    module RecursiveSolution =
        let rec iterate curr stop f =
            if curr <= stop then
                match f curr with
                | None -> iterate (curr + 1) stop f
                | Some result -> Some result
            else
                None

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

    module LoopSolution =
        let findTriplet n =
            let mutable result = None

            for a = 1 to n do
                for b = 1 to n do
                    let c = n - a - b

                    if result.IsNone && c >= 0 && a * a + b * b = c * c then
                        result <- Some(a, b, c)

            result

    module InfiniteSeqSolution =
        let infiniteSequence =
            seq {
                for b in Seq.initInfinite id do
                    for a = 1 to b do
                        yield a, b
            }

        let findTriplet n =
            infiniteSequence
            |> Seq.map (fun (a, b) -> (a, b, n - a - b))
            |> Seq.filter (fun (_, _, c) -> c >= 0)
            |> Seq.filter (fun (a, b, c) -> a * a + b * b = c * c)
            |> Seq.tryHead

    let solve triplet =
        Option.map (fun (a, b, c) -> a * b * c) triplet

module Problem22 =
    let names () =
        System.IO.File.ReadAllText("names.txt").Split ','
        |> Seq.map (fun s -> s.Trim('"').ToUpper())
        |> Seq.sort

    let characterPos (c: char) = int c - int 'A' + 1

    module TailRecursiveSolution =
        let rec nameScore name =
            match Seq.tryHead name with
            | None -> 0
            | Some c -> characterPos c + nameScore (Seq.tail name)

        let rec iterate pos (names: list<string>) =
            match names with
            | [] -> 0
            | head :: tail -> pos * (nameScore head) + iterate (pos + 1) tail

        let solve names = iterate 1 (Seq.toList names)

    module RecursiveSolution =
        let rec nameScore name =
            match Seq.tryHead name with
            | Some c -> characterPos c + nameScore (Seq.tail name)
            | None -> 0

        let rec iterate pos (names: list<string>) =
            match names with
            | head :: tail -> pos * (nameScore head) + iterate (pos + 1) tail
            | [] -> 0

        let solve names = iterate 1 (Seq.toList names)

    module ReduceSolution =
        let solve names =
            names
            |> Seq.zip (Seq.initInfinite id)
            |> Seq.sumBy (fun (i, name) -> (i + 1) * (Seq.sumBy characterPos name))

    module MapSolution =
        let solve names =
            names
            |> Seq.zip (Seq.initInfinite id)
            |> Seq.map
                (fun (i, name) ->
                    (i + 1)
                    * (name |> Seq.map characterPos |> Seq.sum))
            |> Seq.sum

    module LoopSolution =
        let nameScore name =
            let mutable sum = 0

            for c in name do
                sum <- sum + characterPos c

            sum

        let solve names =
            let mutable i = 0
            let mutable result = 0

            for name in names do
                i <- i + 1
                result <- result + i * nameScore name

            result

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
    printfn "Tail recursive solution: %A" (Problem22.TailRecursiveSolution.solve names)
    printfn "Recursive      solution: %A" (Problem22.RecursiveSolution.solve names)
    printfn "Map            solution: %A" (Problem22.MapSolution.solve names)
    0
