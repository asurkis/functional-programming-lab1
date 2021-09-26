module Problem9 =
    let isAnswer n a b =
        let c = n - a - b
        a * a + b * b = c * c

    let optionalAnswer n a b =
        if isAnswer n a b then
            Some(a, b, n - a - b)
        else
            None

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
        let sequence n =
            seq {
                for a = 1 to n do
                    for b = 1 to n do
                        yield (a, b)
            }

        let findTriplet n =
            sequence n
            |> Seq.filter (fun (a, b) -> a + b <= n)
            |> Seq.tryPick (fun (a, b) -> optionalAnswer n a b)

    module MapSolution =
        let sequence n =
            seq {
                for a = 1 to n do
                    yield! (seq { 1 .. n } |> Seq.map (fun b -> a, b))
            }

        let findTriplet n =
            sequence n
            |> Seq.map (fun (a, b) -> (a, b, n - a - b))
            |> Seq.filter (fun (_, _, c) -> c >= 0)
            |> Seq.tryFind (fun (a, b, c) -> a * a + b * b = c * c)

    let solve triplet =
        match triplet with
        | Some (a, b, c) -> Some(a * b * c)
        | None -> None

[<EntryPoint>]
let main _ =
    printfn "Special Pythagorean Triplet"
    let limit = 1000

    Problem9.TailRecursiveSolution.findTriplet limit
    |> Problem9.solve
    |> printfn "Tail recursive solution: %A"

    Problem9.ReduceSolution.findTriplet limit
    |> Problem9.solve
    |> printfn "Reduce         solution: %A"

    Problem9.MapSolution.findTriplet limit
    |> Problem9.solve
    |> printfn "Map-reduce     solution: %A"

    0
