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

        let solve n =
            iterate 1 n (fun a -> iterate a (n - a) <| optionalAnswer n a)

    module ReduceSolution =
        let sequence n =
            seq {
                for a = 1 to n do
                    for b = 1 to n do
                        yield (a, b)
            }

        let solve n =
            sequence n
            |> Seq.filter (fun (a, b) -> a + b <= n)
            |> Seq.tryPick (fun (a, b) -> optionalAnswer n a b)

let solve n = ()

[<EntryPoint>]
let main _ =
    printfn "Special Pythagorean Triplet"
    printfn "Tail recursive solution: %A" (Problem9.TailRecursiveSolution.solve 1000)
    printfn "Reduce         solution: %A" (Problem9.ReduceSolution.solve 1000)
    0
