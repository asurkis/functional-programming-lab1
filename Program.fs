module Problem9 =
    let checkAB n a b =
        let c = n - a - b

        if a * a + b * b = c * c then
            Some(a, b, c)
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
            iterate 1 n (fun a -> iterate a (n - a) <| checkAB n a)

[<EntryPoint>]
let main _ =
    printfn "Special Pythagorean Triplet"
    printfn "Tail recursive solution: %A" (Problem9.TailRecursiveSolution.solve 1000)
    0
