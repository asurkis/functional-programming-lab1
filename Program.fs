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

    let resolveTriplet triplet =
        Option.map (fun (a, b, c) -> a, b, c, a * b * c) triplet

    module TailRecursive =
        let rec iterate curr stop f =
            if curr > stop then
                None
            else
                match f curr with
                | Some result -> Some result
                | None -> iterate (curr + 1) stop f

        let solve n =
            iterate 1 n (fun a -> iterate a (n - a) <| optionalAnswer n a)
            |> resolveTriplet

    module Recursive =
        let rec iterate curr f =
            if curr = 0 then
                None
            else
                match iterate (curr - 1) f with
                | Some result -> Some result
                | None -> f curr

        let solve n =
            iterate n (fun b -> iterate b (fun a -> optionalAnswer n a b))
            |> resolveTriplet

    module Reduce =
        let solve n =
            sequence n
            |> Seq.filter (fun (a, b) -> a + b <= n)
            |> Seq.tryPick (fun (a, b) -> optionalAnswer n a b)
            |> resolveTriplet

    module Map =
        let solve n =
            sequence n
            |> Seq.map (fun (a, b) -> (a, b, n - a - b))
            |> Seq.filter (fun (_, _, c) -> c >= 0)
            |> Seq.tryFind (fun (a, b, c) -> a * a + b * b = c * c)
            |> resolveTriplet

    module Loop =
        let solve n =
            seq {
                for a = 1 to n do
                    for b = 1 to n do
                        let c = n - a - b

                        if c >= 0 && a * a + b * b = c * c then
                            yield a, b, c
            }
            |> Seq.tryHead
            |> resolveTriplet

    module InfiniteSeq =
        let solve n =
            seq {
                for b in Seq.initInfinite id do
                    for a = 1 to b do
                        yield a, b
            }
            |> Seq.map (fun (a, b) -> (a, b, n - a - b))
            |> Seq.filter (fun (_, _, c) -> c >= 0)
            |> Seq.filter (fun (a, b, c) -> a * a + b * b = c * c)
            |> Seq.tryHead
            |> resolveTriplet

    let printSolution () =
        printfn "Problem 9: Special Pythagorean triplet"
        let limit = 1000
        printfn "Tail recursive    solution: %A" (TailRecursive.solve limit)
        printfn "Recursive         solution: %A" (Recursive.solve limit)
        printfn "Reduce            solution: %A" (Reduce.solve limit)
        printfn "Map               solution: %A" (Map.solve limit)
        printfn "Loop              solution: %A" (Loop.solve limit)
        printfn "Infinite sequence solution: %A" (InfiniteSeq.solve limit)
        printfn ""

module Problem22 =
    let characterPos (c: char) = int c - int 'A' + 1

    module TailRecursive =
        let rec nameScore acc name =
            match Seq.tryHead name with
            | None -> acc
            | Some c -> nameScore (acc + characterPos c) (Seq.tail name)

        let rec iterate acc pos (names: list<string>) =
            match names with
            | [] -> acc
            | head :: tail -> iterate (acc + pos * (nameScore 0 head)) (pos + 1) tail

        let solve names = iterate 0 1 (Seq.toList names)

    module Recursive =
        let rec nameScore name =
            match Seq.tryHead name with
            | None -> 0
            | Some c -> characterPos c + nameScore (Seq.tail name)

        let rec iterate pos (names: list<string>) =
            match names with
            | [] -> 0
            | head :: tail -> pos * (nameScore head) + iterate (pos + 1) tail

        let solve names = iterate 1 (Seq.toList names)

    module Reduce =
        let solve names =
            names
            |> Seq.zip (Seq.initInfinite id)
            |> Seq.sumBy (fun (i, name) -> (i + 1) * (Seq.sumBy characterPos name))

    module Map =
        let solve names =
            names
            |> Seq.zip (Seq.initInfinite id)
            |> Seq.map
                (fun (i, name) ->
                    (i + 1)
                    * (name |> Seq.map characterPos |> Seq.sum))
            |> Seq.sum

    module Loop =
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

    let printSolution () =
        printfn "Problem 22: Names scores"

        let names =
            System.IO.File.ReadAllText("names.txt").Split ','
            |> Seq.map (fun s -> s.Trim('"').ToUpper())
            |> Seq.sort

        printfn "Tail recursive solution: %A" (TailRecursive.solve names)
        printfn "Recursive      solution: %A" (Recursive.solve names)
        printfn "Reduce         solution: %A" (Reduce.solve names)
        printfn "Map            solution: %A" (Map.solve names)
        printfn "Loop           solution: %A" (Loop.solve names)
        printfn ""


[<EntryPoint>]
let main _ =
    Problem9.printSolution ()
    Problem22.printSolution ()
    0
