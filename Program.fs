module Problem9 =
    let optionalAnswer n a b =
        let c = n - a - b

        if a * a + b * b = c * c then
            Some(a, b, c)
        else
            None

    let sequence n =
        let oneToN = seq { 1 .. n }
        Seq.allPairs oneToN oneToN

    let resolveTriplet triplet =
        Option.map (fun (a, b, c) -> a, b, c, a * b * c) triplet

    module TailRecursive =
        let rec iterateA n b a =
            if a > b then
                None
            else
                optionalAnswer n a b
                |> Option.orElseWith (fun () -> iterateA n b (a + 1))

        let rec iterateB n b =
            if b > n then
                None
            else
                iterateA n b 1
                |> Option.orElseWith (fun () -> iterateB n (b + 1))

        let solve n = iterateB n 1 |> resolveTriplet

    module Recursive =
        let rec iterateA n b a =
            if a <= 0 then
                None
            else
                iterateA n b (a - 1)
                |> Option.orElse (optionalAnswer n a b)

        let rec iterateB n b =
            if b <= 0 then
                None
            else
                iterateB n (b - 1)
                |> Option.orElseWith (fun () -> iterateA n b b)

        let solve n = iterateB n n |> resolveTriplet

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
            fun i -> Seq.map (fun j -> j + 1, i + 1) { 0 .. i }
            |> Seq.initInfinite
            |> Seq.concat
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
            match name with
            | [] -> acc
            | head :: tail -> nameScore (acc + characterPos head) tail

        let rec iterate acc pos names =
            match names with
            | [] -> acc
            | head :: tail -> iterate (acc + pos * (nameScore 0 (Seq.toList head))) (pos + 1) tail

        let solve names = iterate 0 1 names

    module Recursive =
        let rec nameScore name =
            match name with
            | [] -> 0
            | head :: tail -> characterPos head + nameScore tail

        let rec iterate pos names =
            match names with
            | [] -> 0
            | head :: tail ->
                pos * ((Seq.toList >> nameScore) head)
                + iterate (pos + 1) tail

        let solve names = iterate 1 names

    module Reduce =
        let solve names =
            names
            |> List.zip [ 1 .. names.Length ]
            |> List.sumBy (fun (pos, name) -> pos * Seq.sumBy characterPos name)

    module Map =
        let solve names =
            names
            |> List.mapi
                (fun i name ->
                    (i + 1)
                    * (name |> Seq.map characterPos |> Seq.sum))
            |> List.sum

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

    module InfiniteSeq =
        let nameScore name = Seq.sumBy characterPos name

        let solve names =
            let arr = Seq.toArray names

            Seq.initInfinite (fun i -> i + 1, arr.[i])
            |> Seq.map (fun (i, name) -> i * nameScore name)
            |> Seq.scan (+) 0
            |> Seq.skip arr.Length
            |> Seq.head

    let printSolution () =
        printfn "Problem 22: Names scores"

        let names =
            System.IO.File.ReadAllText("names.txt").Split ','
            |> Array.map (fun s -> s.Trim('"').ToUpper())
            |> Array.sort
            |> Array.toList

        printfn "Tail recursive    solution: %A" (TailRecursive.solve names)
        printfn "Recursive         solution: %A" (Recursive.solve names)
        printfn "Reduce            solution: %A" (Reduce.solve names)
        printfn "Map               solution: %A" (Map.solve names)
        printfn "Loop              solution: %A" (Loop.solve names)
        printfn "Infinite sequence solution: %A" (InfiniteSeq.solve names)
        printfn ""


[<EntryPoint>]
let main _ =
    Problem9.printSolution ()
    Problem22.printSolution ()
    0
