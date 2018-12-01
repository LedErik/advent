let explode s =
    [|for x in s -> x|]

let parseInt (xs: char array) =
    match xs.[0] with
        |'-'-> xs.[1..] |> System.String.Concat |> int |> (fun x -> x * -1)
        |'+'-> xs.[1..] |> System.String.Concat |> int
        | s -> failwith ("unexpected input" + string s)

[<EntryPoint>]
let main(argv: string array) =
    System.IO.File.ReadAllLines argv.[0] |>
    Array.map (explode >> parseInt) |>
    Array.reduce (+) |>
    printfn "%d"
    0
