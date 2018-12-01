//string to char array
let explode s =
    [|for x in s -> x|]

//crude int parser
let parseInt (xs: char array) =
    match xs.[0] with
        |'-'-> xs.[1..] |> System.String.Concat |> int |> (fun x -> x * -1)
        |'+'-> xs.[1..] |> System.String.Concat |> int
        | s -> failwith ("unexpected input" + string s)

let collect set value found =
    match found with
        |None -> if (Set.contains value set && found = None) then (set, Some value,value) else (Set.add value set, found,value)
        |r -> set,r,value

let rec loop x contf func =
    match contf x with
        |false -> x
        |true -> loop (func x) contf func

let findrepeat arr  =
    (fun x -> Array.fold (fun (set,found,sum) x -> (collect set (sum + x) found)) x arr) |>
    loop ((Set.add 0 Set.empty), None, 0) (fun (a,b,c) -> b = None)

[<EntryPoint>]
let main(argv) =
    System.IO.File.ReadAllLines argv.[0] |>
    Array.map (explode >> parseInt) |>
    findrepeat |>
    (fun (_,b,_) -> b.Value) |>
    printfn "%d"
    0
