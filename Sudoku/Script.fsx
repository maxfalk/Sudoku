open System


[1;2;3] -- [2]

//------------------------
//9x9
let board = [[0;0;0;8;0;0;0;0;0]; 
[4;3;7;5;0;2;0;0;0]; 
[0;2;0;7;0;0;3;1;0];
[2;0;0;0;0;0;0;6;0];
[0;9;0;0;0;0;0;2;0];
[0;4;0;0;0;0;0;0;9];
[0;6;4;0;0;5;0;7;0];
[0;0;0;6;0;3;2;4;8];
[0;0;0;0;0;7;0;0;0]]


let findPossiblesRow (board: int list list) =
    [for e in board -> [for i in e -> if i = 0 then (([1;2;3;4;5;6;7;8;9] |> Set.ofList) - (e |> Set.ofSeq)) else Set.empty |> Set.add i]]


let findPossiblesColumn (board: Set<int> list list) =
    let columns = [for col in 0 .. 8 -> board |> List.map (fun row -> row |> List.item col)]
    columns |> List.map(fun col -> col |> Seq.ofList |>  Set.intersectMany List.)
    


let rows = findPossiblesRow board 
rows |> findPossiblesColumn

let columns = [for col in 0 .. 8 -> rows |> List.map (fun row -> row |> List.item col)]
let col1 = List.item 0 columns
col1 |> List.map2 (fun e1 e2 -> (Set.difference e2 (Set.intersect e1 e2))) (List.rev col1)


//----------------------------------------

let board = 
    [|[|0;0;0;8;0;0;0;0;0|]; 
    [|4;3;7;5;0;2;0;0;0|]; 
    [|0;2;0;7;0;0;3;1;0|];
    [|2;0;0;0;0;0;0;6;0|];
    [|0;9;0;0;0;0;0;2;0|];
    [|0;4;0;0;0;0;0;0;9|];
    [|0;6;4;0;0;5;0;7;0|];
    [|0;0;0;6;0;3;2;4;8|];
    [|0;0;0;0;0;7;0;0;0|]|]



let CheckRow (board : int [,]) rowNum value =
    let row = board.[rowNum, *]
    row |> Array.contains value

let CheckColumn (board : int [,]) colNum value = 
    let col = board.[*, colNum]
    col |> Array.contains value


let CheckBox (board : int [,]) rowNum colNum value =
    let boxRow = rowNum % 3
    let boxCol = colNum % 3
    let mutable result = false
    for i in boxRow .. (boxRow + 2) do
        for j in boxCol .. (boxCol + 2) do
            if board.[i, j] = value then
                result <- true
    result


let rec SolveHelper (board : int [,]) i j =
    printfn "AT: i: %d, j: %d" i j
    let newRow = if i = 8 then 0 else i + 1
    let newCol = if i = 8 then j + 1 else j 
    if i < 9 && j < 9 then
        for v in 1 .. 9 do
            if not(CheckRow board i v) && not(CheckColumn board j v) && not(CheckBox board i j v) then
                board.[i, j] <- v
                SolveHelper board newRow newCol
            else
                SolveHelper board newRow newCol


let Solve (board : int [,]) =
    SolveHelper board 0 0
    board
                 

let board2d = Array2D.init 9 9 (fun i j -> board.[i].[j])

CheckRow board2d 0 8
CheckColumn board2d 0 4

Solve (board2d)

//------------------------
let InitPossibles (board : int [][]) =
    [|for i in 0 .. 8 ->
        [|for j in 0 .. 8 ->
            if board.[i].[j] = 0 then [1;2;3;4;5;6;7;8;9] else [board.[i].[j]] |]|]

let rec FilterHelp (row : int list []) i j len state =
    if j < len then
        if i <> j then
            let newState = row.[j] |> List.fold (fun s e -> state |> List.filter (fun e2 -> e <> e2) |> List.append s) [] |> List.distinct
            FilterHelp row i (j + 1) len newState
        else
            FilterHelp row i (j + 1) len state
    else
        state

let FilterPossiblesRow (board : int list [,])  =
    for i in 0 .. 8 do
        let row = board.[i, *]
        board.[i, *] <- [|for j in 0 .. 8 -> FilterHelp row j 0 9 row.[j] |]
    board

let FilterPossiblesCol (board : int list [,])  =
    for i in 0 .. 8 do
        let row = board.[*,i]
        board.[*,i] <- [|for j in 0 .. 8 -> FilterHelp row j 0 9 row.[j] |]
    board

let FilterBoxHelp (board : int list [,]) start k n =
    let boxRow = (start / 3) * 3
    let boxCol = (start % 3) * 3
    for i in boxRow .. (boxRow + 2) do
        for j in boxCol .. (boxCol + 2) do
            if i <> k && n <> j then
                let newBoard = board.[i,j] |> List.fold (fun s e -> board.[k,n] |> List.filter (fun e2 -> e <> e2) |> List.append s) [] |> List.distinct
                board.[k,n] <- newBoard
    board

let rec FilterBox (board : int list [,]) k = 
    if k < 9 then
        let boxRow = (k / 3) * 3
        let boxCol = (k % 3) * 3
        let mutable newBoard = board
        for i in boxRow .. (boxRow + 2) do
            for j in boxCol .. (boxCol + 2) do
                newBoard <- (FilterBoxHelp board k i j)
        FilterBox newBoard (k + 1)
    else
        board

let FilterPossibles (board : int list [,])  =
    let row = FilterPossiblesRow board
    let col = FilterPossiblesCol row
    FilterBox col 0


let board =
    [|[|0;0;0;0;2;7;9;0;0|]; 
    [|0;0;0;6;9;0;0;0;0|]; 
    [|2;0;9;3;4;0;8;0;0|];
    [|1;0;0;0;0;0;3;8;0|];
    [|6;7;3;0;0;0;2;9;5|];
    [|0;9;2;0;0;0;0;0;4|];
    [|0;0;8;0;5;2;7;0;9|];
    [|0;0;0;0;1;8;0;0;0|];
    [|0;0;4;7;6;0;0;0;0|]|]

open System
 
let PrintSodukuProg (board : int list [,]) =
    let xPos = 1
    let yPos = 1
    for i in 0 ..8 do
        for j in 0 ..8 do
            Console.SetCursorPosition(xPos * i,yPos * j)
            printfn "%A" board.[i,j]

let mutable r2d = Array2D.init 9 9 (fun i j -> [board.[i].[j]])
PrintSodukuProg r2d

let r = InitPossibles board
let r2d = Array2D.init 9 9 (fun i j -> r.[i].[j])
FilterPossiblesRow r2d
let row = r.[0]

FilterPossiblesCol r2d
FilterBox r2d 0
FilterPossibles r2d


let ListCompare res (l1:int list) (l2:int list) : bool =
    let rec CompareHelper res (l1:int list) (l2:int list) : bool =
        if res = true then
            match l1 with
            | h::t -> 
                let newRes = l2 |> List.contains h
                CompareHelper newRes t l2 
            | [] ->
                res
        else
            false
    if l1.Length = l2.Length then
        CompareHelper res l1 l2
    else
        false

[1;3;2;1] |> ListCompare true [1;2;3]
