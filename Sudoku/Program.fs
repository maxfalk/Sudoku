open System
open System.Threading
open System.IO

type PrintType = Board of (int list [,] * int * int) | Count of bool 

let PrintSodukuProg (board : int list [,]) iPos jPos =
    let xPos = 6
    let yPos = 2
    for i in 0 ..8 do
        for j in 0 ..8 do
            Console.SetCursorPosition(xPos * i,yPos * j)
            printfn "     "
            Console.SetCursorPosition(xPos * i,yPos * j + 1)
            printfn "     "
            for k in 0 .. (board.[i,j].Length - 1) do
                Console.SetCursorPosition(xPos * i + (k % 5), yPos * j + (k / 5)) |> ignore
                if i = iPos && j = jPos then
                    let prevColor = Console.ForegroundColor
                    Console.ForegroundColor = ConsoleColor.Yellow |> ignore
                    Console.Write("{0}",  (board.[i,j].Item k))
                    Console.ForegroundColor = prevColor |> ignore
                else
                    Console.Write("{0}",  (board.[i,j].Item k))

let printerAgent = MailboxProcessor.Start(fun inbox-> 
    let rec messageLoop(msgs) = async{
        let! msg = inbox.Receive()
        match msg with
        | Board(b, i, j) ->
            PrintSodukuProg b i j
        | Count(b) ->
            Console.SetCursorPosition(0, 20)
            printfn "Messages: %d" msgs
        Thread.Sleep(100)
        return! messageLoop(msgs + 1)  
        }
    messageLoop(0) 
    )


//----------------------------------------------------
let ListCompare (l1:int list) (l2:int list) : bool =
    let rec CompareHelper res (l1:int list) (l2:int list) : bool =
        if res then
            match l1 with
            | h::t -> 
                let newRes = l2 |> List.contains h
                CompareHelper newRes t l2 
            | [] ->
                res
        else
            false
    if l1.Length = l2.Length then
        CompareHelper true l1 l2
    else
        false

let ListArrayCompare (a1: int list []) (a2: int list []) =
    let comps = a1 |> Array.map2 ListCompare a2
    not(comps |> Array.contains false)

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
    let mutable res = false
    for i in 0 .. 8 do
        let row = board.[i, *]
        let newRow = [|for j in 0 .. 8 -> FilterHelp row j 0 9 row.[j] |]
        if not(newRow |> ListArrayCompare row) then
            board.[i, *] <- newRow
            res <- true
    (res,board)

let FilterPossiblesCol (board : int list [,])  =
    let mutable res = false
    for i in 0 .. 8 do
        let row = board.[*,i]
        let newRow = [|for j in 0 .. 8 -> FilterHelp row j 0 9 row.[j] |]
        if not(newRow |> ListArrayCompare row) then
            board.[*,i] <-newRow
            res <- true
    (res,board)

let FilterBoxHelp (board : int list [,]) start k n =
    let boxRow = (start / 3) * 3
    let boxCol = (start % 3) * 3
    let mutable res = false
    for i in boxRow .. (boxRow + 2) do
        for j in boxCol .. (boxCol + 2) do
            if i <> k && n <> j then
                let newBoard = board.[i,j] |> List.fold (fun s e -> board.[k,n] |> List.filter (fun e2 -> e <> e2) |> List.append s) [] |> List.distinct
                if not(board.[k,n] |> ListCompare newBoard) then
                    board.[k,n] <- newBoard
                    res <- true
    (res,board)

let rec FilterBox (board : int list [,]) k =
    let mutable res = false 
    if k < 9 then
        let boxRow = (k / 3) * 3
        let boxCol = (k % 3) * 3
        let mutable newBoard = board
        for i in boxRow .. (boxRow + 2) do
            for j in boxCol .. (boxCol + 2) do
                let (result, newBox) = (FilterBoxHelp board k i j)
                newBoard <- newBox
                res <- if result then result else res 
        FilterBox newBoard (k + 1)
    else
        (res, board)

let FilterPossibles (board : int list [,])  =
    let (r1, row) = FilterPossiblesRow board
    let (r2, col) = FilterPossiblesCol row
    let (r3,box) = FilterBox col 0
    (r1 || r2 || r3, box)


let ReducePossibleValues (board : int list [,]) =
    let mutable loop = true
    while loop do
        let (r1, b) = FilterPossibles board
        loop <- r1
    board

let SolveWithPossiblesReduce board =
    let rec SolveWithPossiblesReduceHelper v =
        match v with 
        | ((board : int list [,]), i, j) when i < 9 && j < 9 ->
            printerAgent.Post (Board(Array2D.copy board, i, j))
            if board.[i,j] |> List.isEmpty |> not then
                let bc = Array2D.copy board
                bc.[i,j] <- [board.[i,j].Head]
                let rdb = ReducePossibleValues bc
                let (res, nb) = SolveWithPossiblesReduceHelper (rdb, (i + 1), j)
                match res with
                | false ->
                    board.[i, j] <- board.[i, j].Tail
                    SolveWithPossiblesReduceHelper(board, i, j)
                | true ->
                    (true, nb)
             else
                (false, board)
        | ((board : int list [,]), i, j) when j < 9  ->
            SolveWithPossiblesReduceHelper (board, 0, (j + 1))
        | ((board : int list [,]), i, j)  ->
            (true, board)
    let r = InitPossibles board
    let mutable r2d = Array2D.init 9 9 (fun i j -> r.[i].[j])
    let (res, nb) = SolveWithPossiblesReduceHelper (r2d, 0, 0)
    nb


[<EntryPoint>]
let main argv = 
    let b1 =
        [|[|0;0;0;0;7;8;0;0;0|]; 
        [|0;1;8;0;0;0;0;0;0|]; 
        [|2;6;0;3;0;0;0;1;0|];
        [|0;0;0;2;0;0;0;6;3|];
        [|0;4;0;0;0;1;0;0;2|];
        [|0;0;0;7;0;0;0;8;1|];
        [|7;5;0;6;0;0;0;9;0|];
        [|0;8;2;0;0;0;0;0;0|];
        [|0;0;0;0;1;9;0;0;0|]|]
    let b2 =
        [|[|0;0;0;0;0;0;0;0;0|]; 
        [|0;0;0;0;0;0;0;0;0|]; 
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];
        [|0;0;0;0;0;0;0;0;0|];|]

    
    let s = SolveWithPossiblesReduce b2
    printerAgent.Post (Count(true))
    Console.ReadKey() |> ignore

    0 // return an integer exit code

