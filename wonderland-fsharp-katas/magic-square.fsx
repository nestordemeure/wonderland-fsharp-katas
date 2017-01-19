// See the file magic-square.md for detailed information.

let values = [| 1.0 .. 0.5 .. 5.0 |]

type Square = float[,]

let dim = 3

//-----

let maxIndex = dim - 1
let indexes = [ 0 .. maxIndex ]

let row (sq:Square) i = [ for col in indexes -> sq.[i,col] ]
let col (sq:Square) i = [ for row in indexes -> sq.[row,i] ]

let sumRow (sq:Square) row =
    [ for col in indexes -> sq.[row,col] ] |> List.sum

let sumColumn (sq:Square) col =
    [ for row in indexes -> sq.[row,col] ] |> List.sum

let sumDownDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i,i] ] |> List.sum

let sumUpDiagonal (sq:Square) =
    [ for i in indexes -> sq.[i, maxIndex - i] ] |> List.sum

//-------------------------------------------------------------------------------------------------

/// unit pipe
let (|->) x f = f x ; x

let inline swap (tab: 'T []) i j = 
    let temp = tab.[i]
    tab.[i] <- tab.[j]
    tab.[j] <- temp

//-------------------------------------------------------------------------------------------------

type FlatSquare = float []

let isMagic (flatSquare : FlatSquare) =
    let diagIncr (fs : FlatSquare) =
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[i + i*dim]
        res
    let digDecr (fs : FlatSquare) =
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[(dim-1-i) + i*dim]
        res
    let row k (fs : FlatSquare) = 
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[i + k*dim]
        res
    let col k (fs : FlatSquare) = 
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[k + i*dim]
        res
    let target = diagIncr flatSquare
    target = digDecr flatSquare && Seq.forall ( (=) target )
    <| seq { 
        for k = 0 to dim-1 do 
            yield row k flatSquare
            yield col k flatSquare
        }

let rec allPossibleSquares startingIndex (tab : FlatSquare) = 
    seq {
        if startingIndex >= tab.Length-1 then
            yield tab
        else for i = startingIndex to tab.Length-1 do
                swap tab i startingIndex
                yield! allPossibleSquares (startingIndex+1) tab
                swap tab startingIndex i
    }

let to2D (flatSquare : FlatSquare) = Array2D.init dim dim (fun row col -> flatSquare.[row * dim + col])

//----

let magicSquare () =
    Array.copy values
    |> allPossibleSquares 0
    |> Seq.find isMagic
    |> to2D
    |-> printfn "%A"
    

//-------------------------------------------------------------------------------------------------

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    let magic = magicSquare ()

    // all the rows sum to the same number
    test <@ indexes |> List.map (sumRow magic) |> Set.ofList |> Set.count = 1 @>

    // all the columns sum to the same number
    test <@ indexes |> List.map (sumColumn magic) |> Set.ofList |> Set.count = 1 @>

    // all the diagonals sum to the same number
    test <@ sumDownDiagonal magic = sumUpDiagonal magic @>

// run the tests
tests ()
