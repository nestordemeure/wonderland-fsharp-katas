// See the file magic-square.md for detailed information.

let values = [| 1.0 .. 0.5 .. 5.0 |]

type Square = float[,]

let dim = 3

//-------------------------------------------------------------------------------------------------
// EXTENSIONS

/// unit pipe
let inline (|->) x f = f x ; x

module Array =
    /// sway two element in an array
    let inline swap (arr: 'T []) i j = 
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp

module Array2D =
    /// build a set from an Array2D
    let toSet (arr: 'T [,]) = arr |> Seq.cast<'T> |> set

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// represents a square as an array (instead of a 2Darray)
type FlatSquare = float []

/// returns true if a flatSquare represents a magicSquare
let isMagic (flatSquare : FlatSquare) =
    /// sum of a diagonal
    let diagIncr (fs : FlatSquare) =
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[i + i*dim]
        res
    /// sum of the other diagonal
    let digDecr (fs : FlatSquare) =
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[(dim-1-i) + i*dim]
        res
    /// sum of a row
    let row k (fs : FlatSquare) = 
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[i + k*dim]
        res
    /// sum of a column
    let col k (fs : FlatSquare) = 
        let mutable res = 0.
        for i = 0 to dim-1 do 
            res <- res + fs.[k + i*dim]
        res
    /// all possible sum in the square (except the diagonals)
    let everySum = 
        seq { 
            for k = 0 to dim-1 do 
                yield row k flatSquare
                yield col k flatSquare
        }
    /// all sums should be equal to the target sum
    let targetSum = diagIncr flatSquare
    /// the test
    (targetSum = digDecr flatSquare) 
    && Seq.forall ((=) targetSum) everySum

/// produces a sequence of all possible permutation of the array
/// the values before the startingIndex are not permuted
/// note : modify the first array in place so the sequence element should be read-only and never more than one a t a time
let rec allPossibleSquares startingIndex (tab : FlatSquare) = 
    seq {
        if startingIndex >= tab.Length-1 then
            yield tab
        else for i = startingIndex to tab.Length-1 do
                Array.swap tab i startingIndex
                yield! allPossibleSquares (startingIndex+1) tab
                Array.swap tab startingIndex i
    }

/// convert a flatSquare into a Square
let toSquare (flatSquare : FlatSquare) : Square = Array2D.init dim dim (fun row col -> flatSquare.[row * dim + col])

//----

/// solves the magic square by testing all possible solutions
let magicSquare () =
    Array.copy values
    |> allPossibleSquares 0
    |> Seq.find isMagic
    |> toSquare
    |-> printfn "%A"
    
//-------------------------------------------------------------------------------------------------

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

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

//-----

let tests () =

    let magic = magicSquare ()

    // the square should be made using only the given values
    test <@ Array2D.toSet magic = set values @>

    // all the rows sum to the same number
    test <@ indexes |> List.map (sumRow magic) |> Set.ofList |> Set.count = 1 @>

    // all the columns sum to the same number
    test <@ indexes |> List.map (sumColumn magic) |> Set.ofList |> Set.count = 1 @>

    // all the diagonals sum to the same number
    test <@ sumDownDiagonal magic = sumUpDiagonal magic @>

// run the tests
tests ()
