// See the file tiny-maze.md for detailed information.

type Cell =
    | Start
    | Exit
    | Empty
    | Wall
    member this.NonBlocking = this <> Wall

type Maze = Cell [,]

type Path = X | O

type Solution = Path [,]

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// unit pipe
let (|->) x f = f x ; x

let solve (maze:Maze) : Solution =
    let l1 = Array2D.length1 maze
    let l2 = Array2D.length1 maze
    let unVisited = Array2D.create l1 l2 true
    let result = Array2D.create l1 l2 O
    /// returns the legal, still unvisited, neigbours of a cell
    let getNeigbours i j =
        [
            if i-1 >= 0 && unVisited.[i-1,j] && maze.[i-1,j].NonBlocking then 
                yield (i-1,j)
            if i+1 < l1 && unVisited.[i+1,j] && maze.[i+1,j].NonBlocking then 
                yield (i+1,j)
            if j-1 >= 0 && unVisited.[i,j-1] && maze.[i,j-1].NonBlocking then 
                yield (i,j-1)
            if j+1 < l1 && unVisited.[i,j+1] && maze.[i,j+1].NonBlocking then 
                yield (i,j+1)
        ]
    /// if (i,j) is on the path to the exit then it is inscribed in the result and the function returns true
    /// else returns false
    let rec hasPath (i,j) =
        unVisited.[i,j] <- false 
        let onPath = ( maze.[i,j] = Exit ) || ( getNeigbours i j |> List.exists hasPath )
        if onPath then result.[i,j] <- X ; true else false
    if not <| hasPath (0,0) then failwith "boom" else result |-> printfn "%A"

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // sample 3x3 maze
    let maze3x3 =
        [ [Start; Empty; Wall]
          [Wall;  Empty; Wall]
          [Wall;  Empty; Exit]]
        |> array2D

    // sample 3x3 maze solution
    let solution3x3 =
        [ [X; X; O]
          [O; X; O]
          [O; X; X]]
        |> array2D

    test <@ solution3x3 = solve maze3x3 @>

    // sample 4x4 maze
    let maze4x4 =
         [[Start; Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Empty]
          [Wall;  Empty; Empty; Wall ]
          [Wall;  Wall;  Empty; Exit ]]
         |> array2D

    // sample 4x4 maze solution
    let solution4x4 =
        [[X; X; X; O]
         [O; O; X; O]
         [O; O; X; O]
         [O; O; X; X]]
        |> array2D

    test <@ solution4x4 = solve maze4x4 @>


// run the tests
tests ()
