// See the file wonderland-number.md for detailed information.

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// unit pipe
let (|->) x f = f x ; x

let wonderlandNumber () =
    let targets = seq { 100000..999999 }
    let digits n = string n |> Set.ofSeq
    let tests n =
        let digitsN = digits n
        digitsN = digits (n*2) 
        && digitsN = digits (n*3) 
        && digitsN = digits (n*4) 
        && digitsN = digits (n*5) 
        && digitsN = digits (n*6)
    targets |> Seq.find tests |-> printfn "%d"

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let haveSameDigits (n1:int,n2:int) =
    (string n1 |> Set.ofSeq) = (string n2 |> Set.ofSeq)

let tests () =

    let wonderNum = wonderlandNumber ()

    test <@ (string wonderNum).Length = 6 @>

    test <@ haveSameDigits (wonderNum, 2 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 3 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 4 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 5 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 6 * wonderNum) @>

// run the tests
tests ()
