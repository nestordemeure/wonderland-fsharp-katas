// See the file wonderland-number.md for detailed information.

/// unit pipe
let (|->) x f = f x ; x

/// bruteforce solution
let wonderlandNumber () =
    let targets = seq { 100000..999999 }
    let digits n = string n |> Set.ofSeq
    let tests n =
        let nDig = digits n
        nDig = digits (n*2) && 
        nDig = digits (n*3) && 
        nDig = digits (n*4) && 
        nDig = digits (n*5) && 
        nDig = digits (n*6)
    targets |> Seq.find tests |-> printfn "%d"

let haveSameDigits (n1:int,n2:int) =
    (string n1 |> Set.ofSeq) = (string n2 |> Set.ofSeq)

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

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
