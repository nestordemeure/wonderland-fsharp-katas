// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

//-------------------------------------------------------------------------------------------------
// EXTENSIONS

///unit pipe
let inline (|->) x f = f x ; x

module Array =
    /// test a predicate on all the value of an array and their index
    let foralli f (a : 'T []) =
        let mutable cont = true
        let mutable i = 0
        while cont && (i < a.Length) do 
            cont <- f i a.[i]
            i <- i+1
        cont

//-------------------------------------------------------------------------------------------------
// TABLES

/// take two chars and returns the encoded char
let substitutionChart (cMessage:char) (cKey:char) =
    let ia = int 'a'
    let iMessage = (int cMessage) - ia
    let iKey = (int cKey) - ia
    let letters = [| 'a'..'z' |]
    letters.[ (iKey+iMessage) % 26 ]

/// take two chars and returns the decoded char
let unSubstitutionChart (cMessage:char) (cKey:char) =
    let ia = int 'a'
    let iMessage = (int cMessage) - ia
    let iKey = (int cKey) - ia
    let letters = [| 'a'..'z' |]
    letters.[ (iMessage-iKey+26) % 26 ]

/// take two chars and returns the key char
let unCipherChart (cMessage:char) (cCipher:char) =
    let ia = int 'a'
    let iMessage = (int cMessage) - ia
    let iCipher = (int cCipher) - ia
    let letters = [| 'a'..'z' |]
    letters.[ (iCipher-iMessage+26) % 26 ]

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// look for the shortest substring that is repeated across the array
let rec findPeriod period arr =
    if period = Array.length arr then period
    elif Array.foralli (fun i c -> c = arr.[i % period ]) arr then period
    else findPeriod (period+1) arr

//-----

let encode (key:Keyword) (message:Message) : Message =
    Array.init message.Length (fun i -> substitutionChart message.[i] key.[i % key.Length])
    |> System.String.Concat
    |-> printfn "%s"

let decode (key:Keyword) (message:Message) : Message =
    Array.init message.Length (fun i -> unSubstitutionChart message.[i] key.[i % key.Length])
    |> System.String.Concat
    |-> printfn "%s"

let decipher (cipher:Message) (message:Message) : Keyword =
    let repeatedKey = Array.init message.Length (fun i -> unCipherChart message.[i] cipher.[i])
    let period = findPeriod 1 repeatedKey
    repeatedKey.[0..(period-1)]
    |> System.String.Concat
    |-> printfn "%s"

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
