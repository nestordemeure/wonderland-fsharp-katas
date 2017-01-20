// See the file fox-goose-bag-of-corn.md for detailed information.

type Animal = F | G | C | Y

type Location =
    | LeftBank
    | RightBank
    | Boat
    /// returns the opposite bank of the river
    member this.Opposite =
        match this with 
        | LeftBank -> RightBank
        | RightBank -> LeftBank
        | Boat -> failwith "the boat stays where it is"

type Positions = {
    Fox:    Location
    Goose:  Location
    Corn:   Location
    You:    Location }

let Start = {
    Fox =   LeftBank
    Goose = LeftBank
    Corn =  LeftBank
    You =   LeftBank }

let Final = {
    Fox =   RightBank
    Goose = RightBank
    Corn =  RightBank
    You =   RightBank }

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// unit pipe
let inline (|->) x f = f x ; x

/// a memory of all visited states
let knownPositions = System.Collections.Generic.Dictionary<Positions,bool>()

/// checks to makes sure that nobody is eating someone and that we have not tried that position yet
let isLegal position =
    (not <| knownPositions.ContainsKey position)
    &&
    (position.You = position.Fox
    || position.Fox <> position.Goose)
    &&
    (position.You = position.Goose
    || position.Goose <> position.Corn)

/// returns (you in a boat with an animal , the resulting new legal position) 
/// or None if the move is not legal
let traverse position animal =
    match animal with 
    | F when position.Fox = position.You -> 
        let newPosition = {position with Fox = position.Fox.Opposite ; You = position.You.Opposite}
        if isLegal newPosition then 
            Some ({position with Fox = Boat ; You = Boat}, newPosition)
        else None
    | G when position.Goose = position.You -> 
        let newPosition = {position with Goose = position.Goose.Opposite ; You = position.You.Opposite}
        if isLegal newPosition then 
            Some ({position with Goose = Boat ; You = Boat}, newPosition)
        else None
    | C when position.Corn = position.You -> 
        let newPosition = {position with Corn = position.Corn.Opposite ; You = position.You.Opposite}
        if isLegal newPosition then 
            Some ({position with Corn = Boat ; You = Boat}, newPosition)
        else None
    | Y -> 
        let newPosition = {position with You = position.You.Opposite}
        if isLegal newPosition then 
            Some ({position with You = Boat}, newPosition)
        else None
    | _ -> None

//-----

/// returns the list of steps needed to get, from this position included, to the Final position
/// if it is doable, otherwise returns None
let rec getToTheEnd position =
    knownPositions.[position] <- true
    if position = Final then Some [position] else
        let legalMoves = List.choose (traverse position) [F; G; C; Y] 
        let rec testAllMoves moves =
            match moves with 
            | [] -> None
            | (boat,nextPosition)::q -> 
                match getToTheEnd nextPosition with 
                | None -> testAllMoves q
                | Some l -> Some (position::boat::l)
        testAllMoves legalMoves

//-----

let riverCrossingPlan () : Positions list = 
    knownPositions.Clear()
    match getToTheEnd Start with
    | None -> failwith "no solution found"
    | Some solution -> solution |-> printfn "%A"

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let invalidTransitions =
    [
        LeftBank,RightBank
        RightBank,LeftBank
    ]

let isValidTransition transition =
    invalidTransitions
    |> Seq.exists ((=) transition)
    |> not


let tests () =

    let plan = riverCrossingPlan ()
    let everyone positions =
        [ positions.You; positions.Fox; positions.Goose; positions.Corn ]

    // TODO: check that things move with you
    // and not on their own?
    let validMove (before:Positions,after:Positions) =
        (everyone before, everyone after)
        ||> List.zip
        |> Seq.forall (isValidTransition)

    // "you begin with the fox, goose and corn on one side of the river"
    test <@ plan.Head = Start @>

    // "you end with the fox, goose and corn on the other side of the river"
    let final = plan |> Seq.last
    test <@ final = { Fox = RightBank ; Goose = RightBank; Corn = RightBank; You = RightBank } @>

    // "things are safe"

    let gooseIsSafe positions =
        (positions.Goose <> positions.Fox)
        || (positions.Goose = positions.You)

    let cornIsSafe positions =
        (positions.Corn <> positions.Goose)
        || (positions.Corn = positions.You)

    let boatIsValid positions =
        let notYouOnBoat =
            [   positions.Goose = Boat
                positions.Fox = Boat
                positions.Corn = Boat ]
            |> Seq.filter id
            |> Seq.length

        match positions.You with
        | Boat -> notYouOnBoat < 2
        | _ -> notYouOnBoat = 0

    plan
    |> List.iter (fun current ->

        // "the fox and the goose should never be left alone together"
        test <@ gooseIsSafe current @>

        // "the goose and the corn should never be left alone together"
        test <@ cornIsSafe current @>

        // "The boat can carry only you plus one other"
        test <@ boatIsValid current @>)

    // move is possible
    plan
    |> Seq.pairwise
    |> Seq.iter (fun (before,after) ->
        test <@ validMove (before, after) @>)

tests ()
