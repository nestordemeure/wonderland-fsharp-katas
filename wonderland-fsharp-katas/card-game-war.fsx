// See the file card-game.md for detailed information.

type Winner =
    | Player1 
    | Player2
    | Equality

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Rank * Suit

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// returns true if the first player wins
let playRound (card1:Card,card2:Card) = card1 > card2

/// returns the identity of the payer who wins the hand
let rec playGame (hand1:Card list, hand2:Card list) =
    match hand1,hand2 with 
    | [], [] -> Equality
    | [], _ -> Player2
    | _, [] -> Player1
    // player 1 wins the round
    | card1::h1, card2::h2 when playRound(card1, card2) -> 
        let newHand1 = h1 @ [card1;card2]
        playGame (newHand1 , h2)
    // player 2 wins the round
    | card1::h1, card2::h2 ->
        let newHand2 = h2 @ [card1;card2]
        playGame (h1 , newHand2)

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

(*
let suits = [ Spade; Club; Diamond; Heart ]
let heads = [ Jack; Queen; King; Ace ]

let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]

let deck = seq {
    for suit in suits do
        for rank in ranks -> suit,rank }
*)

let tests () =

    // playRound
    // "the highest rank wins the cards in the round"
    test <@ playRound ( (Value 10,Club) , (Value 5,Club) ) @>
    // "queens are higher rank than jacks"
    test <@ playRound ( (Queen,Club) , (Jack,Club) ) @>
    // "kings are higher rank than queens"
    test <@ playRound ( (King,Heart) , (Queen,Heart) ) @>
    // "aces are higher rank than kings"
    test <@ playRound ( (Ace,Spade) , (King,Spade) ) @>
    // "if the ranks are equal, clubs beat spades"
    test <@ playRound ( (Value 2,Club) , (Value 2,Spade) ) @>
    // "if the ranks are equal, diamonds beat clubs"
    test <@ playRound ( (Value 5,Diamond) , (Value 5,Club) ) @>
    // "if the ranks are equal, hearts beat diamonds"
    test <@ playRound ( (Ace,Heart) , (Ace,Diamond) ) @>

    // playGame
    // "the player loses when they run out of cards"
    test <@ playGame ( [(Ace,Heart)] , [] ) = Player1 @>

// run the tests
tests ()
