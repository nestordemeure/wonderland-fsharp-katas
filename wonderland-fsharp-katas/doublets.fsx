// See the file doublets.md for detailed information.

open System.IO
open System.Collections.Generic

type Word = string

let wordsPath = Path.Combine (__SOURCE_DIRECTORY__,"resources","words.txt")
let words : Word [] = File.ReadAllLines wordsPath

//-------------------------------------------------------------------------------------------------
// GRAPH

type MutableWord = char []

type Node = 
    { mutable visited : bool ; mutable mutations : MutableWord list }
    /// build an empty node
    static member Create () = { visited = false ; mutations = []}

type Graph = IDictionary<MutableWord,Node>

//-----

/// mutate the word in every possible way and register the mutations that point to existing words
let registerMutations (graph : Graph) (word : MutableWord) =
    let key = Array.copy word
    for i = 0 to word.Length - 1 do 
        let originalLetter = key.[i]
        for c in 'a'..'z' do
            key.[i] <- c
            if (c <> originalLetter) && (graph.ContainsKey key) then
                let node = graph.[key]
                node.mutations <- word::node.mutations
        key.[i] <- originalLetter

//-----

/// take an array of words and returns a graph with each word linked to his mutations
let createGraph (words : Word []) =
    let mutableWords = words |> Array.map (fun w -> w.ToCharArray())
    let graph = mutableWords |> Array.map (fun m -> m, Node.Create()) |> dict
    Array.iter (registerMutations graph) mutableWords
    graph

//-------------------------------------------------------------------------------------------------
// SOLUTION

/// unit pipe
let inline (|->) x f = f x ; x

/// returns the path to go from a position (included) to a destination
/// if it does not exist, returns None
let rec searchPath (graph : Graph) destination position =
    if position = destination then 
        Some [position]
    else 
        let exist, node = graph.TryGetValue position
        if exist && not node.visited then
            node.visited <- true 
            let path = List.tryPick (searchPath graph destination) node.mutations
            match path with 
            | None -> None 
            | Some p -> Some (position::p)
        else None

//-----

/// take two words and returns a list of steps to go from one to the other
/// returns [] if there is no known path
let doublets (w1:Word,w2:Word) = 
    // we could avoid recomputing the graph by storing the name of the testedKeys in a separate datastructure
    let graph = createGraph words 
    let m1 = w1.ToCharArray()
    let m2 = w2.ToCharArray()
    match searchPath graph m2 m1 with 
    | None -> []
    | Some path -> path |> List.map System.String.Concat |-> printfn "%A"

//-------------------------------------------------------------------------------------------------
// TEST

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    test <@ doublets ("head", "tail") = ["head"; "heal"; "teal"; "tell"; "tall"; "tail"] @>
    test <@ doublets ("door", "lock") = ["door"; "boor"; "book"; "look"; "lock"] @>
    test <@ doublets ("bank", "loan") = ["bank"; "bonk"; "book"; "look"; "loon"; "loan"] @>
    test <@ doublets ("wheat", "bread") = ["wheat"; "cheat"; "cheap"; "cheep"; "creep"; "creed"; "breed"; "bread"] @>

    test <@ doublets ("ye", "freezer") |> List.isEmpty @>

// run the tests
tests ()
