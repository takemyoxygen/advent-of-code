#load "utils.fsx"

open System

let lines = Utils.readLines 7 false

let handTypes =
  [| [ 5 ]
     [ 4; 1 ]
     [ 3; 2 ]
     [ 3; 1; 1 ]
     [ 2; 2; 1 ]
     [ 2; 1; 1; 1 ]
     [ 1; 1; 1; 1; 1 ] |]

let input =
  lines
  |> Seq.map (fun line ->
    let [| hand; bid |] = line.Split(" ")
    hand, Int32.Parse(bid))
  |> List.ofSeq


let countCards (hand: string) =
  hand |> Seq.countBy id |> Seq.sortByDescending snd |> List.ofSeq


let compareByOrder order val1 val2 =
  let o1, o2 = Array.IndexOf(order, val1), Array.IndexOf(order, val2)
  o2.CompareTo(o1) // order array assumes that larger items have smaller indexes


let compareCards cardTypes h1 h2 =
  Seq.zip h1 h2
  |> Seq.map (fun (c1, c2) -> compareByOrder cardTypes c1 c2)
  |> Seq.tryFind ((<>) 0)
  |> Option.defaultValue 0


let compareHands cardTypes handCode h1 h2 =
  match compareByOrder handTypes (handCode h1) (handCode h2) with
  | 0 -> compareCards cardTypes h1 h2
  | x -> x

let part1 () =
  let defaultCardTypes =
    [| 'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2' |]

  let handCodeNoJoker hand = countCards hand |> List.map snd

  input
  |> Seq.sortWith (fun (h1, _) (h2, _) ->
    (compareHands defaultCardTypes handCodeNoJoker h1 h2))
  |> Seq.mapi (fun i (_, bid) -> (i + 1) * bid)
  |> Seq.sum

let part2 () =
  let jokerCardTypes =
    [| 'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J' |]

  let handCodeWithJoker hand =
    let countedCards = countCards hand |> Array.ofSeq

    match Array.tryFindIndex (fst >> ((=) 'J')) countedCards with
    | Some(jokerIndex) ->
      match Array.tryFindIndex (fst >> ((<>) 'J')) countedCards with
      | Some(idxToMergeJoker) ->
        let (card, count) = countedCards.[idxToMergeJoker]
        let jokerCount = snd countedCards.[jokerIndex]
        countedCards.[idxToMergeJoker] <- (card, count + jokerCount)
        Array.removeAt jokerIndex countedCards
      | _ -> countedCards
    | None -> countedCards
    |> List.ofSeq
    |> List.map snd

  input
  |> Seq.sortWith (fun (h1, _) (h2, _) ->
    (compareHands jokerCardTypes handCodeWithJoker h1 h2))
  |> Seq.mapi (fun i (_, bid) -> (i + 1) * bid)
  |> Seq.sum



printfn "Day 7"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
