#load "utils.fsx"

open System.Text.RegularExpressions
open Utils
open System

let edges =
  Input.readLines 25 false
  |> Seq.collect (fun line ->
    let node :: adjs =
      Regex.Matches(line, "[a-z]+") |> Seq.map _.Value |> List.ofSeq

    adjs |> Seq.map (fun adj -> node, adj))
  |> Array.ofSeq

let fromEdges edges =
  edges
  |> Seq.collect (fun (a, b) -> [ a, b; b, a ])
  |> Seq.groupBy fst
  |> Seq.map (fun (a, bs) -> a, bs |> Seq.map snd |> List.ofSeq)
  |> Map.ofSeq

let graph = fromEdges edges


let contract graph u v (state: Map<string, Set<string>>) =
  let uv = (state.Count + 1).ToString()

  let uc = state |> Map.tryFind u |> Option.defaultValue (set [ u ])
  let vc = state |> Map.tryFind v |> Option.defaultValue (set [ v ])
  let state' = Map.add uv (Set.union uc vc) state

  let uAdj = Map.find u graph
  let vAdj = Map.find v graph

  let replace = List.map (fun n -> if n = u || n = v then uv else n)

  let graph' =
    uAdj
    |> Seq.fold
      (fun gr node ->
        let adjs = gr |> Map.find node |> replace
        Map.add node adjs gr)
      graph

  let graph' =
    vAdj
    |> Seq.fold
      (fun gr node ->
        let adjs = gr |> Map.find node |> replace
        Map.add node adjs gr)
      graph'

  let combinedAdj = (vAdj @ uAdj) |> replace |> List.filter ((<>) uv)

  graph' |> Map.remove u |> Map.remove v |> Map.add uv combinedAdj, state'


let randomFrom xs =
  let xsArr = Array.ofSeq xs
  let i = Random.Shared.Next(xsArr.Length)
  xsArr[i]

let randomEdge graph =
  let node = graph |> Map.keys |> randomFrom
  let adj = graph |> Map.find node |> randomFrom
  node, adj


// https://en.wikipedia.org/wiki/Karger%27s_algorithm
let rec karger graph state =
  if Map.count graph > 2 then
    let u, v = randomEdge graph
    let graph', state' = contract graph u v state
    karger graph' state'
  else
    graph, state


let part1 () =
  Seq.initInfinite id
  |> Seq.choose (fun _ ->
    let graph', st = karger graph Map.empty
    let cut = graph' |> Map.values |> Seq.head |> List.length

    if cut = 3 then
      graph'
      |> Map.keys
      |> Seq.map (fun n -> st[n].Count)
      |> Seq.fold (*) 1
      |> Some
    else
      None)
  |> Seq.head

printfn "Day 25"
printfn "Part 1: %A" <| part1 ()
