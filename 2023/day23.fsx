#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open Utils
open System
open FSharpx.Collections

let grid = Input.readLines 23 false |> Grid.ofLines

let start, destination =
  let freeInRow rowIdx =
    Grid.row rowIdx grid
    |> Seq.findIndex ((=) '.')
    |> (fun i -> { Row = rowIdx; Col = i })

  freeInRow 0, freeInRow <| Grid.rowsCount grid - 1


let part1 () =
  let slopes = Map [ '>', Right; '<', Left; '^', Up; 'v', Down ]

  let rec dfs next best =
    match next with
    | [] -> best
    | (_, pos, path) :: rest when pos = destination -> dfs rest (max path best)
    | (visited, pos, path) :: rest ->
      let nextPositions =
        Grid.at pos grid
        |> (fun ch -> Map.tryFind ch slopes)
        |> Option.map (fun x -> [| x |])
        |> Option.defaultValue Direction.all
        |> Seq.map (fun dir -> Direction.move pos dir)
        |> Seq.filter (fun pos -> not <| Set.contains pos visited)
        |> Seq.filter (fun pos ->
          match Grid.tryAt pos grid with
          | None
          | Some('#') -> false
          | _ -> true)

      let visited' = Set.add pos visited

      let next' =
        nextPositions
        |> Seq.fold (fun acc pos -> (visited', pos, path + 1) :: acc) rest

      dfs next' best

  dfs [ (Set.empty, start, 0) ] Int32.MinValue


let isAvailable pos =
  Grid.tryAt pos grid |> Option.defaultValue '#' <> '#'


let condenseGraph condensedNodes =
  let recordDistance p1 p2 dist distances =
    distances
    |> Map.addOrUpdate (fun ex -> (p2, dist) :: ex) (fun () -> [ p2, dist ]) p1
    |> Map.addOrUpdate (fun ex -> (p1, dist) :: ex) (fun () -> [ p1, dist ]) p2


  let findPathsFrom start =
    let rec loop visited paths queue =
      match queue with
      | Queue.Nil -> paths
      | Queue.Cons((pos, _), rest) when Set.contains pos visited ->
        loop visited paths rest
      | Queue.Cons((pos, path), rest) when
        Set.contains pos condensedNodes && path > 0
        ->
        loop visited (Map.add pos path paths) rest
      | Queue.Cons((pos, path), rest) ->
        Direction.all
        |> Seq.map (fun dir -> Direction.move pos dir)
        |> Seq.filter (fun pos ->
          not <| Set.contains pos visited && isAvailable pos)
        |> Seq.fold (fun q pos -> Queue.conj (pos, path + 1) q) rest
        |> loop (Set.add pos visited) paths

    loop Set.empty Map.empty (Queue.ofList [ (start, 0) ])


  condensedNodes
  |> Seq.collect (fun node ->
    findPathsFrom node
    |> Map.toSeq
    |> Seq.map (fun (dest, path) -> (minmax node dest), path))
  |> Seq.distinct
  |> Seq.fold
    (fun acc ((p1, p2), dist) -> recordDistance p1 p2 dist acc)
    Map.empty


let findLongestPath graph =
  let rec loop best next =
    match next with
    | [] -> best
    | (_, node, dist) :: rest when node = destination ->
      loop (max best dist) rest
    | (visited, node, dist) :: rest ->
      let visited' = Set.add node visited

      graph
      |> Map.find node
      |> Seq.filter (fun (nextNode, _) -> not <| Set.contains nextNode visited)
      |> Seq.fold
        (fun acc (nextNode, edge) -> (visited', nextNode, dist + edge) :: acc)
        rest
      |> loop best

  loop Int32.MinValue [ Set.empty, start, 0 ]

let part2 () =
  let isDiversion pos =
    Direction.all
    |> Seq.filter (fun dir -> Direction.move pos dir |> isAvailable)
    |> Seq.length > 2

  let condensedNodes =
    Grid.positions grid
    |> Seq.filter (fun pos -> isAvailable pos && isDiversion pos)
    |> set
    |> Set.union (set [ start; destination ])

  condensedNodes |> condenseGraph |> findLongestPath


printfn "Day 23"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
