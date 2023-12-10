#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open Utils
open FSharpx.Collections

[<Struct>]
type InbetweenPoint = InbetweenPoint of int * int

let pipeTypes =
  Map
    [ '|', [ 0, -1; 0, 1 ]
      '-', [ -1, 0; 1, 0 ]
      'L', [ 0, -1; 1, 0 ]
      'J', [ -1, 0; 0, -1 ]
      '7', [ 0, 1; -1, 0 ]
      'F', [ 0, 1; 1, 0 ] ]

let isPipeDown pipeType =
  match Map.tryFind pipeType pipeTypes with
  | Some(dirs) -> Seq.exists (snd >> ((=) 1)) dirs
  | _ -> false

let isPipeUp pipeType =
  match Map.tryFind pipeType pipeTypes with
  | Some(dirs) -> Seq.exists (snd >> ((=) -1)) dirs
  | _ -> false

let isPipeRight pipeType =
  match Map.tryFind pipeType pipeTypes with
  | Some(dirs) -> Seq.exists (fst >> ((=) 1)) dirs
  | _ -> false

let isPipeLeft pipeType =
  match Map.tryFind pipeType pipeTypes with
  | Some(dirs) -> Seq.exists (fst >> ((=) -1)) dirs
  | _ -> false

let directions = [ 1, 0; -1, 0; 0, 1; 0, -1 ]

let input = Input.readLines 10 false

let start =
  input
  |> Seq.indexed
  |> Seq.choose (fun (y, line) ->
    match line.IndexOf('S') with
    | -1 -> None
    | x -> Some(x, y))
  |> Seq.head

let withinBounds x y =
  x >= 0 && y >= 0 && y < input.Length && x < input[y].Length

let adjacentLocations x y =
  directions
  |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
  |> Seq.filter (fun (ax, ay) ->
    ax >= 0 && ay >= 0 && ay < input.Length && ax < input[ay].Length)

let adjacentPipes x y =
  match input.[y].[x] with
  | 'S' ->
    adjacentLocations x y
    |> Seq.filter (fun (ax, ay) ->
      pipeTypes.TryFind(input.[ay].[ax])
      |> Option.defaultValue List.empty
      |> Seq.map (fun (dx, dy) -> ax + dx, ay + dy)
      |> Seq.contains (x, y))
  | p -> pipeTypes.[p] |> Seq.map (fun (dx, dy) -> x + dx, y + dy)

let sReplacement =
  let sx, sy = start

  let pipesDirections =
    adjacentPipes sx sy
    |> Seq.map (fun (ax, ay) -> ax - sx, ay - sy)
    |> Seq.sort
    |> List.ofSeq

  pipeTypes
  |> Map.toSeq
  |> Seq.find (fun (k, v) -> (List.sort v) = pipesDirections)
  |> fst

let findLoop () =
  let rec loop path x y =
    if (x, y) = start && (not <| List.isEmpty path) then
      (x, y) :: path
    else
      let ax, ay =
        adjacentPipes x y
        |> Seq.filter (fun adj ->
          (List.isEmpty path) || adj <> (List.head path))
        |> Seq.head

      loop ((x, y) :: path) ax ay

  loop list.Empty (fst start) (snd start)


let part1 () = findLoop().Length / 2


let part2 () =
  let nextUnclassified inout loopPoints =
    seq {
      for y in 0 .. input.Length - 1 do
        for x in 0 .. input[y].Length - 1 do
          if
            not
            <| (Map.containsKey (x, y) inout || Set.contains (x, y) loopPoints)
          then
            yield x, y
    }
    |> Seq.tryHead

  let classify loopCoords points =
    // assume that if any point is on the edge of the map - they are not inside of the loop
    points
    |> Seq.exists (fun (x, y) ->
      x = 0 || y = 0 || y = input.Length - 1 || x = input[y].Length - 1)
    |> not

  let loopPipeAt loopCoords x y =
    if (x, y) = start then sReplacement
    elif Set.contains (x, y) loopCoords then input[y][x]
    else '.'

  let canFlowRight (InbetweenPoint(x, y)) loopCoords =
    match [ loopPipeAt loopCoords x (y - 1); (loopPipeAt loopCoords x y) ] with
    | [ pt; pb ] when isPipeDown pt && isPipeUp pb -> false
    | _ -> true

  let canFlowLeft (InbetweenPoint(x, y)) loopCoords =
    match
      [ loopPipeAt loopCoords (x - 1) (y - 1)
        (loopPipeAt loopCoords (x - 1) y) ]
    with
    | [ pt; pb ] when isPipeDown pt && isPipeUp pb -> false
    | _ -> true


  let canFlowUp (InbetweenPoint(x, y)) loopCoords =
    match
      [ loopPipeAt loopCoords (x - 1) (y - 1)
        (loopPipeAt loopCoords x (y - 1)) ]
    with
    | [ pl; pr ] when isPipeRight pl && isPipeLeft pr -> false
    | _ -> true


  let canFlowBottom (InbetweenPoint(x, y)) loopCoords =
    match [ loopPipeAt loopCoords (x - 1) y; (loopPipeAt loopCoords x y) ] with
    | [ pl; pr ] when isPipeRight pl && isPipeLeft pr -> false
    | _ -> true

  let findBetweenPipes x y loopCoords =
    let inbetweens =
      [ x + 1, y + 1; x, y + 1; x, y; x + 1, y ] |> List.map InbetweenPoint

    let rec loop visitedInbetween visitedPoints queue =
      match Queue.tryUncons queue with
      | None -> visitedPoints
      | Some(p, queue') when Set.contains p visitedInbetween ->
        loop visitedInbetween visitedPoints queue'
      | Some(InbetweenPoint(x, y) as ibw, queue') ->
        let visitedPoints' =
          [ x - 1, y - 1; x, y - 1; x, y; x - 1, y ]
          |> Seq.filter (fun (x, y) -> withinBounds x y)
          |> Seq.filter (fun p -> not <| Set.contains p loopCoords)
          |> Seq.fold (fun s p -> Set.add p s) visitedPoints

        seq {
          if canFlowLeft ibw loopCoords then
            yield InbetweenPoint(x - 1, y)

          if canFlowRight ibw loopCoords then
            yield InbetweenPoint(x + 1, y)

          if canFlowUp ibw loopCoords then
            yield InbetweenPoint(x, y - 1)

          if canFlowBottom ibw loopCoords then
            yield InbetweenPoint(x, y + 1)
        }
        |> Seq.filter (fun (InbetweenPoint(x, y)) -> withinBounds x y)
        |> Seq.filter (fun ibw -> not <| Set.contains ibw visitedInbetween)
        |> Seq.fold (fun q p -> Queue.conj p q) queue'
        |> loop (Set.add ibw visitedInbetween) visitedPoints'

    loop Set.empty Set.empty (Queue.ofList inbetweens)


  let loopCoords = findLoop () |> set

  let rec loop inout =
    match nextUnclassified inout loopCoords with
    | None -> inout
    | Some(x, y) ->
      let group = findBetweenPipes x y loopCoords
      let outcome = classify loopCoords group

      group |> Seq.fold (fun m p -> Map.add p outcome m) inout |> loop

  loop Map.empty |> Map.filter (fun k v -> v) |> Map.keys |> Seq.length


printfn "Day 10"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
