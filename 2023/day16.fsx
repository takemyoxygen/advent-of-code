#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open FSharpx.Collections
open Utils

type Direction =
  | Left
  | Right
  | Up
  | Down

let steps = Map [ Left, (-1, 0); Right, (1, 0); Up, (0, -1); Down, (0, 1) ]

let move { Col = c; Row = r } dir =
  let dc, dr = Map.find dir steps
  { Col = c + dc; Row = r + dr }

let grid = Input.readLines 16 false |> Grid.ofLines

let canPassThrough dir sym =
  match dir, sym with
  | _, '.'
  | (Up | Down), '|'
  | (Left | Right), '-' -> true
  | _ -> false

let processBeam grid start direction =
  let enqueue posDirs queue =
    posDirs
    |> Seq.filter (fun (p, _) -> Grid.isWithin p grid)
    |> Seq.fold (fun q p -> Queue.conj p q) queue

  let rec loop visited queue =
    match
      Queue.tryUncons queue
      |> Option.map (fun ((pos, dir), q) -> pos, Grid.at pos grid, dir, q)
    with
    | None -> visited
    | Some(pos, sym, dir, queue') when Set.contains (pos, dir) visited ->
      loop visited queue'
    | Some(pos, sym, dir, queue') when canPassThrough dir sym ->
      loop (Set.add (pos, dir) visited) (enqueue [ move pos dir, dir ] queue')
    | Some(pos, '/', Right, queue') ->
      loop (Set.add (pos, Right) visited) (enqueue [ move pos Up, Up ] queue')
    | Some(pos, '/', Left, queue') ->
      loop
        (Set.add (pos, Left) visited)
        (enqueue [ move pos Down, Down ] queue')
    | Some(pos, '/', Up, queue') ->
      loop
        (Set.add (pos, Up) visited)
        (enqueue [ move pos Right, Right ] queue')
    | Some(pos, '/', Down, queue') ->
      loop
        (Set.add (pos, Down) visited)
        (enqueue [ move pos Left, Left ] queue')
    | Some(pos, '\\', Right, queue') ->
      loop
        (Set.add (pos, Right) visited)
        (enqueue [ move pos Down, Down ] queue')
    | Some(pos, '\\', Left, queue') ->
      loop (Set.add (pos, Left) visited) (enqueue [ move pos Up, Up ] queue')
    | Some(pos, '\\', Down, queue') ->
      loop
        (Set.add (pos, Down) visited)
        (enqueue [ move pos Right, Right ] queue')
    | Some(pos, '\\', Up, queue') ->
      loop (Set.add (pos, Up) visited) (enqueue [ move pos Left, Left ] queue')
    | Some(pos, '-', (Up | Down as dir), queue') ->
      loop
        (Set.add (pos, dir) visited)
        (enqueue [ move pos Left, Left; move pos Right, Right ] queue')
    | Some(pos, '|', (Left | Right as dir), queue') ->
      loop
        (Set.add (pos, dir) visited)
        (enqueue [ move pos Up, Up; move pos Down, Down ] queue')
    | Some(p) -> failwithf "Unrecognized pattern: %A" p

  loop Set.empty (Queue.ofList [ start, direction ]) |> Set.map fst |> Set.count


let part1 () =
  processBeam grid { Col = 0; Row = 0 } Right


let part2 () =
  let topRow =
    (seq { 0 .. Grid.columnsCount grid - 1 }
     |> Seq.map (fun col -> { Row = 0; Col = col }, Down))

  let rowsCount = Grid.rowsCount grid

  let bottomRow =
    (seq { 0 .. Grid.columnsCount grid - 1 }
     |> Seq.map (fun col -> { Row = rowsCount - 1; Col = col }, Up))

  let leftColumn =
    (seq { 0 .. Grid.rowsCount grid - 1 }
     |> Seq.map (fun row -> { Row = row; Col = 0 }, Right))

  let colsCount = Grid.columnsCount grid

  let rightColumn =
    (seq { 0 .. Grid.rowsCount grid - 1 }
     |> Seq.map (fun row -> { Row = row; Col = colsCount - 1 }, Left))

  Seq.concat [ topRow; bottomRow; leftColumn; rightColumn ]
  |> Seq.map (fun (pos, dir) -> async { return processBeam grid pos dir })
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Seq.max

printfn "Day 16"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
