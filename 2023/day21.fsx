#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open System
open FSharpx.Collections
open Utils

type Entrypoint =
  | Corner
  | MidEdge

type Coverage =
  | Full
  | Partial of int

type Parity =
  | Even
  | Odd

type Case = Entrypoint * Coverage * Parity

let parity x = if x % 2 = 0 then Even else Odd

let reverseParity p = if p = Even then Odd else Even


let grid = Input.readLines 21 false |> Grid.ofLines
let start = Grid.positions grid |> Seq.find (fun pos -> Grid.at pos grid = 'S')


let distancesFrom start =
  let rec loop distances queue =
    match queue with
    | Queue.Nil -> distances
    | Queue.Cons((_, pos), queue') when Map.containsKey pos distances ->
      loop distances queue'
    | Queue.Cons((path, pos), queue') ->
      Direction.all
      |> Seq.map (Direction.move pos)
      |> Seq.filter (fun next ->
        match Grid.tryAt next grid with
        | Some('S' | '.') -> true
        | _ -> false)
      |> Seq.fold (fun q next -> Queue.conj ((path + 1), next) q) queue'
      |> loop (Map.add pos path distances)

  loop Map.empty (Queue.ofList [ 0, start ])


let findCases steps y =
  let ySteps = if y = 0 then 0 else 66 + 131 * (y - 1)

  let maxX =
    float (steps - ySteps - 66) / 131.0 |> Math.Ceiling |> int |> ((+) 1)

  let rec loop x acc =
    let xSteps = 66 + (x - 1) * 131
    let stepsLeft = steps - xSteps - ySteps

    match stepsLeft with
    | _ when x < 1 -> 0, acc
    | left when left < 0 -> loop (x - 1) acc
    | left when left >= 260 -> x, acc
    | left when left < 260 -> loop (x - 1) ((x, left) :: acc)
    | left -> failwithf "Unexpected # of steps: %d (x = %d, y = %d)" left x y

  if maxX < 1 then
    Map.empty
  else
    let initParity = parity (steps - ySteps)

    let lastFullX, partials = loop maxX List.empty

    let seconds = lastFullX / 2
    let firsts = lastFullX - seconds
    let entry = if y = 0 then MidEdge else Corner

    ((entry, Full, initParity), bigint firsts)
    :: ((entry, Full, reverseParity initParity), bigint seconds)
    :: (List.map
      (fun (x, left) ->
        let key = entry, Partial(left), (parity left)

        key, 1I)
      partials)
    |> Map
    |> Map.filter (fun k v -> v <> 0I)


let countDistances par maxDist distances =
  distances
  |> Map.values
  |> Seq.filter (fun dist -> dist <= maxDist && par = (parity dist))
  |> Seq.length


let countPoints corner midedge (cases: Map<Case, bigint>) =
  let distances =
    Map [ Corner, (distancesFrom corner); MidEdge, (distancesFrom midedge) ]

  cases
  |> Map.toSeq
  |> Seq.sumBy (fun ((entry, coverage, evenodd), count) ->
    let maxDist =
      match coverage with
      | Full -> Int32.MaxValue
      | Partial(d) -> d

    let pointsInOne =
      distances[entry] |> countDistances evenodd maxDist |> bigint

    count * pointsInOne)


let countReachable steps =
  let cases =
    Seq.initInfinite id
    |> Seq.map (findCases steps)
    |> Seq.takeWhile (Map.isEmpty >> not)
    |> Seq.fold (Map.combine (+)) Map.empty

  let quadrants =
    [ { Row = 0; Col = 0 }, { Row = 65; Col = 0 }
      { Row = 0; Col = 130 }, { Row = 0; Col = 65 }
      { Row = 130; Col = 130 }, { Row = 65; Col = 130 }
      { Row = 130; Col = 0 }, { Row = 130; Col = 65 } ]
    |> Seq.sumBy (fun (corner, midedge) -> countPoints corner midedge cases)


  let middle =
    distancesFrom start |> countDistances (parity steps) steps |> bigint

  quadrants + middle

// Works for part 1 but not for part 2. Useful for testing
let slowCountReachable steps =
  let gridPosAt pos =
    let pos' =
      { Row = Num.reme pos.Row (Grid.rowsCount grid)
        Col = Num.reme pos.Col (Grid.columnsCount grid) }

    Grid.at pos' grid

  let rec loop paths queue =
    match queue with
    | Queue.Nil -> paths
    | Queue.Cons((path, _), _) when path > steps -> paths
    | Queue.Cons((_, pos), queue') when Map.containsKey pos paths ->
      loop paths queue'
    | Queue.Cons((path, pos), queue') ->
      Direction.all
      |> Seq.map (Direction.move pos)
      |> Seq.filter (fun next ->
        match gridPosAt next with
        | 'S'
        | '.' -> true
        | _ -> false)
      |> Seq.fold (fun q next -> Queue.conj ((path + 1), next) q) queue'
      |> loop (Map.add pos path paths)

  loop Map.empty (Queue.ofList [ 0, start ])
  |> Map.values
  |> Seq.filter (fun dist -> dist % 2 = steps % 2)
  |> Seq.length


let part1 () = countReachable 64
let part2 () = countReachable 26501365


printfn "Day 21"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
