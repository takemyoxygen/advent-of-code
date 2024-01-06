#load "utils.fsx"

open System
open System.Text.RegularExpressions
open Utils

type Point = { X: int; Y: int; Z: int }
type Brick = { P1: Point; P2: Point; ID: string }


let input =
  Input.readLines 22 false
  |> Seq.map (fun line ->
    Regex.Matches(line, "\d+")
    |> Seq.map (fun m -> Int32.Parse m.Value)
    |> List.ofSeq)
  |> Seq.mapi (fun idx [ x1; y1; z1; x2; y2; z2 ] ->
    { P1 = { X = x1; Y = y1; Z = z1 }
      P2 = { X = x2; Y = y2; Z = z2 }
      ID = (int 'A') + idx |> char |> _.ToString() })
  |> List.ofSeq


let intersect f b1 b2 =
  not ((f b1.P2) < (f b2.P1) || (f b2.P2) < (f b1.P1))


let woudlLayOn b1 b2 =
  (intersect (_.X) b1 b2) && (intersect (_.Y) b1 b2)


let bricksBelow =
  input
  |> List.sortBy (_.P1.Z)
  |> Seq.fold
    (fun (levels, support) brick ->
      let prevLevelKey = -(brick.P1.Z - 1)

      let baseLevel, supportedBy =
        levels
        |> Map.toSeq
        |> Seq.filter (fun (levelKey, _) -> levelKey >= prevLevelKey)
        |> Seq.map (fun (levelKey, bricks) ->
          let overlapping = List.filter (woudlLayOn brick) bricks
          levelKey, overlapping)
        |> Seq.tryFind (fun (_, bricks) -> not <| List.isEmpty bricks)
        |> Option.defaultWith (fun () -> 0, [])

      let levels' =
        Map.addOrUpdate
          (fun ex -> brick :: ex)
          (fun () -> [ brick ])
          (baseLevel - (brick.P2.Z - brick.P1.Z + 1))
          levels

      let support' = Map.add brick.ID (List.map (_.ID) supportedBy) support

      levels', support')
    (Map.empty, Map.empty)
  |> snd


let bricksAbove =
  bricksBelow
  |> Map.toSeq
  |> Seq.collect (fun (above, below) -> Seq.map (fun b -> b, above) below)
  |> Seq.groupBy fst
  |> Seq.map (fun (below, pairs) -> below, pairs |> Seq.map snd |> List.ofSeq)
  |> Map.ofSeq


let part1 () =
  bricksBelow
  |> Map.values
  |> Seq.choose (function
    | [ x ] -> Some(x)
    | _ -> None)
  |> set
  |> Set.count
  |> (fun cannotRemove -> input.Length - cannotRemove)


let findFallenAfterDesintegration desintegrated =
  let above brick =
    bricksAbove |> Map.tryFind brick |> Option.defaultValue List.empty

  let rec findFalls fallen next =
    if Set.isEmpty next then
      fallen
    else
      let wouldFall =
        next
        |> Seq.choose (fun brick ->
          bricksBelow
          |> Map.tryFind brick
          |> Option.defaultValue List.empty
          |> List.forall (fun below -> Set.contains below fallen)
          |> (fun f -> if f then Some(brick) else None))

      let next' = wouldFall |> Seq.collect above |> set

      let fallen' = wouldFall |> set |> Set.union fallen

      findFalls fallen' next'

  findFalls (set [ desintegrated ]) (above desintegrated |> set)
  |> Set.remove desintegrated


let part2 () =
  input
  |> Seq.sumBy (fun brick ->
    findFallenAfterDesintegration brick.ID |> Set.count)


printfn "Day 22"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
