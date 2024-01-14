#load "utils.fsx"
#r "nuget: FSharp.Stats"

open System
open System.Text.RegularExpressions
open FSharp.Stats
open FSharp.Stats.Algebra

open Utils

type Vec3 = { X: float; Y: float; Z: float }
type Hailstone = { Position: Vec3; Velocity: Vec3 }

let hailstones =
  Input.readLines 24 false
  |> Seq.map (fun line ->
    Regex.Matches(line, "-?\d+")
    |> Seq.map (fun m -> Double.Parse m.Value)
    |> List.ofSeq)
  |> Seq.mapi (fun idx [ x; y; z; vx; vy; vz ] ->
    { Position = { X = x; Y = y; Z = z }
      Velocity = { X = vx; Y = vy; Z = vz } })
  |> Array.ofSeq


let precision = 5

let position2DAfter h (time: float) =
  h.Position.X + time * h.Velocity.X, h.Position.Y + time * h.Velocity.Y

let position3DAfter h (time: float) =
  Math.Round(h.Position.X + time * h.Velocity.X, precision),
  Math.Round(h.Position.Y + time * h.Velocity.Y, precision),
  Math.Round(h.Position.Z + time * h.Velocity.Z, precision)


let solve2D fs h1 h2 =
  let A =
    fs |> Seq.map (fun f -> [ (f h1.Velocity); -(f h2.Velocity) ]) |> matrix

  let b = fs |> Seq.map (fun f -> (f h2.Position) - (f h1.Position)) |> vector

  LinearAlgebra.SolveLinearSystem A b

let intersection2D h1 h2 =
  let ts = solve2D [ _.X; _.Y ] h1 h2

  if Vector.exists (fun t -> Double.IsInfinity(t) || t < 0) ts then
    None
  else
    let x, y = position2DAfter h1 ts[0]
    Some(x, y, ts[0], ts[1])


let intersection3D h1 h2 =
  match intersection2D h1 h2 with
  | Some(_, _, t1, t2) ->
    let (x, y, z) as p1 = position3DAfter h1 t1

    if p1 = position3DAfter h2 t2 then
      Some(x, y, z, t1, t2)
    else
      None
  | _ -> None


let inArea coord =
  (float 200000000000000I) <= coord && coord <= (float 400000000000000I)


let allPairs =
  seq {
    for i in 0 .. hailstones.Length - 2 do
      for j in i + 1 .. hailstones.Length - 1 do
        yield hailstones[i], hailstones[j]
  }

let part1 () =
  allPairs
  |> Seq.choose (fun (h1, h2) -> intersection2D h1 h2)
  |> Seq.filter (fun (x, y, _, _) -> (inArea x) && inArea y)
  |> Seq.length

printfn "Day 24, part 1: %A" <| part1 ()
