#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open System
open System.Globalization
open FSharpx.Collections
open Utils

let input =
  Input.readLines 18 false
  |> Array.map (fun line ->
    let [| dir; steps; color |] = line.Split(" ")

    let dir' =
      match dir with
      | "R" -> Right
      | "L" -> Left
      | "D" -> Down
      | "U" -> Up
      | _ -> failwithf "Unexpected direction: %s" dir

    dir', Int32.Parse(steps), color.Substring(1, color.Length - 2))
  |> List.ofSeq

let pos col row = { Col = col; Row = row }

let cartesianPosClockwise ({ Col = x; Row = y } as p) prevDir nextDir =
  match (prevDir, nextDir) with
  | Right, Down
  | Down, Right -> pos (x + 1) y
  | Right, Up
  | Up, Right -> p
  | Left, Down
  | Down, Left -> pos (x + 1) (y + 1)
  | Left, Up
  | Up, Left -> pos x (y + 1)
  | x -> failwithf "Unsupported pair of consecutive directions: %A" x


let digCartesian plan =
  let rec loop pos prevDirection plan edge =
    match plan with
    | [] -> edge |> List.rev |> Array.ofList
    | (dir, steps) :: rest ->
      let pos' = Direction.movex pos dir steps
      let cartesian = cartesianPosClockwise pos prevDirection dir
      loop pos' dir rest (cartesian :: edge)

  loop (pos 0 0) (plan |> List.last |> fst) plan List.empty

// Shoelace/Gauss's area formula - https://en.wikipedia.org/wiki/Shoelace_formula
let calcArea (edges: GridPos array) =
  seq { 0 .. edges.Length - 1 }
  |> Seq.sumBy (fun i ->
    let xi = bigint edges[i].Col
    let yip1 = bigint edges[(i + 1) % edges.Length].Row
    let yim1 = bigint edges[(i - 1 + edges.Length) % edges.Length].Row
    xi * (yip1 - yim1))
  |> (fun x -> x / 2I)


let part1 () =
  input
  |> List.map (fun (dir, steps, _) -> dir, steps)
  |> digCartesian
  |> calcArea


let part2 () =
  input
  |> List.map (fun (_, _, color) ->
    let steps = Int32.Parse(color.Substring(1, 5), NumberStyles.HexNumber)

    let dir =
      match color[color.Length - 1] with
      | '0' -> Right
      | '1' -> Down
      | '2' -> Left
      | '3' -> Up
      | x -> failwithf "Invalid direction: %A" x

    dir, steps)
  |> digCartesian
  |> calcArea


printfn "Day 18"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
