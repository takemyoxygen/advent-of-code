module Utils

open System

module Input =
  open System.IO

  let readLines (day: int) (test: bool) =
    let inputFile =
      Path.Combine(
        __SOURCE_DIRECTORY__,
        "input",
        sprintf "day%d%s.txt" day (if test then "-test" else "")
      )

    File.ReadAllLines inputFile

module Seq =
  let all pred xs = xs |> Seq.exists (pred >> not) |> not

module String =

  let tryAt idx (s: string) =
    if 0 <= idx && idx < s.Length then Some(s[idx]) else None

  let join (separator: string) (items: 'a seq) = String.Join(separator, items)

module List =
  let repeat n xs =
    xs |> List.replicate n |> List.collect id

module Array =
  let tryAt idx (xs: 'a array) =
    if 0 <= idx && idx < xs.Length then Some(xs[idx]) else None


type Grid<'a> = Grid of array<array<'a>>
type GridPos = { Row: int; Col: int }

module Grid =
  let create items = Grid items

  let ofLines (lines: string seq) =
    lines |> Seq.map _.ToCharArray() |> Array.ofSeq |> create

  let column colIdx (Grid content) =
    seq { 0 .. content.Length - 1 }
    |> Seq.map (fun rowIdx -> content[rowIdx][colIdx])
    |> List.ofSeq

  let row rowIdx (Grid content) = content[rowIdx] |> List.ofArray

  let columnsCount (Grid content) = content[0].Length

  let rowsCount (Grid content) = content.Length

  let columns grid =
    seq { 0 .. (columnsCount grid) - 1 } |> Seq.map (fun idx -> column idx grid)

  let rows (Grid content) = content |> Seq.map (List.ofArray)

  let updateWith pos update (Grid content) =
    let newRow =
      Array.updateAt
        pos.Col
        (update <| content[pos.Row][pos.Col])
        content[pos.Row]

    content |> Array.updateAt pos.Row newRow |> create

  let positions grid =
    seq {
      for row in seq { 0 .. rowsCount grid - 1 } do
        for col in seq { 0 .. columnsCount grid - 1 } do
          yield { Col = col; Row = row }
    }

  let tryAt pos (Grid content) =
    content |> Array.tryAt pos.Row |> Option.bind (Array.tryAt pos.Col)

  let at pos grid = tryAt pos grid |> Option.get

  let pick pred grid =
    grid |> positions |> Seq.filter (fun pos -> at pos grid |> pred) |> set

  let toString fmt grid =
    rows grid
    |> Seq.mapi (fun rowIdx row ->
      row
      |> Seq.mapi (fun colIdx x -> fmt x { Col = colIdx; Row = rowIdx })
      |> String.join "")
    |> String.join Environment.NewLine

  let isWithin { Row = r; Col = c } (Grid content) =
    0 <= r && r < content.Length && 0 <= c && c < content[r].Length


type Direction =
  | Left
  | Right
  | Up
  | Down

// directions in clockwise order
let private steps =
  [| Left, (-1, 0); Up, (0, -1); Right, (1, 0); Down, (0, 1) |]

let private stepsMap = Map steps

module Direction =
  let all = steps |> Array.map fst

  let move { Col = c; Row = r } dir =
    let dc, dr = Map.find dir stepsMap
    { Col = c + dc; Row = r + dr }

  let movex { Col = c; Row = r } dir n =
    let dc, dr = Map.find dir stepsMap
    { Col = c + dc * n; Row = r + dr * n }

  let private dirIndex dir =
    let idx = Array.findIndex (fst >> ((=) dir)) steps

    if idx < 0 then
      invalidArg "dir" "Invalid direction"

    idx

  let clockwise dir =
    steps[((dirIndex dir) + 1) % steps.Length] |> fst

  let counterclockwise dir =
    steps[((dirIndex dir) - 1 + steps.Length) % steps.Length] |> fst


module Fun =
  let repeatSeq f init =
    Seq.unfold
      (fun curr ->
        let next = f curr
        Some(next, next))
      init

  let repeatn f init times =
    let rec loop interm cnt =
      if cnt = times then interm else loop (f interm) (cnt + 1)

    loop init 0


module Num =
  let rec gcd (a: bigint) (b: bigint) =
    match (a, b) with
    | (x, y) when x = y -> x
    | (x, y) when x > y -> gcd (x - y) y
    | (x, y) -> gcd x (y - x)

  let lcm a b = a * b / (gcd a b)

  // Euclidian remainer, always positive
  let reme a b = ((a % b) + b) % b


module Map =
  let addOrUpdate update add key map =
    let newval =
      match Map.tryFind key map with
      | Some(existing) -> update existing
      | None -> add ()

    Map.add key newval map

  let combine f m1 m2 =
    m1
    |> Map.toSeq
    |> Seq.fold (fun res (k, v) -> addOrUpdate (f v) (fun () -> v) k res) m2


[<AutoOpen>]
module Common =
  let minmax x1 x2 = (min x1 x2), (max x1 x2)
