#load "utils.fsx"

open Utils

let normalizeGrid grid =
  let rocks = Grid.pick ((=) 'O') grid

  let grid' =
    rocks |> Seq.fold (fun g pos -> Grid.updateWith pos (fun _ -> '.') g) grid

  grid', rocks

let grid, rocks = Input.readLines 14 false |> Grid.ofLines |> normalizeGrid


let move edge offset grid rocks =
  let advance ({ Row = r; Col = c }) =
    let { Row = dr; Col = dc } = offset
    { Row = r + dr; Col = c + dc }

  let moveLine start =
    let rec loop freePos currentPos acc =
      let next = advance currentPos

      match Grid.tryAt currentPos grid with
      | None -> acc
      | Some('#') -> loop next next acc
      | _ when Set.contains currentPos rocks ->
        loop (advance freePos) next (Set.add freePos acc)
      | _ -> loop freePos next acc

    loop start start Set.empty

  edge |> Seq.map moveLine |> Set.unionMany

let moveNorth grid =
  move
    (seq { 0 .. Grid.columnsCount grid - 1 }
     |> Seq.map (fun col -> { Row = 0; Col = col }))
    { Row = 1; Col = 0 }
    grid

let moveSouth grid =
  let lastRow = Grid.rowsCount grid - 1

  move
    (seq { 0 .. Grid.columnsCount grid - 1 }
     |> Seq.map (fun col -> { Row = lastRow; Col = col }))
    { Row = -1; Col = 0 }
    grid

let moveWest grid =
  move
    (seq { 0 .. Grid.rowsCount grid - 1 }
     |> Seq.map (fun row -> { Row = row; Col = 0 }))
    { Row = 0; Col = 1 }
    grid

let moveEast grid =
  let lastCol = Grid.columnsCount grid - 1

  move
    (seq { 0 .. Grid.rowsCount grid - 1 }
     |> Seq.map (fun row -> { Row = row; Col = lastCol }))
    { Row = 0; Col = -1 }
    grid

let cycle grid =
  moveNorth grid >> moveWest grid >> moveSouth grid >> moveEast grid

let calculateLoad grid rocks =
  let totalRows = Grid.rowsCount grid
  rocks |> Seq.sumBy (fun pos -> totalRows - pos.Row)

let findPattern minPeriod iterations grid rocks =
  let loads =
    Seq.unfold
      (fun (rocks, counter) ->
        if counter >= iterations then
          None
        else
          let rocks' = cycle grid rocks
          let load = calculateLoad grid rocks'
          Some(load, (rocks', counter + 1)))
      (rocks, 0)
    |> Array.ofSeq

  let isRepeated start size =
    seq { start .. start + size - 1 }
    |> Seq.forall (fun idx -> loads[idx] = loads[idx + size])

  let findRepetition start =
    let itemsLeft = loads.Length - start
    let maxSize = itemsLeft / 2

    seq { minPeriod..maxSize }
    |> Seq.tryFind (fun size -> isRepeated start size)

  let offset, size =
    seq { 0 .. loads.Length - 1 }
    |> Seq.choose (fun start ->
      findRepetition start |> Option.map (fun size -> start, size))
    |> Seq.head

  offset, loads[offset .. offset + size - 1]

let part1 () =
  moveNorth grid rocks |> calculateLoad grid

let part2 () =
  let offset, period = findPattern 3 300 grid rocks
  let partialPeriod = (1000000000 - offset) % period.Length - 1
  period[partialPeriod]


printfn "Day 14"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
