#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open Utils
open FSharpx.Collections

type OnOff =
  | On
  | Off

type Pulse =
  | High
  | Low

type Module =
  | FlipFlop of OnOff
  | Conj of Map<string, Pulse>
  | Broadcaster

type PulseCounter = { High: int; Low: int }

let modules, connections =
  let rec loop modules connections (lines: string list) =
    match lines with
    | [] -> modules, connections
    | line :: rest ->
      let [| input; outputs |] = line.Split(" -> ")
      let outputs = outputs.Split(", ") |> List.ofSeq

      let name, state =
        match input with
        | "broadcaster" -> input, Broadcaster
        | input when input.StartsWith("&") ->
          input.Substring(1), Conj(Map.empty)
        | input when input.StartsWith("%") -> input.Substring(1), FlipFlop(Off)
        | _ -> failwithf "Unknown module name: %s" input

      loop (Map.add name state modules) (Map.add name outputs connections) rest

  let modules, connections =
    Input.readLines 20 false |> List.ofSeq |> loop Map.empty Map.empty

  let modules =
    modules
    |> Map.toSeq
    |> Seq.filter (fun (_, state) ->
      match state with
      | Conj(_) -> true
      | _ -> false)
    |> Seq.map (fun (name, _) ->
      let inputs =
        connections
        |> Map.toSeq
        |> Seq.filter (fun (_, outputs) -> List.contains name outputs)
        |> Seq.map fst

      name, inputs)
    |> Seq.fold
      (fun modules (name, inputs) ->
        let memory = inputs |> Seq.map (fun inp -> inp, Low) |> Map
        Map.add name (Conj memory) modules)
      modules

  modules, connections

let handleSignal state source pulse =
  match state, pulse with
  | FlipFlop(_), High -> state, None
  | FlipFlop(Off), Low -> FlipFlop(On), Some(High)
  | FlipFlop(On), Low -> FlipFlop(Off), Some(Low)
  | Broadcaster, _ -> Broadcaster, Some(pulse)
  | Conj(mem), _ ->
    let mem' = Map.add source pulse mem
    let pulse' = if Map.forall (fun _ p -> p = High) mem' then Low else High
    Conj(mem'), Some(pulse')


let inc cnt =
  function
  | High -> { cnt with High = cnt.High + 1 }
  | Low -> { cnt with Low = cnt.Low + 1 }


let sum { High = h1; Low = l1 } { High = h2; Low = l2 } =
  { High = h1 + h2; Low = l1 + l2 }


let pressButton modules connections =
  let rec loop modules queue counter history =
    match queue with
    | Queue.Nil -> modules, counter, history
    | Queue.Cons((name, signal, source), queue') when
      not <| Map.containsKey name modules
      ->
      // printfn "%s -%A-> %s (!!!)" source signal name
      loop
        modules
        queue'
        (inc counter signal)
        ((name, signal, source) :: history)
    | Queue.Cons((name, signal, source), queue') ->
      let state = Map.find name modules
      let state', outSignal = handleSignal state source signal

      // printfn "%s -%A-> %s (out: %A)" source signal name outSignal

      let queue'' =
        match outSignal with
        | None -> queue'
        | Some(pulse) ->
          Map.find name connections
          |> Seq.fold (fun q conn -> Queue.conj (conn, pulse, name) q) queue'

      let modules' = Map.add name state' modules

      loop
        modules'
        queue''
        (inc counter signal)
        ((name, signal, source) :: history)

  loop
    modules
    (Queue.ofList [ "broadcaster", Low, "button" ])
    { High = 0; Low = 0 }
    List.empty


let part1 () =
  let _, cnt =
    Fun.repeatn
      (fun (modules, counter) ->
        let modules', newCounter, _ = pressButton modules connections
        modules', (sum counter newCounter))
      (modules, { High = 0; Low = 0 })
      1000

  cnt.High * cnt.Low


let part2 () =
  // A signle Conjunction input to the rx module.
  // it should receive High pulse for all its inputs to emit a Low pulse
  let reqModule = "rg"

  let inputs =
    Map.find reqModule modules
    |> function
      | Conj(mem) -> set mem.Keys
      | _ -> failwithf "%s should be a conj module" reqModule

  let rec findPeriods toFind results presses modules =
    if Set.isEmpty toFind then
      results
    else
      let modules', _, history = pressButton modules connections

      let found =
        history
        |> Seq.filter (fun (_, pulse, source) ->
          pulse = High && Set.contains source toFind)
        |> Seq.map (fun (_, _, source) -> source)
        |> set

      let results' =
        found |> Set.fold (fun res fnd -> (fnd, presses) :: res) results

      let toFind' = Set.difference toFind found

      findPeriods toFind' results' (presses + 1) modules'


  findPeriods inputs List.empty 1 modules
  |> Seq.map (snd >> bigint)
  |> Seq.fold Num.lcm 1I


printfn "Day 20"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
