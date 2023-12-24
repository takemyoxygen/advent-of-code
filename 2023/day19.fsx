#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open System
open System.Text.RegularExpressions
open FSharpx.Collections
open Utils

type Op =
  | Lt
  | Lte
  | Gt
  | Gte

type FinalOutcome =
  | Accepted
  | Rejected

type RuleOutcome =
  | Final of FinalOutcome
  | Send of string

type Rule =
  | Cond of string * Op * int * RuleOutcome
  | Outcome of RuleOutcome

type Range =
  | Empty
  | Range of int * int

let parseWorkflow s =
  let parseOutcome =
    function
    | "A" -> Final Accepted
    | "R" -> Final Rejected
    | x -> Send(x)

  let parseRule r =
    let cond =
      Regex.Match(r, "(?<part>[a-z]+)(?<op>.)(?<val>\d+):(?<outcome>.+)")

    if cond.Success then
      let op = if cond.Groups["op"].Value = "<" then Lt else Gt

      Cond(
        cond.Groups["part"].Value,
        op,
        Int32.Parse(cond.Groups["val"].Value),
        parseOutcome <| cond.Groups["outcome"].Value
      )
    else
      Outcome(parseOutcome r)


  let groups = Regex.Match(s, "(?<name>.+){(?<rules>.*)}").Groups
  let name = groups["name"].Value
  let rules = groups["rules"].Value

  name, rules.Split(',') |> List.ofArray |> List.map parseRule


let parseRatings (s: string) =
  s.Substring(1, s.Length - 2).Split(",")
  |> Seq.map (fun pair ->
    let [| part; rating |] = pair.Split("=")
    part, Int32.Parse(rating))
  |> Map


let workflows, ratings =
  let ratings, workflows =
    Input.readLines 19 false
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> List.ofSeq
    |> List.partition _.StartsWith("{")

  List.map parseWorkflow workflows |> Map, List.map parseRatings ratings


let reverseOp =
  function
  | Lt -> Gte
  | Lte -> Gt
  | Gte -> Lt
  | Gt -> Lte


let addConstraint constraints part op v =
  let partConstraints =
    Map.tryFind part constraints |> Option.defaultValue List.empty

  Map.add part ((op, v) :: partConstraints) constraints


let inferConstrants workflows name input =
  Map.find name workflows
  |> Seq.fold
    (fun (sent, accepted, currentInput) rule ->
      match rule with
      | Outcome(Final(Accepted)) ->
        sent, Set.add currentInput accepted, Map.empty
      | Outcome(Final(Rejected)) -> sent, accepted, Map.empty
      | Cond(part, op, v, outcome) ->
        let constraintsTrue = addConstraint currentInput part op v
        let constraintsFalse = addConstraint currentInput part (reverseOp op) v

        let sent', accepted' =
          match outcome with
          | Send(wf) -> (wf, constraintsTrue) :: sent, accepted
          | Final(Accepted) -> sent, Set.add constraintsTrue accepted
          | _ -> sent, accepted

        sent', accepted', constraintsFalse
      | Outcome(Send(wf)) -> (wf, currentInput) :: sent, accepted, Map.empty)
    (List.empty, Set.empty, input)
  |> (fun (sent, accepted, _) -> sent, accepted)


let inferAll workflows =
  let rec loop accepted queue =
    match queue with
    | Queue.Nil -> accepted
    | Queue.Cons((wf, constraints), rest) ->
      let wfSent, wfAcc = inferConstrants workflows wf constraints
      let accepted' = Set.union wfAcc accepted
      Seq.fold (fun q s -> Queue.conj s q) rest wfSent |> loop accepted'

  loop Set.empty (Queue.ofList [ "in", Map.empty ])

let checkOverflow =
  function
  | Empty -> Empty
  | Range(st, fin) when fin < st -> Empty
  | range -> range

let applyConstraints range constraints =
  Seq.fold
    (fun range constr ->
      match range with
      | Empty -> Empty
      | Range(st, fin) ->
        match constr with
        | Lt, x -> checkOverflow <| Range(st, min fin (x - 1))
        | Lte, x -> checkOverflow <| Range(st, min fin x)
        | Gt, x -> checkOverflow <| Range(max (x + 1) st, fin)
        | Gte, x -> checkOverflow <| Range(max x st, fin))
    range
    constraints


let applyConstraintsMap constraints ranges =
  Map.map
    (fun key range ->
      Map.tryFind key constraints
      |> Option.defaultValue List.empty
      |> applyConstraints range)
    ranges


let satisfiesConstraints constraints ranges =
  constraints
  |> Seq.exists (fun constr ->
    applyConstraintsMap constr ranges
    |> Map.values
    |> Seq.contains Empty
    |> not)


let constraints = inferAll workflows


let part1 () =
  ratings
  |> Seq.map (Map.map (fun _ x -> Range(x, x)))
  |> Seq.filter (satisfiesConstraints constraints)
  |> Seq.collect Map.values
  |> Seq.sumBy (function
    | Empty -> 0
    | Range(st, _) -> st)


let part2 () =
  let init =
    [ "x"; "m"; "a"; "s" ] |> List.map (fun l -> l, Range(1, 4000)) |> Map


  constraints
  |> Seq.map (fun constr ->
    let ranges = applyConstraintsMap constr init

    ranges
    |> Map.values
    |> Seq.map (function
      | Empty -> 0I
      | Range(st, fn) -> bigint (fn - st + 1))
    |> Seq.fold ((*)) 1I)
  |> Seq.sum


printfn "Day 19"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
