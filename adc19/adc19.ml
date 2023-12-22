(* let file = "test19.txt" *)
let file = "input19.txt"

type part = { x : int; m : int; a : int; s : int }
type action = R | A | W of string

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let id =
    let is_letter = function 'a' .. 'z' -> true | _ -> false in
    take_while1 is_letter

  let action =
    char 'R' *> return R
    <|> char 'A' *> return A
    <|> (id >>| fun id -> W id)

  let rule =
    (let+ category = take 1
     and+ test = take 1
     and+ value = integer
     and+ _ = char ':'
     and+ action = action in
     (Some (category, test, value), action))
    <|> let+ action = action in
        (None, action)

  let workflow =
    let+ id = id
    and+ _ = char '{'
    and+ rules = sep_by1 (char ',') rule
    and+ _ = string "}"
    and+ _ = end_of_line in
    (id, rules)

  let workflows = many1 workflow

  let part =
    let+ x = string "{x=" *> integer
    and+ m = string ",m=" *> integer
    and+ a = string ",a=" *> integer
    and+ s =
      string ",s=" *> integer <* string "}" <* end_of_line
    in
    { x; m; a; s }

  let data =
    let+ workflows = workflows
    and+ _ = end_of_line
    and+ parts = many1 part in
    (workflows, parts)
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let read_input file =
  let input = open_in file in
  in_channel_length input |> really_input_string input

let part_score p = p.x + p.m + p.a + p.s

let accept_part dict part =
  let eval = function
    | None -> true
    | Some ("x", "<", v) -> part.x < v
    | Some ("x", ">", v) -> part.x > v
    | Some ("m", "<", v) -> part.m < v
    | Some ("m", ">", v) -> part.m > v
    | Some ("a", "<", v) -> part.a < v
    | Some ("a", ">", v) -> part.a > v
    | Some ("s", "<", v) -> part.s < v
    | Some ("s", ">", v) -> part.s > v
    | _ -> failwith "eval"
  in
  let rec apply_workflow id = apply_rules (Hashtbl.find dict id)
  and apply_rules = function
    | [] -> failwith "apply_workflow"
    | (test, A) :: rules -> eval test || apply_rules rules
    | (test, R) :: rules ->
        if eval test then false else apply_rules rules
    | (test, W id) :: rules ->
        if eval test then apply_workflow id
        else apply_rules rules
  in
  apply_workflow "in"

let reverse = function
  | "<" -> ">"
  | ">" -> "<"
  | _ -> failwith "reverse"

let rec filter cat test value = function
  | [] -> []
  | (xb, xe, mb, me, ab, ae, sb, se) :: lst -> (
      let lst' = filter cat test value lst in
      match (cat, test) with
      | "x", ">" ->
          (* x > value *)
          if xe <= value then lst'
          else
            (max xb (value + 1), xe, mb, me, ab, ae, sb, se)
            :: lst'
      | "x", "<" ->
          (* x < value *)
          if xb >= value then lst'
          else
            (xb, min xe (value - 1), mb, me, ab, ae, sb, se)
            :: lst'
      | "m", ">" ->
          (* m > value *)
          if me <= value then lst'
          else
            (xb, xe, max mb (value + 1), me, ab, ae, sb, se)
            :: lst'
      | "m", "<" ->
          (* m < value *)
          if mb >= value then lst'
          else
            (xb, xe, mb, min me (value - 1), ab, ae, sb, se)
            :: lst'
      | "a", ">" ->
          (* a > value *)
          if ae <= value then lst'
          else
            (xb, xe, mb, me, max ab (value + 1), ae, sb, se)
            :: lst'
      | "a", "<" ->
          (* a < value *)
          if ab >= value then lst'
          else
            (xb, xe, mb, me, ab, min ae (value - 1), sb, se)
            :: lst'
      | "s", ">" ->
          (* s > value *)
          if se <= value then lst'
          else
            (xb, xe, mb, me, ab, ae, max sb (value + 1), se)
            :: lst'
      | "s", "<" ->
          (* s < value *)
          if sb >= value then lst'
          else
            (xb, xe, mb, me, ab, ae, sb, min se (value - 1))
            :: lst'
      | _ -> failwith "constraint")

let process_workflow dict =
  let accept = [ (1, 4000, 1, 4000, 1, 4000, 1, 4000) ]
  and reject = [] in

  let dict' = Hashtbl.create 10 in
  let rec aux w =
    match w with
    | A -> accept
    | R -> reject
    | W id -> (
        match Hashtbl.find_opt dict' id with
        | Some v -> v
        | None ->
            let v = aux2 (Hashtbl.find dict id) in
            Hashtbl.add dict' id v;
            v)
  and aux2 = function
    | [] -> failwith "process_workflow"
    | (None, w) :: _ -> aux w
    | (Some (cat, test, value), w) :: lst ->
        List.rev_append
          (filter cat test value (aux w))
          (filter cat (reverse test)
             (* > 1000 devient < 1001 *)
             (if test = ">" then value + 1 else value - 1)
             (aux2 lst))
  in

  aux (W "in")

let rec score2 = function
  | [] -> 0
  | (xb, xe, mb, me, ab, ae, sb, se) :: lst ->
      (xe - xb + 1)
      * (me - mb + 1)
      * (ae - ab + 1)
      * (se - sb + 1)
      + score2 lst

let _ =
  let workflows, parts = read_input file |> eval Parser.data in
  let dict = Hashtbl.create 10 in
  List.iter
    (fun (id, rule_list) -> Hashtbl.add dict id rule_list)
    workflows;
  Printf.printf "Part one: %d\n"
    (List.fold_left
       (fun s part ->
         if accept_part dict part then s + part_score part else s)
       0 parts);
  Printf.printf "Part two: %d\n" (process_workflow dict |> score2)
