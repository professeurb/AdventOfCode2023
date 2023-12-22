(* let file = "test20.txt" *)
(* let file = "test20b.txt" *)
let file = "input20.txt"

type pulse = Low | High
type module_kind = F | C | N

type module_inner =
  | Norm
  | Flip of { mutable state : pulse }
  | Conj of {
      mutable total : int;
      mutable current : int;
      connections : (string, pulse) Hashtbl.t;
    }

let process_pulse p from = function
  | Norm -> Some p
  | Flip s ->
      if p = High then None
      else if s.state = Low then (
        s.state <- High;
        Some High)
      else (
        s.state <- Low;
        Some Low)
  | Conj s ->
      if p = Low then (
        if Hashtbl.find s.connections from = High then
          s.current <- s.current - 1;
        Hashtbl.replace s.connections from Low;
        Some High)
      else (
        if Hashtbl.find s.connections from = Low then
          s.current <- s.current + 1;
        Hashtbl.replace s.connections from High;
        if s.current = s.total then Some Low else Some High)

(* let serialize_hashtbl tbl = *)
(*   Hashtbl.fold (fun x y l -> (x, y) :: l) tbl [] *)
(*   |> List.sort compare *)

(* let serialize_module = function *)
(*   | Conj s -> serialize_hashtbl s.connections *)
(*   | Flip s -> [ ("", s.state) ] *)
(*   | Norm -> [] *)

(* let serialize_circuit circuit = *)
(*   Hashtbl.fold *)
(*     (fun x (y, outs) l -> (x, serialize_module y, outs) :: l) *)
(*     circuit [] *)
(*   |> List.sort compare *)

module Parser = struct
  open Angstrom

  let id =
    let is_letter = function 'a' .. 'z' -> true | _ -> false in
    take_while1 is_letter

  let module_spec =
    (let+ _ = char '%' and+ id = id in
     (id, F))
    <|> (let+ _ = char '&' and+ id = id in
         (id, C))
    <|> let+ id = id in
        (id, N)

  let line =
    let+ module_spec = module_spec
    and+ _ = string " -> "
    and+ dest = sep_by1 (string ", ") id
    and+ _ = end_of_line in
    (module_spec, dest)

  let data = many1 line
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

let create_circuit modules =
  let circuit = Hashtbl.create 10 in
  List.iter
    (fun ((name, kind), outs) ->
      Hashtbl.add circuit name
        ( (match kind with
          | N -> Norm
          | F -> Flip { state = Low }
          | C ->
              Conj
                {
                  total = 0;
                  current = 0;
                  connections = Hashtbl.create 10;
                }),
          outs ))
    modules;
  List.iter
    (fun ((name, _), outs) ->
      List.iter
        (fun out ->
          match Hashtbl.find_opt circuit out with
          | Some (Conj state, _) ->
              state.total <- state.total + 1;
              Hashtbl.add state.connections name Low
          | None -> Hashtbl.add circuit out (Norm, [])
          | _ -> ())
        outs)
    modules;
  circuit

let _ =
  if false then (
    let circuit =
      read_input file |> eval Parser.data |> create_circuit
    in
    Printf.printf "Let's go!\n%!";
    let q = Queue.create () in
    let count_low = ref 0 and count_high = ref 0 in
    for _ = 1 to 1000 do
      Queue.push (Low, "button", "broadcaster") q;
      while not (Queue.is_empty q) do
        let pulse, from, curr = Queue.take q in
        if pulse = Low then incr count_low else incr count_high;
        (* Printf.printf "%s -- %s -> %s\n" from *)
        (*   (if pulse = Low then "Low" else "High") *)
        (*   curr; *)
        let mod_inner, outputs =
          try Hashtbl.find circuit curr
          with Not_found ->
            failwith ("Prout " ^ from ^ " " ^ curr)
        in
        match process_pulse pulse from mod_inner with
        | None -> ()
        | Some p ->
            List.iter
              (fun id -> Queue.push (p, curr, id) q)
              outputs
      done
    done;
    Printf.printf "Part one: %d\n" (!count_low * !count_high))

let _ =
  if true then (
    let circuit =
      read_input file |> eval Parser.data |> create_circuit
    in
    Printf.printf "Let's go!\n%!";
    let q = Queue.create () in
    let cnt = ref 0 in
    let exception Stop in
    (try
       for _ = 1 to 1_000_000 do
         Queue.push (Low, "button", "broadcaster") q;
         incr cnt;
         while not (Queue.is_empty q) do
           let pulse, from, curr = Queue.take q in
           if pulse = Low && curr = "rx" then raise Stop;
           let mod_inner, outputs = Hashtbl.find circuit curr in
           match process_pulse pulse from mod_inner with
           | None -> ()
           | Some p ->
               List.iter
                 (fun id -> Queue.push (p, curr, id) q)
                 outputs
         done
       done
     with Stop -> ());
    Printf.printf "Part two: %d\n" !cnt)
