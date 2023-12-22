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
      connections : (int, pulse) Hashtbl.t;
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
(*   | Flip s -> [ (-1, s.state) ] *)
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
  let id_cnt = ref 0 in
  let map_ids = Hashtbl.create 10 in
  List.iter
    (fun ((name, _), outs) ->
      List.iter
        (fun id ->
          if not (Hashtbl.mem map_ids id) then (
            Hashtbl.add map_ids id !id_cnt;
            incr id_cnt))
        (name :: outs))
    modules;
  let get_id = Hashtbl.find map_ids in
  let circuit = Array.make !id_cnt (Norm, []) in
  List.iter
    (fun ((name, kind), outs) ->
      circuit.(get_id name) <-
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
          List.map get_id outs ))
    modules;
  List.iter
    (fun ((name, _), outs) ->
      List.iter
        (fun out ->
          match circuit.(get_id out) with
          | Conj state, _ ->
              state.total <- state.total + 1;
              Hashtbl.add state.connections (get_id name) Low
          | _ -> ())
        outs)
    modules;
  (map_ids, circuit)

let _ =
  if false then (
    let map_ids, circuit =
      read_input file |> eval Parser.data |> create_circuit
    in
    let get_id = Hashtbl.find map_ids in
    Printf.printf "Let's go!\n%!";
    let q = Queue.create () in
    let count_low = ref 0 and count_high = ref 0 in
    for _ = 1 to 1000 do
      Queue.push
        (Low, get_id "broadcaster", get_id "broadcaster")
        q;
      while not (Queue.is_empty q) do
        let pulse, from, curr = Queue.take q in
        if pulse = Low then incr count_low else incr count_high;
        let mod_inner, outputs = circuit.(curr) in
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
    let map_ids, circuit =
      read_input file |> eval Parser.data |> create_circuit
    in
    let get_id = Hashtbl.find map_ids in
    (* let brdcst = get_id "broadcaster" *)
    (* let zm_id = get_id "zm" in *)
    (* let lz_id = get_id "lz" in *)
    (* let mz_id = get_id "mz" in *)
    let pl_id = get_id "pl" in
    let rx = get_id "rx" in
    Printf.printf "Let's go!\n%!";
    let q = Queue.create () in
    let cnt = ref 0 in
    let exception Stop in
    (try
       while true do
         Queue.push (Low, 0, get_id "broadcaster") q;
         incr cnt;
         while not (Queue.is_empty q) do
           let pulse, from, curr = Queue.take q in
           (* if curr = zm_id && pulse = Low then *)
           (*   Printf.printf "zm: %d\n%!" !cnt; *)
           (* if curr = lz_id && pulse = Low then *)
           (*   Printf.printf "lz: %d\n%!" !cnt; *)
           (* if curr = mz_id && pulse = Low then *)
           (*   Printf.printf "mz: %d\n%!" !cnt; *)
           if curr = pl_id && pulse = Low then
             Printf.printf "pl: %d\n%!" !cnt;
           let mod_inner, outputs = circuit.(curr) in
           match process_pulse pulse from mod_inner with
           | None -> ()
           | Some p ->
               List.iter
                 (fun id ->
                   if curr = rx && pulse = Low then raise Stop
                   else Queue.push (p, curr, id) q)
                 outputs
         done
       done
     with Stop -> ());
    Printf.printf "Part two: %d\n" !cnt)
(*
let kosaraju g =
  let g = Array.map snd g in

  let parcours_1 g =
    let n = Array.length g in
    let vus = Array.make n false in
    let l = ref [] in
    let rec aux s =
      if not vus.(s) then (
        vus.(s) <- true;
        List.iter aux g.(s);
        l := s :: !l)
    in
    for i = 0 to n - 1 do
      aux i
    done;
    !l
  in

  let transpose g =
    let n = Array.length g in
    let g' = Array.make n [] in
    for i = 0 to n - 1 do
      List.iter (fun j -> g'.(j) <- i :: g'.(j)) g.(i)
    done;
    g'
  in

  let rec parcours_2 g vus l s =
    if not vus.(s) then (
      vus.(s) <- true;
      List.iter (fun t -> parcours_2 g vus l t) g.(s);
      l := s :: !l)
  in

  let n = Array.length g in
  let vus = Array.make n false and g' = transpose g in
  let l = ref [] in
  List.iter
    (fun s ->
      if not vus.(s) then (
        let ll = ref [] in
        parcours_2 g' vus ll s;
        l := !ll :: !l))
    (parcours_1 g);
  !l

let modules = read_input file |> eval Parser.data

let map_ids, circuit =
  read_input file |> eval Parser.data |> create_circuit

let id_maps = Hashtbl.fold (fun x y l -> (y, x) :: l) map_ids []

let composantes =
  kosaraju circuit
  |> List.map (List.map (fun x -> List.assoc x id_maps))

let outputs circuit composante =
  let res = ref [] in
  List.iter
    (fun ((name, _), outs) ->
      if List.mem name composante then
        List.iter
          (fun out ->
            if not (List.mem out composante) then
              res := out :: !res)
          outs)
    circuit;
  (composante, !res |> List.sort_uniq compare)

let create_sub_circuit composantes modules =
  let id_cnt = ref 2 in
  let map_ids = Hashtbl.create 10 in
  List.iter
    (fun id ->
      if not (Hashtbl.mem map_ids id) then (
        Hashtbl.add map_ids id !id_cnt;
        incr id_cnt))
    composantes;
  let get_id = Hashtbl.find map_ids in
  let circuit = Array.make !id_cnt (Norm, []) in
  List.iter
    (fun ((name, kind), outs) ->
      match Hashtbl.find_opt map_ids name with
      | Some id ->
          circuit.(get_id name) <-
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
              List.map
                (fun out ->
                  match Hashtbl.find_opt map_ids out with
                  | Some v -> v
                  | None -> 1)
                outs )
      | None -> ())
    modules;
  List.iter
    (fun ((name, _), outs) ->
      List.iter
        (fun out ->
          match circuit.(get_id out) with
          | Conj state, _ ->
              state.total <- state.total + 1;
              Hashtbl.add state.connections
                (match Hashtbl.find_opt map_ids name with
                | Some v -> v
                | None -> 0)
                Low
          | _ -> ())
        outs)
    modules;
  (map_ids, circuit)
*)

let rec euclid a b = if b = 0 then a else euclid b (a mod b)
