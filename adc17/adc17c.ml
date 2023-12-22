(* let file = "test17.txt" *)
let file = "input17.txt"

let read_map () =
  let input = open_in file in
  let map = ref [] in
  (try
     while true do
       map :=
         (input_line input |> String.to_seq |> Array.of_seq
         |> Array.map (fun c -> Char.code c - 48))
         :: !map
     done
   with
  | End_of_file -> close_in input
  | exn ->
      close_in input;
      raise exn);
  !map |> List.rev |> List.to_seq |> Array.of_seq

module LHeap = struct
  type 'a tree = E | N of int * 'a tree * 'a * 'a tree

  let combine v t1 t2 =
    match (t1, t2) with
    | E, E -> N (0, E, v, E)
    | E, (N (r, _, _, _) as t) | (N (r, _, _, _) as t), E ->
        N (r + 1, t, v, E)
    | (N (rg, _, _, _) as g), (N (rd, _, _, _) as d) ->
        if rg < rd then N (rg + 1, d, v, g)
        else N (rd + 1, g, v, d)

  let rec merge t1 t2 =
    match (t1, t2) with
    | E, t | t, E -> t
    | N (_, g1, v1, d1), N (_, g2, v2, d2) ->
        if v1 <= v2 then combine v1 g1 (merge d1 t2)
        else combine v2 g2 (merge d2 t1)

  let empty = E
  let insert v t = merge t (N (0, E, v, E))

  let extract = function
    | E -> failwith "extraire : tas vide"
    | N (_, g, v, d) -> (v, merge g d)

  (* let extract_opt = function *)
  (*   | E -> None *)
  (*   | N (_, g, v, d) -> Some (v, merge g d) *)
end

let minimize_heat_loss_1 map =
  let mark = Hashtbl.create 10 in
  let height = Array.length map
  and width = Array.length map.(0) in

  let insert v l c da db dist cond heap =
    if l < 0 || l >= height || c < 0 || c >= width || not cond
    then heap
    else LHeap.insert (v + map.(l).(c), l, c, da, db, dist) heap
  in

  let rec aux heap =
    let (v, l, c, da, db, dist), heap' = LHeap.extract heap in
    if l = height - 1 && c = width - 1 then v
    else
      match Hashtbl.find_opt mark (l, c, da, db, dist) with
      | Some _ -> aux heap'
      | None ->
          Hashtbl.add mark (l, c, da, db, dist) ();
          heap'
          |> insert v (l - db) (c + da) (-db) da 1 true
          |> insert v (l + db) (c - da) db (-da) 1 true
          |> insert v (l + da) (c + db) da db (dist + 1)
               (dist + 1 <= 3)
          |> aux
  in
  aux
    (LHeap.empty
    |> insert 0 0 0 1 0 0 true
    |> insert 0 0 0 0 1 0 true)
  - map.(0).(0)

let minimize_heat_loss_2 map =
  let mark = Hashtbl.create 10 in
  let height = Array.length map
  and width = Array.length map.(0) in

  let insert v l c da db dist cond heap =
    if l < 0 || l >= height || c < 0 || c >= width || not cond
    then heap
    else LHeap.insert (v + map.(l).(c), l, c, da, db, dist) heap
  in

  let rec aux heap =
    (* Printf.printf "%d\n" (Hashtbl.length mark); *)
    let (v, l, c, da, db, dist), heap' = LHeap.extract heap in
    if l = height - 1 && c = width - 1 then v
    else
      match Hashtbl.find_opt mark (l, c, da, db, dist) with
      | Some _ -> aux heap'
      | None ->
          Hashtbl.add mark (l, c, da, db, dist) ();
          heap'
          |> insert v (l + da) (c + db) da db (dist + 1)
               (dist + 1 <= 10)
          |> insert v (l - db) (c + da) (-db) da 1 (dist >= 4)
          |> insert v (l + db) (c - da) db (-da) 1 (dist >= 4)
          |> aux
  in
  aux
    (LHeap.empty
    |> insert 0 0 0 1 0 0 true
    |> insert 0 0 0 0 1 0 true)
  - map.(0).(0)

let _ =
  let map = read_map () in
  Printf.printf "Part one: %d\n" (minimize_heat_loss_1 map);
  Printf.printf "Part two: %d\n" (minimize_heat_loss_2 map)
