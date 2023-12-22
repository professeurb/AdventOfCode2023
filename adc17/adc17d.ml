let file = "test17.txt"
(* let file = "test17b.txt" *)
(* let file = "input17.txt" *)

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

let minimize_heat_loss_1 map =
  let exception Found of int in
  let path = Hashtbl.create 10 in
  let height = Array.length map
  and width = Array.length map.(0) in

  let h l c = height - 1 - l + width - 1 - c in
  let rec insert v ((l, c, _, _, _) as node) cond bound mini =
    if
      l >= 0 && l < height && c >= 0 && c < width && cond
      && not (Hashtbl.mem path node)
    then (
      Hashtbl.add path node ();
      let r = min mini (search node (v + map.(l).(c)) bound) in
      Hashtbl.remove path node;
      r)
    else mini
  and search (l, c, da, db, dist) g bound =
    let f = g + h l c in
    if f > bound then f
    else if l = height - 1 && c = width - 1 then
      raise (Found bound)
    else
      max_int
      |> insert g (l - db, c + da, -db, da, 1) true bound
      |> insert g (l + db, c - da, db, -da, 1) true bound
      |> insert g
           (l + da, c + db, da, db, dist + 1)
           (dist + 1 <= 3)
           bound
  in
  let rec ida_star bound =
    search (0, 0, 0, 1, 0) 0 bound |> ida_star
  in
  try ida_star (h 0 0) with Found v -> v

let minimize_heat_loss_2 map =
  let exception Found of int in
  let path = Hashtbl.create 10 in
  let height = Array.length map
  and width = Array.length map.(0) in

  let h l c = height - 1 - l + width - 1 - c in
  let rec insert v ((l, c, _, _, _) as node) cond bound mini =
    if
      l >= 0 && l < height && c >= 0 && c < width && cond
      && not (Hashtbl.mem path node)
    then (
      Hashtbl.add path node ();
      let r = min mini (search node (v + map.(l).(c)) bound) in
      Hashtbl.remove path node;
      r)
    else mini
  and search (l, c, da, db, dist) g bound =
    let f = g + h l c in
    if f > bound then f
    else if l = height - 1 && c = width - 1 then
      raise (Found bound)
    else
      max_int
      |> insert g (l - db, c + da, -db, da, 1) (dist >= 4) bound
      |> insert g (l + db, c - da, db, -da, 1) (dist >= 4) bound
      |> insert g
           (l + da, c + db, da, db, dist + 1)
           (dist + 1 <= 10)
           bound
  in
  let rec ida_star bound =
    search (0, 0, 0, 1, 0) 0 bound |> ida_star
  in
  try ida_star (h 0 0) with Found v -> v

let _ =
  let map = read_map () in
  Printf.printf "Part one: %d\n%!" (minimize_heat_loss_1 map);
  Printf.printf "Part two: %d\n%!" (minimize_heat_loss_2 map)
