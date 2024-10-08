(* let file = "test23.txt" *)
let file = "input23.txt"

type tile = P | F | N | S | W | E

let tile_of_char = function
  | '.' -> P
  | '#' -> F
  | '<' -> W
  | '>' -> E
  | '^' -> N
  | 'v' -> S
  | _ -> failwith "Wrong char"

let read_map file =
  let input = open_in file in
  let map = ref [] in
  (try
     while true do
       let line = input_line input in
       map :=
         Array.init (String.length line) (fun i ->
             tile_of_char line.[i])
         :: !map
     done
   with
  | End_of_file -> close_in input
  | exn ->
      close_in input;
      raise exn);
  !map |> List.rev |> List.to_seq |> Array.of_seq

(* let graph_of_map map = *)
(*   let graph = Hashtbl.create 10 in *)
(*   let height = Array.length map *)
(*   and width = Array.length map.(0) in *)
(*   for i = 1 to height - 2 do *)
(*     for j = 1 to width - 2 do *)
(*       if map.(i).(j) <> F then ( *)
(*         let l = Hashtbl.create 4 in *)
(*         if i > 1 && map.(i - 1).(j) <> F then *)
(*           Hashtbl.add l (i - 1, j) 1; *)
(*         if i < height - 2 && map.(i + 1).(j) <> F then *)
(*           Hashtbl.add l (i + 1, j) 1; *)
(*         if map.(i).(j - 1) <> F then Hashtbl.add l (i, j - 1) 1; *)
(*         if map.(i).(j + 1) <> F then Hashtbl.add l (i, j + 1) 1; *)
(*         Hashtbl.add graph (i, j) l) *)
(*     done *)
(*   done; *)
(*   (* Printf.printf "Sommets avant nettoyage: %d\n" *) *)
(*   (*   (Hashtbl.length graph); *) *)
(*   Hashtbl.iter *)
(*     (fun cell adj -> *)
(*       assert (Hashtbl.length adj > 0); *)
(*       match adj |> Hashtbl.to_seq |> List.of_seq with *)
(*       | [ (p1, l1); (p2, l2) ] -> *)
(*           let a1 = Hashtbl.find graph p1 *)
(*           and a2 = Hashtbl.find graph p2 in *)
(*           Hashtbl.remove a1 cell; *)
(*           Hashtbl.remove a2 cell; *)
(*           assert (not (Hashtbl.mem a1 p2)); *)
(*           assert (not (Hashtbl.mem a2 p1)); *)
(*           Hashtbl.add a1 p2 (l1 + l2); *)
(*           Hashtbl.add a2 p1 (l1 + l2); *)
(*           Hashtbl.remove adj p1; *)
(*           Hashtbl.remove adj p2 *)
(*       | _ -> ()) *)
(*     graph; *)
(*   for i = 1 to height - 2 do *)
(*     for j = 1 to width - 2 do *)
(*       if map.(i).(j) <> F then *)
(*         let adj = Hashtbl.find graph (i, j) in *)
(*         match Hashtbl.length adj with *)
(*         | 2 -> failwith "Prout" *)
(*         | 0 -> Hashtbl.remove graph (i, j) *)
(*         | _ -> () *)
(*     done *)
(*   done; *)
(*   (* Printf.printf "Sommets après nettoyage: %d\n" *) *)
(*   (*   (Hashtbl.length graph); *) *)
(*   for i = 1 to width - 2 do *)
(*     if map.(0).(i) <> F then ( *)
(*       Hashtbl.add (Hashtbl.find graph (1, i)) (0, i) 1; *)
(*       Hashtbl.add graph (0, i) (Hashtbl.create 1); *)
(*       Hashtbl.add (Hashtbl.find graph (0, i)) (1, i) 1); *)
(*     if map.(height - 1).(i) <> F then ( *)
(*       Hashtbl.add *)
(*         (Hashtbl.find graph (height - 2, i)) *)
(*         (height - 1, i) *)
(*         1; *)
(*       Hashtbl.add graph (height - 1, i) (Hashtbl.create 1); *)
(*       Hashtbl.add *)
(*         (Hashtbl.find graph (height - 1, i)) *)
(*         (height - 2, i) *)
(*         1) *)
(*   done; *)
(*   graph *)

let dfs map c =
  let seen = Hashtbl.create 10
  and max_steps = ref 0
  and end_line = Array.length map - 1 in
  let rec visit lin col steps =
    (* Printf.printf "%d %d %d\n" lin col steps; *)
    if lin = end_line then (
      if steps > !max_steps then max_steps := steps)
    else if not (Hashtbl.mem seen (lin, col)) then (
      Hashtbl.add seen (lin, col) ();
      (match map.(lin).(col) with
      | P ->
          visit (lin - 1) col (steps + 1);
          visit (lin + 1) col (steps + 1);
          visit lin (col - 1) (steps + 1);
          visit lin (col + 1) (steps + 1)
      | F -> ()
      | N -> visit (lin - 1) col (steps + 1)
      | S -> visit (lin + 1) col (steps + 1)
      | W -> visit lin (col - 1) (steps + 1)
      | E -> visit lin (col + 1) (steps + 1));
      Hashtbl.remove seen (lin, col))
  in
  visit 0 c 0;
  !max_steps

(* let dfs2 graph end_line c = *)
(*   let seen = Hashtbl.create 10 and max_steps = ref 0 in *)
(*   let rec visit ((lin, _) as pos) steps = *)
(*     (* Printf.printf "%d %d %d\n" lin col steps; *) *)
(*     if lin = end_line then ( *)
(*       if steps > !max_steps then max_steps := steps) *)
(*     else if not (Hashtbl.mem seen pos) then ( *)
(*       Hashtbl.add seen pos (); *)
(*       Hashtbl.iter *)
(*         (fun nxt len -> visit nxt (steps + len)) *)
(*         (Hashtbl.find graph pos); *)
(*       Hashtbl.remove seen pos) *)
(*   in *)
(*   visit (0, c) 0; *)
(*   !max_steps *)

let _ =
  let map = read_map file in
  let height = Array.length map
  and width = Array.length map.(0) in
  for i = 0 to Array.length map.(0) - 1 do
    if map.(0).(i) = P then (
      map.(0).(i) <- S;
      Printf.printf "Part one: %d\n%!" (dfs map i))
  done;
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if map.(i).(j) <> F then map.(i).(j) <- P
    done
  done;
  for i = 0 to Array.length map.(0) - 1 do
    if map.(0).(i) = P then (
      map.(0).(i) <- S;
      Printf.printf "Part two: %d\n%!" (dfs map i))
  done

(* let graph = graph_of_map map in *)
(* for i = 0 to height - 1 do *)
(*   if map.(0).(i) <> F then ( *)
(*     map.(0).(i) <- S; *)
(*     Printf.printf "Part two: %d\n" (dfs2 graph (height - 1) i)) *)
(* done *)
