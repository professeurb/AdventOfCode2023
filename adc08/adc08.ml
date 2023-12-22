(* let file = "test08.txt" *)
(* let file = "test08b.txt" *)
let file = "input08.txt"

exception Stop

let follow_directions dirs graph start =
  let steps = ref 0 in
  let pos = ref start in
  (try
     while true do
       String.iter
         (fun d ->
           if !pos.[2] = 'Z' then raise Stop;
           (* Printf.printf "%s : %c %d\n" !pos d !steps; *)
           incr steps;
           let l, r = Hashtbl.find graph !pos in
           match d with
           | 'L' -> pos := l
           | 'R' -> pos := r
           | _ -> failwith "Wrong direction")
         dirs
     done
   with Stop -> ());
  (!steps, !pos)

let rec euclid a b = if b = 0 then a else euclid b (a mod b)
let lcm a b = a / euclid a b * b

let rec bezout a b =
  if b = 0 then (1, 0, a)
  else
    let u, v, gcd = bezout b (a mod b) in
    (v, u - (a / b * v), gcd)

let chinois (a1, n1) (a2, n2) =
  let u1, u2, gcd = bezout n1 n2 in
  ((a1 * u2 * n2) + (a2 * u1 * n1), n1 / gcd * n2)

let poor_man's_hare_tortoise dirs graph start =
  let steps = ref 0 in
  let pos = ref start in
  let poses = ref [] in
  (try
     while true do
       String.iter
         (fun d ->
           (if !pos.[2] = 'Z' then
              match List.assoc_opt !pos !poses with
              | None -> poses := (!pos, !steps) :: !poses
              | Some _ -> raise Stop);
           (* Printf.printf "%s : %c %d\n" !pos d !steps; *)
           incr steps;
           let l, r = Hashtbl.find graph !pos in
           match d with
           | 'L' -> pos := l
           | 'R' -> pos := r
           | _ -> failwith "Wrong direction")
         dirs
     done
   with Stop -> ());
  (!poses, !steps - List.assoc !pos !poses)

let _ =
  let data = open_in file in
  let directions = input_line data in
  let _ = input_line data in
  let graph = Hashtbl.create 10 in
  (try
     while true do
       let line = input_line data in
       let from = String.sub line 0 3
       and left = String.sub line 7 3
       and rght = String.sub line 12 3 in
       Hashtbl.add graph from (left, rght)
     done
   with _ -> close_in data);
  Printf.printf "Part one: %d"
    (follow_directions directions graph "AAA" |> fst)

let _ =
  let res = ref [] in
  Hashtbl.iter
    (fun node _ ->
      if node.[2] = 'A' then
        res :=
          poor_man's_hare_tortoise directions graph node :: !res)
    graph
