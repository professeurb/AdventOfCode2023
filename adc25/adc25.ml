(* let file = "test25.txt" *)
let file = "input25.txt"

module Parser = struct
  open Angstrom

  let id =
    let is_letter = function 'a' .. 'z' -> true | _ -> false in
    take_while1 is_letter

  let line =
    let+ node = id <* char ':'
    and+ neighbours = many1 (char ' ' *> id)
    and+ _ = end_of_line in
    (node, neighbours)

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

let read_data file =
  let edges = ref [] in
  read_input file |> eval Parser.data
  |> List.iter (fun (node, neighs) ->
         List.iter
           (fun neigh -> edges := (node, neigh) :: !edges)
           neighs);
  !edges

type 'a node = R of int | NR of 'a

let rec find uf id =
  match Hashtbl.find uf id with
  | R _ -> id
  | NR next ->
      let root = find uf next in
      Hashtbl.replace uf id (NR root);
      root

let union uf n1 n2 =
  let rt1 = find uf n1 and rt2 = find uf n2 in
  if rt1 = rt2 then false
  else
    match (Hashtbl.find uf rt1, Hashtbl.find uf rt2) with
    | R rk1, R rk2 ->
        if rk1 >= rk2 then (
          Hashtbl.replace uf rt1 (R (rk1 + rk2));
          Hashtbl.replace uf rt2 (NR rt1))
        else (
          Hashtbl.replace uf rt2 (R (rk2 + rk1));
          Hashtbl.replace uf rt1 (NR rt2));
        true
    | _ -> failwith "union"

let shuffle d =
  d
  |> List.map (fun c -> (Random.bits (), c))
  |> List.sort compare |> List.map snd

let karger edges =
  let uf = Hashtbl.create 10 in
  List.iter
    (fun (a, b) ->
      Hashtbl.replace uf a (R 1);
      Hashtbl.replace uf b (R 1))
    edges;
  let nb_nodes = ref (Hashtbl.length uf) in
  let rec aux = function
    | [] -> []
    | (a, b) :: lst ->
        if union uf a b then decr nb_nodes;
        if !nb_nodes = 2 then lst else aux lst
  in
  let rest = aux (shuffle edges) in
  let cnt =
    List.fold_left
      (fun cnt (a, b) ->
        if find uf a = find uf b then cnt else cnt + 1)
      0 rest
  in
  (cnt, uf)

let _ =
  Random.self_init ();
  let data = read_data file in
  let cnt = ref 0 in
  let rec aux () =
    let cut_size, uf = karger data in
    if cut_size = 3 then (
      Hashtbl.iter
        (fun _ -> function
          | R rk -> Printf.printf "%d " rk
          | _ -> ())
        uf;
      Printf.printf "%d (%d)\n" cut_size !cnt)
    else (
      incr cnt;
      aux ())
  in
  aux ()
