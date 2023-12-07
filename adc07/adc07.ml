(* let file = "test07.txt" *)
let file = "input07.txt"

let occurences s =
  let h = Hashtbl.create 5 in
  String.iter
    (fun c ->
      match Hashtbl.find_opt h c with
      | None -> Hashtbl.add h c (ref 1)
      | Some cnt -> incr cnt)
    s;
  h

let occ_value d =
  Hashtbl.fold (fun _ occ l -> !occ :: l) d []
  |> List.sort compare |> List.rev

let score order hand =
  let len = Hashtbl.length order in
  String.fold_left
    (fun scr chr -> (len * scr) + Hashtbl.find order chr)
    0 hand

let make_order str =
  let pos = Hashtbl.create 5 in
  String.iteri (fun p c -> Hashtbl.add pos c p) str;
  pos

let hand_value1 h = occurences h |> occ_value

let hand_value2 h =
  let occ = occurences h in
  let j =
    match Hashtbl.find_opt occ 'J' with
    | Some v ->
        Hashtbl.remove occ 'J';
        !v
    | None -> 0
  in
  match occ_value occ with [] -> [ j ] | a :: l -> (a + j) :: l

let _ =
  let players =
    let data = open_in file and players_ref = ref [] in
    (try
       while true do
         let line = input_line data in
         players_ref :=
           ( String.sub line 0 5,
             String.sub line 6 (String.length line - 6)
             |> int_of_string )
           :: !players_ref
       done
     with _ -> close_in data);
    !players_ref
  in
  let score1 =
    let order = make_order "23456789TJQKA" in
    players
    |> List.map (fun (h, b) ->
           (hand_value1 h, score order h, h, b))
    |> List.sort compare
    |> List.mapi (fun p v -> (p + 1, v))
    |> List.fold_left
         (fun scr (p, (_, _, _, b)) -> scr + (p * b))
         0
  in
  Printf.printf "Part one: %d\n" score1;
  let score2 =
    let order = make_order "J23456789TQKA" in
    players
    |> List.map (fun (h, b) ->
           (hand_value2 h, score order h, h, b))
    |> List.sort compare
    |> List.mapi (fun p v -> (p + 1, v))
    |> List.fold_left
         (fun scr (p, (_, _, _, b)) -> scr + (p * b))
         0
  in
  Printf.printf "Part two: %d\n" score2
