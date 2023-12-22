(* let file = "test22.txt" *)
let file = "input22.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    take_while1 is_digit >>| int_of_string

  let line =
    let+ x1 = integer <* char ','
    and+ y1 = integer <* char ','
    and+ z1 = integer <* char '~'
    and+ x2 = integer <* char ','
    and+ y2 = integer <* char ','
    and+ z2 = integer <* end_of_line in
    ((x1, y1, z1), (x2 - x1, y2 - y1, z2 - z1))

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

let read_data file = read_input file |> eval Parser.data
let ( += ) a b = a := !a + b

let score piece below above =
  (* Printf.printf "%d: %!" piece; *)
  let nb = Array.length below in
  let trans = Array.make nb false in
  trans.(piece) <- true;
  let curr = ref above.(piece) and next = ref [] in
  while !curr <> [] do
    (* Printf.printf ".%!"; *)
    List.iter
      (fun piece ->
        (* Printf.printf "p%d %!" piece; *)
        if List.for_all (fun p -> trans.(p)) below.(piece) then (
          trans.(piece) <- true;
          next := List.rev_append above.(piece) !next))
      !curr;
    curr := List.sort_uniq compare !next;
    next := []
  done;
  let cnt =
    Array.fold_left
      (fun cnt b -> if b then cnt + 1 else cnt)
      0 trans
    - 1
  in
  (* Printf.printf "%d\n%!" cnt; *)
  cnt

let _ =
  let data = read_data file in
  let data =
    List.sort
      (fun ((_, _, z1), _) ((_, _, z2), _) -> compare z1 z2)
      data
  in
  let nb = List.length data in
  let map = Hashtbl.create 10
  and disintegratable = Array.make (nb + 1) true
  and just_above = Array.make (nb + 1) []
  and just_below = Array.make (nb + 1) [] in
  disintegratable.(0) <- false;
  let get x y =
    match Hashtbl.find_opt map (x, y) with
    | Some v -> v
    | None -> (0, 0)
  in
  List.iteri
    (fun num ((x, y, z), (dx, dy, dz)) ->
      let num = num + 1 in
      let top = ref 0 in
      for i = 0 to dx do
        top := max !top (get (x + i) y |> snd)
      done;
      for j = 0 to dy do
        top := max !top (get x (y + j) |> snd)
      done;
      assert (z > !top);
      let below = ref [] in
      for i = 0 to dx do
        let p, h = get (x + i) y in
        if h = !top then below := p :: !below
      done;
      for j = 0 to dy do
        let p, h = get x (y + j) in
        if h = !top then below := p :: !below
      done;
      let below = List.sort_uniq compare !below in
      just_below.(num) <- below;
      List.iter
        (fun piece ->
          just_above.(piece) <- num :: just_above.(piece))
        below;
      (match below with
      | [ piece ] -> disintegratable.(piece) <- false
      | _ -> ());
      for i = 0 to dx do
        Hashtbl.add map (x + i, y) (num, !top + 1 + dz)
      done;
      for j = 0 to dy do
        Hashtbl.add map (x, y + j) (num, !top + 1 + dz)
      done)
    data;
  (* for i = 1 to nb do *)
  (*   Printf.printf "%d : %c\n" i *)
  (*     (if disintegratable.(i) then '*' else '.') *)
  (* done; *)
  Printf.printf "Part one: %d\n"
    (Array.fold_left
       (fun cnt b -> if b then cnt + 1 else cnt)
       0 disintegratable);
  let cnt = ref 0 in
  for i = 1 to nb do
    cnt += score i just_below just_above
  done;
  Printf.printf "Part two: %d\n" !cnt
