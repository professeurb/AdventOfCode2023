(* let file = "test16.txt" *)
let file = "input16.txt"

let read_grid () =
  let input = open_in file in
  let grid = ref [] in
  (try
     while true do
       grid := input_line input :: !grid
     done
   with _ -> close_in input);
  !grid |> List.rev |> List.to_seq |> Array.of_seq

type direction = L | R | U | D

module HS = struct
  type 'a t = ('a, unit) Hashtbl.t

  let create () : 'a t = Hashtbl.create 10
  let len (set : 'a t) = Hashtbl.length set
  let mem (set : 'a t) elt = Hashtbl.mem set elt
  let add (set : 'a t) elt = Hashtbl.replace set elt ()
end

let energize grid l c d =
  let height = Array.length grid
  and width = String.length grid.(0) in
  let tbl_dirs = HS.create () and tbl = HS.create () in
  let rec aux l c d =
    if l >= 0 && l < height && c >= 0 && c < width then
      if not (HS.mem tbl_dirs (l, c, d)) then (
        HS.add tbl_dirs (l, c, d);
        HS.add tbl (l, c);
        match (grid.(l).[c], d) with
        | '.', L -> aux l (c - 1) L
        | '.', R -> aux l (c + 1) R
        | '.', U -> aux (l - 1) c U
        | '.', D -> aux (l + 1) c D
        | '|', L ->
            aux (l - 1) c U;
            aux (l + 1) c D
        | '|', R ->
            aux (l - 1) c U;
            aux (l + 1) c D
        | '|', U -> aux (l - 1) c U
        | '|', D -> aux (l + 1) c D
        | '-', L -> aux l (c - 1) L
        | '-', R -> aux l (c + 1) R
        | '-', U ->
            aux l (c - 1) L;
            aux l (c + 1) R
        | '-', D ->
            aux l (c - 1) L;
            aux l (c + 1) R
        | '/', L -> aux (l + 1) c D
        | '/', R -> aux (l - 1) c U
        | '/', U -> aux l (c + 1) R
        | '/', D -> aux l (c - 1) L
        | '\\', L -> aux (l - 1) c U
        | '\\', R -> aux (l + 1) c D
        | '\\', U -> aux l (c - 1) L
        | '\\', D -> aux l (c + 1) R
        | _ -> assert false)
  in
  aux l c d;
  HS.len tbl

let _ =
  let grid = read_grid () in
  Printf.printf "Part one: %d\n" (energize grid 0 0 R);
  let maxi = ref 0
  and height = Array.length grid
  and width = String.length grid.(0) in
  for l = 0 to height - 1 do
    maxi := max !maxi (energize grid l 0 R);
    maxi := max !maxi (energize grid l (width - 1) L)
  done;
  for c = 0 to width - 1 do
    maxi := max !maxi (energize grid 0 c D);
    maxi := max !maxi (energize grid (height - 1) c U)
  done;
  Printf.printf "Part two: %d\n" !maxi
