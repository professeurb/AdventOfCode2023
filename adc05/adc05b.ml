(* let file = "test05.txt" *)
let file = "input05.txt"

module Parser = struct
  open Angstrom

  let is_letter = function 'a' .. 'z' -> true | _ -> false

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let whitespace = many1 (char ' ')
  let numbers = many1 (whitespace *> integer)

  let map_line =
    let+ dst = integer
    and+ src = whitespace *> integer
    and+ len = whitespace *> integer in
    (src, src + len, dst - src)

  let map_header =
    let+ src = take_while1 is_letter
    and+ _ = string "-to-"
    and+ dst = take_while1 is_letter
    and+ _ = string " map:" in
    (src, dst)

  let seeds = string "seeds:" *> numbers
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let read_map data =
  let src, dst = input_line data |> eval Parser.map_header in
  let rules = ref [] in
  (try
     while true do
       rules :=
         (input_line data |> eval Parser.map_line) :: !rules
     done
   with _ -> ());
  (src, dst, !rules)

let apply_map arr v =
  let rec bisect s e =
    (* arr.(s) <= v < arr.(e) *)
    if e = s + 1 then
      let is, ie, off = arr.(s) in
      if v < ie && is <= v then v + off else v
    else
      let m = s + ((e - s) / 2) in
      let is, _, _ = arr.(m) in
      if is <= v then bisect m e else bisect s m
  in
  bisect 0 (Array.length arr)

let rec transform maps kind value =
  match Hashtbl.find_opt maps kind with
  | None -> value
  | Some (kind', map) ->
      transform maps kind' (apply_map map value)

let fold1 f = function
  | [] -> failwith "fold1: empty list"
  | a :: l -> List.fold_left f a l

let rec to_intervals = function
  | start :: len :: l -> (start, start + len) :: to_intervals l
  | [] -> []
  | _ -> failwith "to_intervals : odd length"

let compact l =
  let rec aux = function
    | (is, ie) :: (js, je) :: l when js <= ie ->
        aux ((is, max ie je) :: l)
    | (is, ie) :: l when is = ie -> aux l
    | i :: l -> i :: aux l
    | [] -> []
  in
  l |> List.sort compare |> aux

let process ints rules =
  let rules = rules |> Array.to_seq |> List.of_seq in
  let rec aux = function
    | ints, [] -> ints
    | [], _ -> []
    | ( ((is, ie) :: ints' as ints),
        ((rs, re, offset) :: rules' as rules) ) ->
        if is >= re then aux (ints, rules')
        else if is >= rs && ie >= re then
          (is + offset, re + offset)
          :: aux ((re, ie) :: ints', rules')
        else if is >= rs && ie < re then
          (is + offset, ie + offset) :: aux (ints', rules)
        else if is < rs && ie > rs then
          (is, rs) :: aux ((rs, ie) :: ints', rules)
        else if is < rs && ie <= rs then
          (is, ie) :: aux (ints', rules)
        else assert false
  in
  aux (ints, rules)

let rec transform2 maps kind ints =
  match Hashtbl.find_opt maps kind with
  | None -> compact ints
  | Some (kind', map) ->
      transform2 maps kind' (process (compact ints) map)

let _ =
  let data = open_in file in
  let seeds = input_line data |> eval Parser.seeds in
  input_line data |> ignore;
  let maps = Hashtbl.create 5 in
  (try
     while true do
       let src, dst, rules = read_map data in
       let rules' =
         rules |> List.sort compare |> List.to_seq
         |> Array.of_seq
       in
       Hashtbl.add maps src (dst, rules')
     done
   with _ -> close_in data);
  Printf.printf "Part one: %d\n"
    (seeds |> List.map (transform maps "seed") |> fold1 min);
  let ints = to_intervals seeds in
  Printf.printf "Part two: %d\n"
    (ints |> transform2 maps "seed" |> List.map fst |> fold1 min)
