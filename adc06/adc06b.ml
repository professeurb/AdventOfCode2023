(* let file = "test06.txt" *)
let file = "input06.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let whitespace = many1 (char ' ')
  let numbers = many1 (whitespace *> integer)
  let times = string "Time:" *> numbers
  let distances = string "Distance:" *> numbers
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let process time dist =
  let delta =
    Float.(pow (pow (of_int time) 2. -. (4. *. of_int dist)) 0.5)
  in
  let first =
    Float.((of_int time -. delta) /. 2. |> ceil |> to_int)
  and last =
    Float.((of_int time +. delta) /. 2. |> floor |> to_int)
  in
  last - first + 1

let get_bigint l =
  l
  |> List.map string_of_int
  |> List.fold_left ( ^ ) ""
  |> int_of_string

let _ =
  let data = open_in file in
  let times = input_line data |> eval Parser.times
  and dists = input_line data |> eval Parser.distances in
  Printf.printf "Part one: %d\n"
    (List.map2 process times dists |> List.fold_left ( * ) 1);
  let bigtime = get_bigint times
  and bigdist = get_bigint dists in
  Printf.printf "Part two: %d\n" (process bigtime bigdist)
