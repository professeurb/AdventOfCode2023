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
  let cnt = ref 0 in
  for charge = 1 to time - 1 do
    if charge * (time - charge) > dist then incr cnt
  done;
  !cnt

let _ =
  let data = open_in file in
  let times = input_line data |> eval Parser.times
  and dists = input_line data |> eval Parser.distances in
  Printf.printf "Part one: %d\n"
    (List.map2 process times dists |> List.fold_left ( * ) 1);
  let bigtime =
    times
    |> List.map string_of_int
    |> List.fold_left ( ^ ) ""
    |> int_of_string
  and bigdist =
    dists
    |> List.map string_of_int
    |> List.fold_left ( ^ ) ""
    |> int_of_string
  in
  Printf.printf "Part two: %d\n" (process bigtime bigdist)
