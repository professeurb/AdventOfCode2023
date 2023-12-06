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
  let rec bisect1 a b =
    if b = a + 1 then b
    else
      let m = (a + b) / 2 in
      if m * (time - m) <= dist then bisect1 m b else bisect1 a m
  and bisect2 a b =
    if b = a + 1 then a
    else
      let m = (a + b) / 2 in
      if m * (time - m) <= dist then bisect2 a m else bisect2 m b
  in
  bisect2 (time / 2) time - bisect1 0 (time / 2) + 1

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
