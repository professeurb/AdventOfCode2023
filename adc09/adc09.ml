(* let file = "test09.txt" *)
let file = "input09.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let negative_integer =
    let+ _ = char '-' and+ n = integer in
    -n

  let whitespace = many1 (char ' ')
  let line = sep_by1 whitespace (integer <|> negative_integer)
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let rec delta = function
  | a :: (b :: _ as l) -> (b - a) :: delta l
  | _ -> []

let rec previous_term = function
  | [] -> 0
  | a :: _ as l -> a - (l |> delta |> previous_term)

let _ =
  let data = open_in file in
  let sum1, sum2 = (ref 0, ref 0) in
  (try
     while true do
       let line = input_line data in
       print_endline line;
       let line = eval Parser.line line in
       sum2 := !sum2 + previous_term line;
       sum1 := !sum1 + previous_term (List.rev line)
     done
   with End_of_file -> close_in data);
  Printf.printf "Part one: %d\n" !sum1;
  Printf.printf "Part two: %d\n" !sum2
