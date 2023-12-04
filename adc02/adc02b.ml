(* let file = "test02.txt" *)
let file = "input02.txt"

let make_bag =
  List.fold_left
    (fun (r, g, b) -> function
      | r', "red" -> (r', g, b)
      | g', "green" -> (r, g', b)
      | b', "blue" -> (r, g, b')
      | _ -> failwith "wrong color")
    (0, 0, 0)

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let header = string "Game " *> integer <* string ": "

  let one_color =
    let+ number = integer <* char ' '
    and+ color =
      string "red" <|> string "green" <|> string "blue"
    in
    (number, color)

  let bag = sep_by1 (string ", ") one_color
  let bags = sep_by1 (string "; ") (bag >>| make_bag)

  let line =
    let+ id = header and+ bags = bags in
    (id, bags)
end

let is_possible =
  List.for_all (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14)

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let power bags =
  let r, g, b =
    List.fold_left
      (fun (a, b, c) (a', b', c') ->
        (max a a', max b b', max c c'))
      (0, 0, 0) bags
  in
  r * g * b

let _ =
  let data = open_in file in
  let sum1 = ref 0 and sum2 = ref 0 in
  try
    while true do
      let line = input_line data in
      let id, bags = eval Parser.line line in
      if is_possible bags then sum1 := !sum1 + id;
      sum2 := !sum2 + power bags
    done
  with End_of_file ->
    close_in data;
    Printf.printf "Part one: %d\n" !sum1;
    Printf.printf "Part two: %d\n" !sum2
