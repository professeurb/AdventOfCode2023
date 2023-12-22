(* let file = "test15.txt" *)
let file = "input15.txt"

type instr = Insert of string * int | Remove of string

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let line =
    sep_by1 (char ',')
      (take_till (function ',' -> true | _ -> false))

  let instruction =
    (let+ label =
       take_while1 (function 'a' .. 'z' -> true | _ -> false)
     and+ _ = char '='
     and+ lens = integer in
     Insert (label, lens))
    <|> let+ label =
          take_while1 (function
            | 'a' .. 'z' -> true
            | _ -> false)
        and+ _ = char '-' in
        Remove label
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let hash s =
  String.fold_left
    (fun hash char -> 17 * (hash + Char.code char) mod 256)
    0 s

let modify shelf slot f =
  Hashtbl.replace shelf slot
    (match Hashtbl.find_opt shelf slot with
    | None -> f []
    | Some v -> f v)

let rec remove label = function
  | [] -> []
  | (a, _) :: l when a = label -> l
  | x :: l -> x :: remove label l

let insert label lens lst =
  let rec aux = function
    | [] -> (false, [])
    | (a, _) :: l when a = label -> (true, (a, lens) :: l)
    | x :: l ->
        let b, l' = aux l in
        (b, x :: l')
  in
  let b, l = aux lst in
  if b then l else (label, lens) :: l

let process_instruction shelf = function
  | Insert (label, lens) ->
      let slot = hash label in
      modify shelf slot (insert label lens)
  | Remove label ->
      let slot = hash label in
      modify shelf slot (remove label)

let _ =
  let input = open_in file in
  let data = input_line input |> eval Parser.line in
  Printf.printf "Part one: %d\n"
    (data |> List.map hash |> List.fold_left ( + ) 0);
  let shelf = Hashtbl.create 10 in
  data
  |> List.map (eval Parser.instruction)
  |> List.iter (fun instr ->
         process_instruction shelf instr;
         (* print_shelf shelf ; *)
         ());
  Printf.printf "Part two: %d\n"
    (Hashtbl.fold
       (fun pos box sum ->
         sum
         + (pos + 1)
           * (box |> List.rev
             |> List.mapi (fun p (_, v) -> v * (p + 1))
             |> List.fold_left ( + ) 0))
       shelf 0)
