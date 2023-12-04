(* let file = "test04.txt" *)
let file = "input04.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let whitespace = many1 (char ' ')

  let header =
    string "Card" *> whitespace *> integer <* string ":"

  let numbers = many1 (whitespace *> integer)

  let line =
    let+ id = header
    and+ winning = numbers
    and+ _ = string " |"
    and+ personal = numbers in
    (id, winning, personal)
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let wins w p =
  let w = List.sort compare w and p = List.sort compare p in
  let rec aux w p =
    match (w, p) with
    | a :: w', b :: _ when a < b -> aux w' p
    | a :: _, b :: p' when a > b -> aux w p'
    | a :: w', b :: p' when a = b -> 1 + aux w' p'
    | _ -> 0
  in
  aux w p

let rec pow a n = if n = 0 then 1 else a * pow a (n - 1)

let score1 w p =
  let scr = wins w p in
  if scr = 0 then 0 else pow 2 (scr - 1)

let _ =
  let data = open_in file in
  let sum1 = ref 0 and cards = ref [] in
  (try
     while true do
       let line = input_line data in
       let _, w, p = eval Parser.line line in
       cards := (w, p) :: !cards;
       sum1 := !sum1 + score1 w p
     done
   with End_of_file -> close_in data);
  Printf.printf "Part one: %d\n" !sum1;
  let cards =
    !cards |> List.rev |> List.to_seq |> Array.of_seq
  in
  let copies = Array.make (Array.length cards) 1 in
  let sum2 = ref 0 in
  for i = 0 to Array.length cards - 1 do
    (* Printf.printf "Card %d: %d copies\n" i copies.(i); *)
    sum2 := !sum2 + copies.(i);
    let w, p = cards.(i) in
    let wc = wins w p in
    for j = 1 to wc do
      copies.(i + j) <- copies.(i + j) + copies.(i)
    done
  done;
  Printf.printf "Part two: %d\n" !sum2
