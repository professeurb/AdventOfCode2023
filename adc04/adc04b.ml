open Brucore

(* let file = "test04.txt" *)
let file = "input04.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    int_of_string <$> take_while1 is_digit

  let whitespace = many1 (char ' ')

  let header =
    string "Card" *> whitespace *> integer <* string ":"

  let numbers = many1 (whitespace *> integer)

  let line =
    let+ winning = header *> numbers
    and+ hand = string " |" *> numbers in
    let wins = Hashset.init () in
    List.iter (Hashset.add wins) winning;
    List.fold_left
      (fun score n ->
        if Hashset.mem wins n then score + 1 else score)
      0 hand
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let rec pow a n = if n = 0 then 1 else a * pow a (n - 1)

let _ =
  let scores = Dynarray.init () in
  Gen.(
    of_file file
    |> map (eval Parser.line)
    |> iter (Dynarray.push scores));
  Printf.printf "Part one: %d\n"
    (Dynarray.fold
       (fun sum score ->
         if score = 0 then sum else sum + pow 2 (score - 1))
       0 scores);
  let copies = Array.make (Dynarray.length scores) 1 in
  Dynarray.iteri
    (fun pos score ->
      for i = 1 to score do
        copies.(pos + i) <- copies.(pos + i) + copies.(pos)
      done)
    scores;
  Printf.printf "Part two: %d\n" (Array.fold_left ( + ) 0 copies)
