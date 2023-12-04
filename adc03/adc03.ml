(* let file = "test03.txt" *)
let file = "input03.txt"
let is_symbol = function '.' | '0' .. '9' -> false | _ -> true

let is_digit = function
  | '0' .. '9' as c -> Some (Char.code c - 48)
  | _ -> None

let read_grid data =
  let lines = ref [] in
  (try
     while true do
       lines :=
         (input_line data |> String.to_seq |> Array.of_seq)
         :: !lines
     done
   with End_of_file -> ());
  !lines |> List.rev |> List.to_seq |> Array.of_seq

let get_surrounding tbl l a b =
  let srnd = ref [] in
  let test k = if Hashtbl.mem tbl k then srnd := k :: !srnd in
  test (l, a - 1);
  test (l, b);
  for c = a - 1 to b do
    test (l - 1, c);
    test (l + 1, c)
  done;
  !srnd

let get_symbols grid =
  let symbols = Hashtbl.create 10 in
  for l = 0 to Array.length grid - 1 do
    for c = 0 to Array.length grid.(0) - 1 do
      let char = grid.(l).(c) in
      if is_symbol char then Hashtbl.add symbols (l, c) char
    done
  done;
  symbols

let get_numbers grid =
  let numbers = Hashtbl.create 10 in
  let rec scan_line line col =
    if col < Array.length grid.(line) then
      match is_digit grid.(line).(col) with
      | Some d -> scan_number line (col + 1) col d
      | None -> scan_line line (col + 1)
  and scan_number line col start number =
    if col < Array.length grid.(line) then (
      match is_digit grid.(line).(col) with
      | Some d ->
          scan_number line (col + 1) start ((10 * number) + d)
      | None ->
          Hashtbl.add numbers (line, start, col) number;
          scan_line line (col + 1))
    else Hashtbl.add numbers (line, start, col) number
  in
  for line = 0 to Array.length grid - 1 do
    scan_line line 0
  done;
  numbers

let _ =
  let data = open_in file in
  let grid = read_grid data in
  let numbers = get_numbers grid
  and symbols = get_symbols grid in
  Printf.printf "Part one: %d\n"
    (Hashtbl.fold
       (fun (l, a, b) n sum ->
         match get_surrounding symbols l a b with
         | [] -> sum
         | _ -> sum + n)
       numbers 0);
  let gears = Hashtbl.create 10 in
  Hashtbl.iter
    (fun pos symb -> if symb = '*' then Hashtbl.add gears pos [])
    symbols;
  Hashtbl.iter
    (fun (l, a, b) n ->
      List.iter
        (fun pos ->
          Hashtbl.replace gears pos (n :: Hashtbl.find gears pos))
        (get_surrounding gears l a b))
    numbers;
  Printf.printf "Part two: %d\n"
    (Hashtbl.fold
       (fun _ l sum ->
         match l with [ a; b ] -> sum + (a * b) | _ -> sum)
       gears 0)
