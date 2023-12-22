(* let file = "test18.txt" *)
let file = "input18.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let hexa =
    let is_hexa = function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false
    in
    take_while1 is_hexa

  let line =
    let+ direction = take 1
    and+ _ = char ' '
    and+ dist = integer
    and+ _ = string " (#"
    and+ color = hexa
    and+ _ = char ')' in
    (direction, dist, color)
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let read_instructions () =
  let instr = ref [] in
  (let input = open_in file in
   try
     while true do
       instr := (input_line input |> eval Parser.line) :: !instr
     done
   with
   | End_of_file -> close_in input
   | exn ->
       close_in input;
       raise exn);
  !instr |> List.rev

let delta = function
  | "R" -> (1, 0)
  | "U" -> (0, 1)
  | "L" -> (-1, 0)
  | "D" -> (0, -1)
  | _ -> failwith "Wrong direction"

let compute_surface instructions =
  let x, y, l, s =
    List.fold_left
      (fun (x, y, l, s) (dir, dst) ->
        let dx, dy = delta dir in
        ( x + (dst * dx),
          y + (dst * dy),
          l + dst,
          s + (x * dy * dst) - (y * dx * dst) ))
      (0, 0, 0, 0) instructions
  in
  assert (x = 0);
  assert (y = 0);
  (abs s / 2) + (l / 2) + 1

let instr_from_hexa s =
  let direction = function
    | '0' -> "R"
    | '1' -> "D"
    | '2' -> "L"
    | '3' -> "U"
    | _ -> failwith "Wrong hexa direction"
  in
  ( direction s.[String.length s - 1],
    Scanf.sscanf
      (String.sub s 0 (String.length s - 1))
      "%x%!"
      (fun x -> x) )

let _ =
  let instructions = read_instructions () in
  Printf.printf "Part one: %d\n"
    (instructions
    |> List.map (fun (dir, dist, _) -> (dir, dist))
    |> compute_surface);
  Printf.printf "Part two: %d\n"
    (instructions
    |> List.map (fun (_, _, hexa) -> instr_from_hexa hexa)
    |> compute_surface)
