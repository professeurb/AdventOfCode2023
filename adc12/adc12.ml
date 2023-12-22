(* let file = "test12.txt" *)
let file = "input12.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let is_map = function '.' | '?' | '#' -> true | _ -> false

  let line =
    let+ map = take_while1 is_map
    and+ _ = char ' '
    and+ spec = sep_by1 (char ',') integer in
    (map, spec)
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let rec wait dict text spec =
  match Hashtbl.find_opt dict (0, 0, text, spec) with
  | Some r -> r
  | None ->
      let r =
        match (text, spec) with
        | [], [] -> 1
        | [], _ -> 0
        | '.' :: text', [] -> wait dict text' []
        | '?' :: text', [] -> wait dict text' []
        | '#' :: _, [] -> 0
        | '.' :: text', _ -> wait dict text' spec
        | '?' :: text', n :: spec' ->
            wait dict text' spec + eat dict (n - 1) text' spec'
        | '#' :: text', n :: spec' ->
            eat dict (n - 1) text' spec'
        | _ -> assert false
      in
      Hashtbl.add dict (0, 0, text, spec) r;
      r

and eat dict count text spec =
  match Hashtbl.find_opt dict (1, count, text, spec) with
  | Some r -> r
  | None ->
      let r =
        match (count, text) with
        | 0, '#' :: _ -> 0
        | 0, _ :: text' -> wait dict text' spec
        | 0, [] -> if spec = [] then 1 else 0
        | n, '#' :: text' -> eat dict (n - 1) text' spec
        | n, '?' :: text' -> eat dict (n - 1) text' spec
        | _ -> 0
      in
      Hashtbl.add dict (1, count, text, spec) r;
      r

let _ =
  let data = open_in file in
  let sum1 = ref 0 and sum2 = ref 0 in
  (try
     while true do
       let map, spec = data |> input_line |> eval Parser.line in
       (* Printf.printf "%s\n%!" map; *)
       let dict = Hashtbl.create 10 in
       (* let v = *)
       (*   wait dict (map |> String.to_seq |> List.of_seq) spec *)
       (* in *)
       (* Printf.printf "%s: %d\n" map v; *)
       sum1 :=
         !sum1
         + wait dict (map |> String.to_seq |> List.of_seq) spec;
       let map2 =
         map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map
       and spec2 =
         List.flatten [ spec; spec; spec; spec; spec ]
       in
       sum2 :=
         !sum2
         + wait dict (map2 |> String.to_seq |> List.of_seq) spec2
     done
   with
  | End_of_file -> close_in data
  | exn ->
      close_in data;
      raise exn);
  Printf.printf "Part one: %d\n" !sum1;
  Printf.printf "Part two: %d\n" !sum2
