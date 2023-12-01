(* let file = "test01.txt" *)
(* let file = "test01b.txt" *)
let file = "input01.txt"

let rec next_digit_1 = function
  | ('0' .. '9' as c) :: l -> Some (Char.code c - 48, l)
  | _ :: l -> next_digit_1 l
  | [] -> None

let rec next_digit_2 = function
  | ('0' .. '9' as c) :: l -> Some (Char.code c - 48, l)
  | 'z' :: ('e' :: 'r' :: 'o' :: _ as l) -> Some (0, l)
  | 'o' :: ('n' :: 'e' :: _ as l) -> Some (1, l)
  | 't' :: ('w' :: 'o' :: _ as l) -> Some (2, l)
  | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as l) -> Some (3, l)
  | 'f' :: ('o' :: 'u' :: 'r' :: _ as l) -> Some (4, l)
  | 'f' :: ('i' :: 'v' :: 'e' :: _ as l) -> Some (5, l)
  | 's' :: ('i' :: 'x' :: _ as l) -> Some (6, l)
  | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as l) -> Some (7, l)
  | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as l) -> Some (8, l)
  | 'n' :: ('i' :: 'n' :: 'e' :: _ as l) -> Some (9, l)
  | _ :: l -> next_digit_2 l
  | [] -> None

let get_value nd str =
  let d1 = ref None and d2 = ref None in
  let rec aux l =
    match nd l with
    | Some (d, l') ->
        (match !d1 with
        | None ->
            d1 := Some d;
            d2 := Some d
        | Some _ -> d2 := Some d);
        aux l'
    | None -> ()
  in
  aux (str |> String.to_seq |> List.of_seq);
  (10 * Option.get !d1) + Option.get !d2

let _ =
  let data = open_in file in
  let sum1 = ref 0 and sum2 = ref 0 in
  try
    while true do
      let line = input_line data in
      let v1 = get_value next_digit_1 line in
      sum1 := v1 + !sum1;
      let v2 = get_value next_digit_2 line in
      sum2 := v2 + !sum2
    done
  with End_of_file ->
    close_in data;
    Printf.printf "Part one : %d\n" !sum1;
    Printf.printf "Part two : %d\n" !sum2
