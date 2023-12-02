let file = "test02.txt"
(* let file = "input02.txt" *)

let process_bag s =
  let r = ref 0 and g = ref 0 and b = ref 0 in
  let colors = String.split_on_char ',' s in
  List.iter
    (fun s ->
      match String.split_on_char ' ' s with
      | [ ""; i; "red" ] -> r := int_of_string i
      | [ ""; i; "green" ] -> g := int_of_string i
      | [ ""; i; "blue" ] -> b := int_of_string i
      | _ -> failwith s)
    colors;
  (!r, !g, !b)

let process_bags s =
  let bag_list = String.split_on_char ';' s in
  List.map process_bag bag_list

let process_line s =
  match String.split_on_char ':' s with
  | [ header; bags ] ->
      let id =
        String.sub header 5 (String.length header - 5)
        |> int_of_string
      in
      (id, process_bags bags)
  | _ -> failwith "No game id"

let is_possible =
  List.for_all (fun (r, g, b) -> r <= 12 && g <= 13 && b <= 14)

let _ =
  let data = open_in file in
  let sum1 = ref 0 in
  try
    while true do
      let line = input_line data in
      let id, bags = process_line line in
      if is_possible bags then sum1 := !sum1 + id
    done
  with End_of_file ->
    close_in data;
    Printf.printf "Part one: %d\n" !sum1
