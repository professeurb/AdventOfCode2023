(* let file = "test12.txt" *)
let file = "input12.txt"

let rec wait dict text post spec poss =
  match Hashtbl.find_opt dict (-1, post, poss) with
  | Some r -> r
  | None ->
      let r =
        if post = String.length text then
          if poss = Array.length spec then 1 else 0
        else if poss = Array.length spec then
          match text.[post] with
          | '#' -> 0
          | _ -> wait dict text (post + 1) spec poss
        else
          match text.[post] with
          | '.' -> wait dict text (post + 1) spec poss
          | '?' ->
              wait dict text (post + 1) spec poss
              + eat dict
                  (spec.(poss) - 1)
                  text (post + 1) spec (poss + 1)
          | '#' ->
              eat dict
                (spec.(poss) - 1)
                text (post + 1) spec (poss + 1)
          | _ -> assert false
      in
      Hashtbl.add dict (-1, post, poss) r;
      r

and eat dict count text post spec poss =
  match Hashtbl.find_opt dict (count, post, poss) with
  | Some r -> r
  | None ->
      let r =
        if count = 0 then
          if post = String.length text then
            if poss = Array.length spec then 1 else 0
          else if text.[post] = '#' then 0
          else wait dict text (post + 1) spec poss
        else if post = String.length text then 0
        else
          match text.[post] with
          | '#' -> eat dict (count - 1) text (post + 1) spec poss
          | '?' -> eat dict (count - 1) text (post + 1) spec poss
          | _ -> 0
      in
      Hashtbl.add dict (count, post, poss) r;
      r

let process_line str =
  match String.split_on_char ' ' str with
  | [ map; spec ] ->
      ( map,
        spec
        |> String.split_on_char ','
        |> List.map int_of_string
        |> List.to_seq |> Array.of_seq )
  | _ -> failwith str

let _ =
  let data = open_in file in
  let sum1 = ref 0 and sum2 = ref 0 in
  (try
     while true do
       let map, spec = data |> input_line |> process_line in
       (* Printf.printf "%s\n%!" map; *)
       let dict1 = Hashtbl.create 10 in
       sum1 := !sum1 + wait dict1 map 0 spec 0;
       let map2 =
         map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map
       and spec2 =
         let spec = spec |> Array.to_seq |> List.of_seq in
         [ spec; spec; spec; spec; spec ]
         |> List.flatten |> List.to_seq |> Array.of_seq
       in
       let dict2 = Hashtbl.create 10 in
       sum2 := !sum2 + wait dict2 map2 0 spec2 0
     done
   with
  | End_of_file -> close_in data
  | exn ->
      close_in data;
      raise exn);
  Printf.printf "Part one: %d\n" !sum1;
  Printf.printf "Part two: %d\n" !sum2
