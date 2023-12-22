(* let file = "test12.txt" *)
let file = "input12.txt"

module Parser = struct
  open Angstrom

  let integer =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let+ number = take_while1 is_digit in
    int_of_string number

  let block =
    let is_block = function '#' | '?' -> true | _ -> false in
    take_while1 is_block

  let map =
    let is_map = function
      | '.' | '?' | '#' -> true
      | _ -> false
    in
    take_while1 is_map

  let dots = take_while (function '.' -> true | _ -> false)
  let blocks = dots *> sep_by1 dots block <* dots

  let line =
    let+ map = map
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

let preprocess_block block =
  let block = block ^ "?" in
  let blklen = String.length block in
  let position = ref 0 in
  for i = 0 to blklen - 2 do
    if block.[i] = '#' then position := i + 1
  done;
  let advance = Array.make blklen 0
  and is_starting_point = Array.make blklen true
  and last = Array.make blklen 0 in
  for i = 1 to blklen - 1 do
    if block.[i - 1] = '#' then is_starting_point.(i) <- false
  done;
  let acc = ref 0 in
  for i = blklen - 1 downto 0 do
    if block.[i] = '?' then acc := 0 else incr acc;
    advance.(i) <- !acc
  done;
  let acc = ref (blklen - 1) in
  for i = blklen - 1 downto 0 do
    if block.[i] = '#' then acc := i;
    last.(i) <- !acc
  done;
  (advance, is_starting_point, last, !position)

(*
?###????????
032100000000 advance
110001111111 is_starting_point
11006543210 last

is_starting_point == 
    *)

let rec next_starting_point adv isp width limit pos =
  if pos > limit || pos + width >= Array.length adv then None
  else if isp.(pos) then
    let delta = adv.(pos + width) in
    if delta = 0 then Some pos
    else next_starting_point adv isp width limit (pos + delta)
  else None
(* next_starting_point adv isp width limit (pos + adv.(pos) + 1) *)

let process_block adv isp endp lst widths =
  let ht = Hashtbl.create 10
  and len = Array.length adv
  and rmg = Array.fold_left (fun a b -> a + b + 1) 0 widths in
  let rec aux idx pos rmg =
    if idx = Array.length widths then
      if pos >= endp then 1 else 0
    else if pos >= len then 0
    else
      let width = widths.(idx) in
      match
        next_starting_point adv isp width
          (min lst.(pos) (len - rmg))
          pos
      with
      | None -> 0
      | Some pos' ->
          haux (idx + 1) (pos' + width + 1) (rmg - width - 1)
          + haux idx (pos' + 1) rmg
  and haux idx pos rmg =
    match Hashtbl.find_opt ht (idx, pos) with
    | Some v -> v
    | None ->
        let v = aux idx pos rmg in
        Hashtbl.add ht (idx, pos) v;
        (* Printf.printf "%d, %d: %d\n" idx pos v; *)
        v
  in
  haux 0 0 rmg

(*
let adv, isp, last, endp = preprocess_block "???????#???????????";;
process_block adv isp endp last [| 1;2|] ;;

let adv, isp, last, endp = preprocess_block "?#?#?#?#?#?#?#?";;
process_block adv isp endp last [| 1; 3; 1; 6 |] ;;

0123456789012345678
?#??????#??????????
1234567 123456789
*)

(* let print_list lst = *)
(*   print_string "[ "; *)
(*   List.iter *)
(*     (fun n -> *)
(*       print_int n; *)
(*       print_char ' ') *)
(*     lst; *)
(*   print_string "]" *)

let preprocess_blocks blocks =
  let processed_blocks = Hashtbl.create 10 in
  let block_counter = ref 0 in
  List.iter
    (fun block ->
      if not (Hashtbl.mem processed_blocks block) then (
        Hashtbl.add processed_blocks block
          (!block_counter, preprocess_block block);
        incr block_counter))
    blocks;
  processed_blocks

let hashnil, hashcons =
  let cnt = ref 1 and tbl = Hashtbl.create 10 in
  ( (0, []),
    fun a (b, l) ->
      match Hashtbl.find_opt tbl (a, b) with
      | Some n -> (n, a :: l)
      | None ->
          let n = !cnt in
          incr cnt;
          Hashtbl.add tbl (a, b) n;
          (n, a :: l) )

let process blocks widths =
  let processed_blocks =
    preprocess_blocks (blocks |> Array.to_seq |> List.of_seq)
  and tbl = Hashtbl.create 10
  and tbl2 = Hashtbl.create 10 in
  let rec aux ib iw =
    if ib = Array.length blocks then
      if iw = Array.length widths then 1 else 0
    else if iw = Array.length widths then
      let _, (badv, bisp, blst, bendp) =
        Hashtbl.find processed_blocks blocks.(ib)
      in
      let v = process_block badv bisp bendp blst [||] in
      if v > 0 then (
        assert (v = 1);
        haux (ib + 1) iw)
      else 0
    else
      let block = blocks.(ib) in
      let block_length = String.length block
      and block_id, (badv, bisp, blst, bendp) =
        Hashtbl.find processed_blocks block
      and cnt = ref 0 in
      let rec aux2 iw lw sw =
        let v =
          match Hashtbl.find_opt tbl2 (block_id, fst lw) with
          | Some v -> v
          | None ->
              (* print_string blocks.(ib); *)
              (* print_string " : "; *)
              (* print_list (lw |> List.rev); *)
              let v =
                process_block badv bisp bendp blst
                  (lw |> snd |> List.rev |> List.to_seq
                 |> Array.of_seq)
              in
              Hashtbl.add tbl2 (block_id, fst lw) v;
              (* Printf.printf " -> %d\n" v; *)
              v
        in
        if v > 0 then cnt := !cnt + (v * haux (ib + 1) iw);
        if iw < Array.length widths then
          let w = widths.(iw) in
          let sw' = sw + w + 1 in
          if sw' <= block_length + 1 then
            aux2 (iw + 1) (hashcons w lw) sw'
      in
      aux2 iw hashnil 0;
      !cnt
  and haux ib iw =
    match Hashtbl.find_opt tbl (ib, iw) with
    | Some v -> v
    | None ->
        let v = aux ib iw in
        Hashtbl.add tbl (ib, iw) v;
        v
  in
  haux 0 0

let read_input () =
  let data = open_in file in
  let entries = ref [] in
  (try
     while true do
       let entry = data |> input_line |> eval Parser.line in
       entries := entry :: !entries
     done
   with
  | End_of_file -> close_in data
  | exn ->
      close_in data;
      raise exn);
  List.rev !entries

let _ =
  let input = read_input () in
  let sum1 = ref 0 and sum2 = ref 0 in
  List.iter
    (fun (map, widths) ->
      let blocks =
        eval Parser.blocks map |> List.to_seq |> Array.of_seq
      in
      let v =
        process blocks (widths |> List.to_seq |> Array.of_seq)
      in
      (* Printf.printf "%s: %d\n" map v; *)
      sum1 := !sum1 + v;
      let map2 =
        map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map ^ "?" ^ map
      and widths2 =
        List.flatten [ widths; widths; widths; widths; widths ]
      in
      let blocks2 =
        eval Parser.blocks map2 |> List.to_seq |> Array.of_seq
      in
      let v =
        process blocks2 (widths2 |> List.to_seq |> Array.of_seq)
      in
      (* Printf.printf "%s: %d\n" map2 v; *)
      sum2 := !sum2 + v;
      ())
    input;
  Printf.printf "Part one: %d\n" !sum1;
  Printf.printf "Part two: %d\n" !sum2
