(* let file = "test25.txt" *)
let file = "input25.txt"

let add g k v =
  match Hashtbl.find_opt g k with
  | None -> Hashtbl.add g k [ v ]
  | Some l -> Hashtbl.replace g k (v :: l)

let read_graph file =
  let graph = Hashtbl.create 10 in
  let input = open_in file in
  (try
     while true do
       let line = input_line input in
       match String.split_on_char ' ' line with
       | h :: l ->
           let h = String.sub h 0 (String.length h - 1) in
           List.iter
             (fun v ->
               add graph h v;
               add graph v h)
             l
       | _ -> ()
     done
   with
  | End_of_file -> close_in input
  | exn ->
      close_in input;
      raise exn);
  graph

let graph = read_graph file

let rec remove_from_list v = function
  | [] -> []
  | a :: lst when a = v -> lst
  | a :: lst -> a :: remove_from_list v lst

let remove_edge g a b =
  Hashtbl.replace g a (Hashtbl.find g a |> remove_from_list b);
  Hashtbl.replace g b (Hashtbl.find g b |> remove_from_list a)

let _ =
  remove_edge graph "gxv" "tpn";
  remove_edge graph "rtt" "zcj";
  remove_edge graph "txl" "hxq"

let dfs g s =
  let cnt = ref 0 in
  let seen = Hashtbl.create 10 in
  let rec visit s =
    if not (Hashtbl.mem seen s) then (
      Hashtbl.add seen s ();
      incr cnt;
      List.iter visit (Hashtbl.find g s))
  in
  visit s;
  !cnt

(* gxv -- tpn
   rtt -- zcj
   txl -- hxq
*)
