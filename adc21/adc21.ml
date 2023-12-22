let file, steps = ("test21.txt", 6)

(* let file, steps = ("input21.txt", 64) *)
let mmod a b = ((a mod b) + (2 * b)) mod b

let read_grid file =
  let input = open_in file in
  let sl = ref 0 and sc = ref 0 and lc = ref 0 in
  let grid = ref [] in
  (try
     while true do
       let line = input_line input in
       let arr =
         Array.init (String.length line) (fun c ->
             match line.[c] with
             | '#' -> 1
             | '.' -> 0
             | 'S' ->
                 sl := !lc;
                 sc := c;
                 0
             | _ -> failwith "Wrong character")
       in
       grid := arr :: !grid;
       incr lc
     done
   with
  | End_of_file -> close_in input
  | exn ->
      close_in input;
      raise exn);
  (!grid |> List.rev |> List.to_seq |> Array.of_seq, !sl, !sc)

(* let print_grid grid = *)
(*   Array.iter *)
(*     (fun line -> *)
(*       Array.iter *)
(*         (function *)
(*           | 0 -> print_char '.' *)
(*           | 1 -> print_char '#' *)
(*           | _ -> print_char 'O') *)
(*         line; *)
(*       print_newline ()) *)
(*     grid *)

let _ =
  let grid, sl, sc = read_grid file and cnt = ref 0 in
  let height = Array.length grid
  and width = Array.length grid.(0) in
  let f1 = ref [ (sl, sc) ] and f2 = ref [] in
  for _ = 0 to steps do
    cnt := 0;
    List.iter
      (fun (ln, cl) ->
        if
          ln >= 0 && ln < height && cl >= 0 && cl < width
          && grid.(ln).(cl) = 0
        then (
          (* Printf.printf "%d, %d\n" ln cl; *)
          incr cnt;
          f2 :=
            (ln - 1, cl)
            :: (ln + 1, cl)
            :: (ln, cl - 1)
            :: (ln, cl + 1)
            :: !f2))
      !f1;
    f1 := List.sort_uniq compare !f2;
    f2 := []
  done;
  Printf.printf "Part one: %d\n" !cnt

let _ =
  let grid, sl, sc = read_grid file
  and cnt = ref 0
  and cnt2 = ref 0 in
  let height = Array.length grid
  and width = Array.length grid.(0) in
  let seen = Hashtbl.create 10
  and curr = ref [ (sl, sc) ]
  and next = ref [] in
  let add (ln, cl) =
    if
      grid.(mmod ln height).(mmod cl width) = 0
      && not (Hashtbl.mem seen (ln, cl))
    then (
      Hashtbl.add seen (ln, cl) ();
      incr cnt;
      next := (ln, cl) :: !next)
  in
  Hashtbl.add seen (sl, sc) ();
  incr cnt;
  for _ = 1 to 5000 do
    (* 26501365 do *)
    let buf = !cnt in
    cnt := !cnt2;
    cnt2 := buf;
    List.iter
      (fun (ln, cl) ->
        add (ln + 1, cl);
        add (ln - 1, cl);
        add (ln, cl + 1);
        add (ln, cl - 1))
      !curr;
    curr := !next;
    next := []
    (* print_grid grid; *)
    (* Printf.printf "%d: %d\n%!" i !cnt *)
  done;
  Printf.printf "Part two: %d\n%!" !cnt
