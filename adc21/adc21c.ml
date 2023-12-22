(* let file = "test21.txt" *)
let file = "input21.txt"

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

let bfs arr grid sl sc =
  let height = Array.length grid
  and width = Array.length grid.(0) in
  let curr = ref [ (sl, sc) ]
  and next = ref []
  and seen = Hashtbl.create 10
  and cnt = ref 0
  and cnt2 = ref 0 in
  let add (ln, cl) =
    if
      ln >= 0 && ln < height && cl >= 0 && cl < width
      && grid.(ln).(cl) = 0
      && not (Hashtbl.mem seen (ln, cl))
    then (
      Hashtbl.add seen (ln, cl) ();
      incr cnt;
      next := (ln, cl) :: !next)
  in
  Hashtbl.add seen (sl, sc) ();
  incr cnt;
  arr.(0) <- arr.(0) + !cnt;
  for i = 1 to height + width do
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
    next := [];
    arr.(i) <- arr.(i) + !cnt
  done

let ( += ) a b =
  (* Printf.printf "+ %d %!" b; *)
  a := !a + b

(* The terms ... - ((target - d) mod 2)
    were REALLY hard to get right. *)
let score center edges corners sq target =
  (* Printf.printf "...%d, %d: " sq target; *)
  let sum = ref 0 in
  sum += center.(min target ((4 * sq) + 2 - (target mod 2)));
  (*
  First, lines...

  from (2 sq + 1) k - sq to (2 sq + 1) k + 2 sq
  soit (2 sq + 1) k + 2 sq <= target
     <=> k <= (target - 2 sq) / (2 sq + 1)
  et target < (2 sq + 1) k - sq
     <=> k > (target + sq) / (2 sq + 1)

  *)
  let ka = (target - (2 * sq)) / ((2 * sq) + 1)
  and kb = (target + sq) / ((2 * sq) + 1) in
  sum
  += (ka + 1) / 2
     * edges.((4 * sq) + 2 - ((target + sq + 1) mod 2));
  sum += (ka / 2 * edges.((4 * sq) + 2 - ((target + sq) mod 2)));
  for k = ka + 1 to kb do
    let d = target - ((((2 * sq) + 1) * k) - sq) in
    assert (d >= 0);
    sum += edges.(min d ((4 * sq) + 2 - (d mod 2)))
  done;
  (*
  Second,quadrants...

  from (2 * sq + 1) * (k + l) - 2 sq to (2 * sq + 1) * (k + l) + 2 sq
  soit (2 * sq + 1) * (k + l) + 2 sq <= target
      <=> k + l <= (target - 2 sq) / (2 sq + 1)
  et target < (2 sq + 1) (k + l) - 2 sq
      <=> k + l > (target + 2 sq) / (2 sq + 1)
  *)
  let kla = (target - (2 * sq)) / ((2 * sq) + 1)
  and klb = (target + (2 * sq)) / ((2 * sq) + 1) in
  if kla >= 2 then (
    sum
    += kla / 2 * (kla / 2)
       * corners.((4 * sq) + 2 - (target mod 2));
    sum
    += ((kla * (kla - 1) / 2) - (kla / 2 * (kla / 2)))
       * corners.((4 * sq) + 2 - ((target + 1) mod 2)));
  if kla >= 1 then
    for kl = kla + 1 to klb do
      let d = target - ((((2 * sq) + 1) * kl) - (2 * sq)) in
      assert (d >= 0);
      sum
      += ((kl - 1) * corners.(min d ((4 * sq) + 2 - (d mod 2))))
    done;
  !sum

let _ =
  let grid, sl, sc = read_grid file in
  let sq = sl in
  assert (sc = sq);
  assert (Array.length grid = (2 * sq) + 1);
  assert (Array.length grid.(0) = (2 * sq) + 1);
  let center = Array.make ((4 * sq) + 3) 0
  and edges = Array.make ((4 * sq) + 3) 0
  and corners = Array.make ((4 * sq) + 3) 0 in
  bfs center grid sq sq;
  bfs edges grid sq 0;
  bfs edges grid sq (2 * sq);
  bfs edges grid 0 sq;
  bfs edges grid (2 * sq) sq;
  bfs corners grid 0 0;
  bfs corners grid 0 (2 * sq);
  bfs corners grid (2 * sq) (2 * sq);
  bfs corners grid (2 * sq) 0;
  Printf.printf "Part one: %d\n"
    (score center edges corners sq 64);
  Printf.printf "Part two: %d\n"
    (score center edges corners sq 26501365)

(* somme des u - 1 pour u pair et 2 <= u <= kl *)
(*
let rec bli u =
  if u <= 1 then 0
  else if u mod 2 = 1 then bli (u - 1)
  else u - 1 + bli (u - 1)

let rec bla u =
  if u <= 2 then 0
  else if u mod 2 = 0 then bla (u - 1)
  else u - 1 + bla (u - 1)

let _ =
  for i = 0 to 20 do
    Printf.printf "%d : %d, %d | %d, %d \n" i (bli i) (bla i)
      (i / 2 * (i / 2))
      ((i * (i - 1) / 2) - (i / 2 * (i / 2)))
  done
*)
