(* let file = "test11.txt" *)
let file = "input11.txt"

let data () =
  let input = open_in file in
  let read_line () =
    try Some (input_line input)
    with _ ->
      close_in input;
      None
  in
  Seq.of_dispenser read_line

let galaxies =
  let lst = ref [] in
  List.iteri
    (fun i s ->
      String.iteri
        (fun j c ->
          match c with
          | '.' -> ()
          | '#' -> lst := (i, j) :: !lst
          | _ -> failwith "Wrong caracter")
        s)
    (data () |> List.of_seq);
  !lst |> List.rev

let missing lst =
  let rec aux miss = function
    | a :: (b :: _ as lst) when b <= a + 1 -> aux miss lst
    | a :: (_ :: _ as lst) ->
        aux ((a + 1) :: miss) ((a + 1) :: lst)
    | _ -> List.rev miss
  in
  aux [] (List.sort compare lst)

let cnt lst lft rgt =
  let lft, rgt = (min lft rgt, max lft rgt) in
  let rec aux = function
    | v :: lst when v < lft -> aux lst
    | v :: lst when v < rgt -> 1 + aux lst
    | _ -> 0
  in
  aux lst

let part_one () =
  let miss1 = galaxies |> List.map fst |> missing
  and miss2 = galaxies |> List.map snd |> missing
  and fact = 1000000 in
  let rec aux2 (i, j) = function
    | [] -> 0
    | (k, l) :: lst ->
        let dist =
          abs (k - i)
          + ((fact - 1) * cnt miss1 i k)
          + abs (l - j)
          + ((fact - 1) * cnt miss2 j l)
        in
        (* Printf.printf "(%d, %d) -> (%d, %d) : %d\n" i j k l dist; *)
        dist + aux2 (i, j) lst
  in
  let rec aux1 = function
    | [] -> 0
    | (i, j) :: lst -> aux2 (i, j) lst + aux1 lst
  in
  aux1 galaxies

let _ = Printf.printf "Part one: %d\n" (part_one ())
