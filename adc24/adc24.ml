let file, wmin, wmax = ("test24.txt", 7, 27)

let file, wmin, wmax =
  ("input24.txt", 200000000000000, 400000000000000)

let wmin = float_of_int wmin
and wmax = float_of_int wmax

module Parser = struct
  open Angstrom

  let is_space = function ' ' -> true | _ -> false

  let natural =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    take_while1 is_digit >>| int_of_string

  let integer =
    take_while is_space
    *> (natural
       <|> let+ _ = char '-' and+ n = natural in
           -n)

  let line =
    let+ px = integer <* string ","
    and+ py = integer <* string ","
    and+ pz = integer <* string " @"
    and+ vx = integer <* string ","
    and+ vy = integer <* string ","
    and+ vz = integer <* end_of_line in
    ((px, py, pz), (vx, vy, vz))

  let data = many1 line
end

let eval parser str =
  match
    Angstrom.(parse_string ~consume:Consume.All parser str)
  with
  | Ok r -> r
  | Error err -> failwith err

let read_input file =
  let input = open_in file in
  in_channel_length input |> really_input_string input

(* let test ((xa, ya, za), (vxa, vya, vza)) *)
(*     ((xb, yb, zb), (vxb, vyb, vzb)) = *)
(*   let dx = xb - xa and dy = yb - ya in *)
(*   Printf.printf "%d, %d, %d @ %d, %d, %d\n" xa ya za vxa vya vza; *)
(*   Printf.printf "%d, %d, %d @ %d, %d, %d\n" xb yb zb vxb vyb vzb; *)
(* (* *)
   (*   xa + ta vxa = xb + tb vxb *)
   (*   ya + ta vya = yb + tb vyb *)
   (**)
   (* ta vxa - tb vxb = xb - xa *)
   (* ta vya - tb vyb = yb - ya *)
   (* *) *)
(*   let d = (vya * vxb) - (vxa * vyb) in *)
(*   if d = 0 then ( *)
(*     Printf.printf "... d = 0\n"; *)
(*     false) *)
(*   else *)
(*     let d_ta = (vxb * dy) - (vyb * dx) *)
(*     and d_tb = (vxa * dy) - (vya * dx) in *)
(*     if *)
(*       (d > 0 && (d_ta < 0 || d_tb < 0)) *)
(*       || (d < 0 && (d_ta > 0 || d_tb > 0)) *)
(*     then ( *)
(*       Printf.printf "... past\n"; *)
(*       false) *)
(*     else *)
(*       let d_x = *)
(*         (xa * vxb * vya) *)
(*         - (xb * vyb * vxa) *)
(*         - (ya * vxa * vxb) *)
(*         + (yb * vxa * vxb) *)
(*       and d_y = *)
(*         (xa * vya * vyb) *)
(*         - (xb * vya * vyb) *)
(*         - (ya * vxa * vyb) *)
(*         + (yb * vya * vxb) *)
(*       in *)
(*       Printf.printf "... %f %f\n" *)
(*         (float_of_int d_x /. float_of_int d) *)
(*         (float_of_int d_y /. float_of_int d); *)
(*       d > 0 *)
(*       && wmin * d <= d_x *)
(*       && d_x <= wmax * d *)
(*       && wmin * d <= d_y *)
(*       && d_y <= wmax * d *)
(*       || d < 0 *)
(*          && wmin * d >= d_x *)
(*          && d_x >= wmax * d *)
(*          && wmin * d >= d_y *)
(*          && d_y >= wmax * d *)

let test ((xa, ya, _), (vxa, vya, _)) ((xb, yb, _), (vxb, vyb, _))
    =
  let dx = xb - xa |> float_of_int
  and dy = yb - ya |> float_of_int in
  let d = (vya * vxb) - (vxa * vyb) in
  if d = 0 then (
    Printf.printf "... d = 0\n";
    false)
  else
    let d = float_of_int d
    and vxa = float_of_int vxa
    and vya = float_of_int vya
    and vxb = float_of_int vxb
    and vyb = float_of_int vyb in
    let d_ta = (vxb *. dy) -. (vyb *. dx)
    and d_tb = (vxa *. dy) -. (vya *. dx) in
    if d_ta /. d < 0. || d_tb /. d < 0. then (
      Printf.printf "... past\n";
      false)
    else
      let xa = float_of_int xa
      and ya = float_of_int ya
      and xb = float_of_int xb
      and yb = float_of_int yb in
      let d_x =
        (xa *. vxb *. vya)
        -. (xb *. vyb *. vxa)
        -. (ya *. vxa *. vxb)
        +. (yb *. vxa *. vxb)
      and d_y =
        (xa *. vya *. vyb)
        -. (xb *. vya *. vyb)
        -. (ya *. vxa *. vyb)
        +. (yb *. vya *. vxb)
      in
      Printf.printf "... %f %f\n" (d_x /. d) (d_y /. d);
      wmin <= d_x /. d
      && d_x /. d <= wmax
      && wmin <= d_y /. d
      && d_y /. d <= wmax

let _ =
  let data = read_input file |> eval Parser.data in
  let cnt = ref 0 in
  let rec aux = function
    | [] -> ()
    | a :: l ->
        List.iter (fun b -> if test a b then incr cnt) l;
        aux l
  in
  aux data;
  Printf.printf "Part one: %d\n" !cnt

let test2 ((xa, ya, za), (vxa, vya, vza))
    ((xb, yb, zb), (vxb, vyb, vzb)) =
  let dx = xb - xa |> float_of_int
  and dy = yb - ya |> float_of_int
  and dz = zb - za |> float_of_int in
  let d = (vya * vxb) - (vxa * vyb) in
  if d = 0 then failwith "Nooooo!"
  else
    let d = float_of_int d
    and vxa = float_of_int vxa
    and vya = float_of_int vya
    and vza = float_of_int vza
    and vxb = float_of_int vxb
    and vyb = float_of_int vyb
    and vzb = float_of_int vzb in
    let d_ta = (vxb *. dy) -. (vyb *. dx)
    and d_tb = (vxa *. dy) -. (vya *. dx) in
    assert (
      float_of_int za
      +. (d_ta *. vza /. d)
      -. (float_of_int zb +. (d_tb *. vzb /. d))
      < 0.1);
    d_ta /. d

let tt =
  test2
    ( ( 422_521_403_380_479,
        268_293_246_383_898,
        153_073_450_808_511 ),
      (-289, 12, 234) )

let rec min1 = function
  | [] -> failwith "Empty"
  | [ a ] -> a
  | a :: l -> min a (min1 l)

let x0, y0, z0 =
  (422_521_403_380_479, 268_293_246_383_898, 153_073_450_808_511)

let vx0, vy0, vz0 = (-289, 12, 234)
let dt = 57159548726
let x1 = x0 + ((dt - 1) * vx0)
let y1 = y0 + ((dt - 1) * vy0)
let z1 = z0 + ((dt - 1) * vz0)
let tt2 = test2 ((x1, y1, z1), (vx0, vy0, vz0))

let ttt ((xa, ya, za), (vxa, vya, vza))
    ((xb, yb, zb), (vxb, vyb, vzb)) =
  let tx = if vxb = vxa then 0 else (xa - xb) / (vxb - vxa)
  and ty = if vyb = vya then 0 else (ya - yb) / (vyb - vya)
  and tz = if vzb = vza then 0 else (za - zb) / (vzb - vza) in
  assert (xa - xb = tx * (vxb - vxa));
  assert (ya - yb = ty * (vyb - vya));
  assert (za - zb = tz * (vzb - vza));
  if tx <> 0 && ty <> 0 then assert (tx = ty);
  if tx <> 0 && tz <> 0 then assert (tx = tz);
  if ty <> 0 && tz <> 0 then assert (ty = tz);
  if tx <> 0 then tx else if ty <> 0 then ty else tz

(* (xa + t vxa = xb + t vxb => xa - xb = t (vxb - vxa)) *)
