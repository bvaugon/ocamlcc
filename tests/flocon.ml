open Graphics;;

let perim n =
  let rec f r = function
    | k when k = n -> r
    | k -> f (r*.4./.3.) (k+1)
  in
  f 3. 0
;;

let aire n =
  let rec f r l t = function
    | k when k = n -> r
    | k -> f (r +. (float_of_int t)*.l*.l*.sqrt(3.)/.4.) (l/.3.) (t*4) (k+1)
  in
  f (sqrt(3.)/.4.) (1./.3.) 3 0
;;

let rec trace nb xm ym xn yn =
  if nb <> 0 then
    let xa = float_of_int (2*xm + xn) /. 3. in
    let xc = float_of_int (xm + 2*xn) /. 3. in
    let ya = float_of_int (2*ym + yn) /. 3. in
    let yc = float_of_int (ym + 2*yn) /. 3. in
    let xb = xa +. (xc -. xa -. sqrt 3. *. (yc -. ya)) /. 2. in
    let yb = ya +. (yc -. ya +. sqrt 3. *. (xc -. xa)) /. 2. in
    trace (nb - 1) xm ym (int_of_float xa) (int_of_float ya);
    trace (nb - 1) (int_of_float xa) (int_of_float ya) (int_of_float xb)
      (int_of_float yb);
    trace (nb - 1) (int_of_float xb) (int_of_float yb) (int_of_float xc)
      (int_of_float yc);
    trace (nb - 1) (int_of_float xc) (int_of_float yc) xn yn
  else
    moveto xm ym ; lineto xn yn
;;

let xmax = 600;;
let ymax = 600;;

open_graph (" " ^ (string_of_int xmax) ^ "x" ^ (string_of_int ymax));;
set_color blue;;

let yv = ymax / 8 in
let xv = xmax / 2 in
let yu = yv + int_of_float (0.5196 *. float_of_int ymax) in
let xu = xv - int_of_float (3. *. float_of_int xmax /. 10.) in
let yw = yu in
let xw = xv + int_of_float (3. *. float_of_int xmax /. 10.) in
let rec loop = function
  | 10 -> loop 0;
  | k ->
    clear_graph ();
    moveto 20 20 ; draw_string ("N = " ^ (string_of_int (k+1)));
    moveto 200 20 ; draw_string ("P = " ^ (string_of_float (perim k)));
    moveto 400 20 ; draw_string ("A = " ^ (string_of_float (aire k)));
    trace k xv yv xu yu;
    trace k xu yu xw yw;
    trace k xw yw xv yv;
      (*      synchronize ();*)
    let t = Unix.time () in
    while Unix.time () < t +. 2. do
      Unix.sleep 1
    done;
    draw_circle (xmax/2) (ymax/2-18) 208;
    let t = Unix.time () in
    while Unix.time () < t +. 2. do
      Unix.sleep 1
    done;
    loop (k + 1)
in
  (*  auto_synchronize false;*)
loop 0;
