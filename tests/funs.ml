(*************************************************************************)
(*                                                                       *)
(*                               OCamlCC                                 *)
(*                                                                       *)
(*                    Michel Mauny, Benoit Vaugon                        *)
(*                          ENSTA ParisTech                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

let f s1 s2 =
  print_string s1;
  print_char ' ';
  print_endline s2;
in
f "Hello" "world";
let g = f "Bye" in
g "bye";
;;

(***)

let f s1 =
  print_string s1;
  print_char ':';
  fun s2 -> print_endline (s1 ^ " " ^ s2);
in
f "Cot" "cot";;

(***)

let f s1 s2 =
  print_string (s1 ^ "-" ^ s2);
  print_char ':';
  fun s3 -> print_endline (s1 ^ s2 ^ s3);
in
let g = f "Bla" in
g "lab" "alb"
;;
