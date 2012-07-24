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

let rec f n =
  (try ignore (int_of_string "42") with Failure _ -> print_endline "hohoho");
  if n = 0 then 1 else f (n - 1)
in
print_int (f 100000000);
print_newline ();
;;
