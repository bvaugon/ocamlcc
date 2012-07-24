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
  if n > 0 then
    try
      f (n - 1);
      if n mod 2 = 0 then Sys.remove "/tmp/abc.ocamlcc.tmp"
      else raise Exit;
      print_endline "coucou";
    with exn ->
      f (n - 1);
      print_endline "exception";
in
f 8;
;;
