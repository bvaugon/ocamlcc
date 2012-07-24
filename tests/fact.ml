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

let usage () =
  Printf.eprintf "Usage: %s <n> <iter-nb>\n" Sys.argv.(0);
  exit 1;
;;

if Array.length Sys.argv <> 3 then usage ();;
let n = try int_of_string Sys.argv.(1) with _ -> usage ();;
let p = try int_of_string Sys.argv.(2) with _ -> usage ();;

let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
for i = 2 to p do ignore (fact n) done;
Printf.printf "%d\n" (fact n);
;;
