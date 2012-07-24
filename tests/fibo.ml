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
  Printf.eprintf "Usage: %s <n>\n" Sys.argv.(0);
  exit 1;
;;

if Array.length Sys.argv <> 2 then usage ();;
let n = try int_of_string Sys.argv.(1) with _ -> usage ();;

let rec fibo x = if x < 2 then 1 else fibo (x - 1) + fibo (x - 2) in
Printf.printf "%d\n" (fibo n);
;;
