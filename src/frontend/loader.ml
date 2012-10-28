(*************************************************************************)
(*                                                                       *)
(*                               OCamlCC                                 *)
(*                                                                       *)
(*                    Michel Mauny, Benoit Vaugon                        *)
(*                          ENSTA ParisTech                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

let load input_file =
  Options.verb_start "+ Loading %S" input_file;
  let ic = open_in_bin input_file in
  Options.message ".";
  let index = Index.parse ic in
  Options.message ".";
  let prims = Prim.parse ic index in
  Options.message ".";
  let data = Data.dump ic index in
  Options.message ".";
  let code = Code.parse ic index in
  Options.message ".";
  let dbug = Dbug.parse ic index in
  close_in ic;
  Options.verb_stop();
  (prims, data, code, dbug)
;;
