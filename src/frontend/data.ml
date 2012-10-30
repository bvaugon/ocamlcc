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

open Types

let dump ic index =
  let (offset, length) =
    try Index.find_section index Data
    with Not_found ->
      failwith "invalid bytecode executable file (DATA section not found)"
  in
  let buf = String.create length in
  seek_in ic offset;
  really_input ic buf 0 length;
  buf
;;

let export oc data =
  let len = String.length data in
  let rec f i =
    if i mod 8 = 0 then Printf.fprintf oc "\n ";
    if i < len then (
      Printf.fprintf oc " 0x%02x," (int_of_char data.[i]);
      f (succ i);
    ) else (
      Printf.fprintf oc " 0x00\n";
    )
  in
  Printf.fprintf oc "unsigned char ocamlcc_global_data[] = {";
  f 0;
  Printf.fprintf oc "};\n";
;;
