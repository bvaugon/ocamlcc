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

let import ic index =
  let (offset, _) =
    try Index.find_section index Data
    with Not_found -> failwith "code section not found"
  in
  seek_in ic offset;
  let (tbl : Obj.t array) = input_value ic in
  let f obj =
    if Obj.tag obj = Obj.int_tag && Obj.obj obj = 0
    then VInt 0 else VString ""
  in
  Array.to_list (Array.map f tbl)
;;

let dump ic index =
  let (offset, length) =
    try Index.find_section index Data
    with Not_found -> failwith "code section not found"
  in
  let buf = String.create length in
  seek_in ic offset;
  really_input ic buf 0 length;
  buf
;;

let parse ic index = {
  values = import ic index;
  dump = dump ic index;
};;

let export oc { values = _ ; dump = dump } =
  let len = String.length dump in
  let rec f i =
    if i mod 8 = 0 then Printf.fprintf oc "\n ";
    if i < len then (
      Printf.fprintf oc " 0x%02x," (int_of_char dump.[i]);
      f (succ i);
    ) else (
      Printf.fprintf oc " 0x00\n";
    )
  in
  Printf.fprintf oc "unsigned char ocamlcc_global_data[] = {";
  f 0;
  Printf.fprintf oc "};\n";
;;
