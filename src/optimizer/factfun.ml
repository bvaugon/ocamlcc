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

open Types;;
open Tools;;

module BMap = Map.Make (
  struct
    type t = instr array
    let compare = Code.compare
  end
);;

let remap_body mmap body =
  let remap_ptr ptr =
    let old_fun_id = ptr.pointed.index in
    let new_fun_id = IMap.find old_fun_id mmap in
    if old_fun_id = new_fun_id then ptr else
      { ofs = ptr.ofs ; pointed = { ptr.pointed with index = new_fun_id } }
  in
  let remap_instr instr =
    match instr.bc with
      | StaticApply (n, ptr) ->
        instr.bc <- StaticApply (n, remap_ptr ptr)
      | StaticAppterm (n, p, ptr) ->
        instr.bc <- StaticAppterm (n, p, remap_ptr ptr)
      | Closure (n, ptr) ->
        instr.bc <- Closure (n, remap_ptr ptr)
      | Closurerec (n, p, ptr, ptrs) ->
        instr.bc <- Closurerec (n, p, remap_ptr ptr, Array.map remap_ptr ptrs)
      | _ -> ()
  in
  Array.iter remap_instr body
;;

let factor_functions funs =
  let rec fix_point funs =
    Options.message ".";
    let main = IMap.find 0 funs in
    let nomain_funs = IMap.remove 0 funs in
    let f fun_id fd acc = BMap.add fd.body fun_id acc in
    let g bmap fun_id fd acc = IMap.add fun_id (BMap.find fd.body bmap) acc in
    let h _ fun_id acc = IMap.add fun_id (IMap.find fun_id nomain_funs) acc in
    let bmap = IMap.fold f nomain_funs BMap.empty in
    let mmap = IMap.fold (g bmap) nomain_funs IMap.empty in
    let new_nomain_funs = IMap.fold h mmap IMap.empty in
    let old_fun_nb = IMap.fold (fun _ _ acc -> acc + 1) nomain_funs 0 in
    let new_fun_nb = IMap.fold (fun _ _ acc -> acc + 1) new_nomain_funs 0 in
    let new_funs = IMap.add 0 main new_nomain_funs in
    IMap.iter (fun _ fd -> remap_body mmap fd.body) new_nomain_funs;
    remap_body mmap main.body;
    if old_fun_nb = new_fun_nb then new_funs else fix_point new_funs
  in
  Options.verb_start "+ Factorizing functions..";
  let result = fix_point funs in
  Options.verb_stop ();
  result
;;
