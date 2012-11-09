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

let search_funs performed tosearch states read_set body =
  let len = Array.length body in
  let add ptr s =
    let index = ptr.pointed.index in
    if ISet.mem index performed then s else ISet.add index s
  in
  let rec f i s =
    if i = len then s else
      match body.(i).bc with
        | StaticApply (_, ptr) | StaticAppterm (_, _, ptr) ->
          f (i + 1) (add ptr s)
        | Closure (_, ptr) ->
          begin match states.(i + 1) with
            | Some { accu = accu ; stack = _ } when ISet.mem accu read_set ->
              f (i + 1) (add ptr s)
            | Some _ | None ->
              f (i + 1) s
          end
        | Closurerec (_, _, ptr, ptrs) ->
          f (i + 1) (Array.fold_right add ptrs (add ptr s))
        | _ ->
          f (i + 1) s
  in
  f 0 tosearch
;;

let clean_functions funs dzeta_code fun_tys tc_set =
  Options.verb_start "+ Cleaning functions.";
  let rec search performed tosearch =
    if ISet.is_empty tosearch then performed else
      let id = ISet.choose tosearch in
      let new_performed = ISet.add id performed in
      let new_tosearch = ISet.remove id tosearch in
      let body = (IMap.find id funs).body in
      let (_, states, _, _, read_set, _) = IMap.find id dzeta_code in
      let new_tosearch2 =
        search_funs new_performed new_tosearch states read_set body
      in
      search new_performed new_tosearch2
  in
  let used = search ISet.empty (ISet.singleton 0) in
  let filter id _ = ISet.mem id used in
  Options.message ".";
  let new_funs = IMap.filter filter funs in
  Options.message ".";
  let new_dzeta_code = IMap.filter filter dzeta_code in
  Options.message ".";
  let new_fun_tys = IMap.filter filter fun_tys in
  Options.message ".";
  let new_tc_set = ISet.inter tc_set used in
  Options.verb_stop ();
  (new_funs, new_dzeta_code, new_fun_tys, new_tc_set)
;;
