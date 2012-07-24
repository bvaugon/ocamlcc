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
open Tools

let parse ic index =
  try
    let rec read_events n acc =
      if n = 0 then acc else
        let orig = input_binary_int ic in
        let evl = (input_value ic : debug_event list) in
        let add acc ev =
          if ev.ev_loc.loc_ghost then acc
          else IMap.add ((orig + ev.ev_pos) / 4) ev.ev_loc acc
        in
        read_events (n - 1) (List.fold_left add acc evl)
    in
    let (offset, length) = Index.find_section index Dbug in
    if length = 0 then raise Not_found;
    seek_in ic offset;
    let event_nb = input_binary_int ic in
    let events = read_events event_nb IMap.empty in
    events
  with Not_found -> IMap.empty
;;
