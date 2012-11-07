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

let flag_size = 60;;

let percentage x y = float_of_int (x * 100) /. float_of_int y;;

let print_flag oc title =
  let len = String.length title in
  let bef = flag_size / 2 - (len + 1) / 2 in
  let aft = flag_size - bef - len in
  for _i = 1 to bef do output_char oc '=' done;
  output_string oc title;
  for _i = 1 to aft do output_char oc '=' done;
  output_char oc '\n';
;;

let functions oc funs dzeta_code tc_set =
  let main = (IMap.find 0 funs).body in
  let others = IMap.remove 0 funs in
  let main_size = Array.length main in
  let foldf _ ({ fun_id = _ ; arity = arity ; body = body ; is_special = _ }
                  as fun_desc) (nb, tot, mini, maxi, arities, inln_nb) =
    let sz = Array.length body in
    let new_arities =
      try IMap.add arity (IMap.find arity arities + 1) arities
      with Not_found -> IMap.add arity 1 arities
    in
    let new_inln_nb =
      if Body.test_inlinable funs dzeta_code fun_desc then inln_nb + 1
      else inln_nb
    in
    (nb+1, tot+sz, min sz mini, max sz maxi, new_arities, new_inln_nb)
  in
  let (fun_nb, total_size, min_size, max_size, arities, inln_nb) =
    IMap.fold foldf others (0, 0, max_int, 0, IMap.empty, 0)
  in
  let tc_nb = ISet.cardinal tc_set in
  let average_size = if fun_nb <> 0 then total_size / fun_nb else 0 in
  let print_arity arity n =
    if arity = 1 then
      Printf.fprintf oc "   1 -> %6d  (%.2f%%)\n"
        n (percentage n fun_nb)
    else
      Printf.fprintf oc "                    %2d -> %6d  (%.2f%%)\n"
        arity n (percentage n fun_nb)
  in
  Printf.fprintf oc "\n\
\                       -> %6d functions\n\
\n\
Function size: minimum -> %6d instructions\n\
\               average -> %6d instructions\n\
\               maximum -> %6d instructions\n\
\               TOTAL   -> %6d instructions\n\
\n\
Size of main function  -> %6d instructions\n"
    fun_nb min_size average_size max_size total_size main_size;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "Function arities: ";
  IMap.iter print_arity arities;
  Printf.fprintf oc "\n\
Inlinable              -> %6d  (%.2f%%)\n\
With special tail call -> %6d  (%.2f%%)\n\
\n" inln_nb (percentage inln_nb fun_nb) tc_nb (percentage tc_nb fun_nb);
;;

let calls oc funs tc_set =
  let apps = ref 0 in
  let terms = ref 0 in
  let statics = ref 0 in
  let partials = ref 0 in
  let notc = ref 0 in
  let stapp = ref 0 in
  let f instr =
    match instr.bc with
      | StaticApply (_, ptr) ->
        if ISet.mem ptr.pointed.index tc_set then incr notc;
        incr apps; incr statics; incr stapp;
      | DynamicApply _   -> incr apps; incr stapp;
      | PartialApply _   -> incr apps; incr partials;
      | DynamicAppterm _ -> incr apps; incr terms;
      | StaticAppterm _  -> incr apps; incr terms; incr statics;
      | PartialAppterm _ -> incr apps; incr terms; incr partials;
      | SpecialAppterm _ -> incr apps; incr terms;
      | _ -> ()
  in
  IMap.iter (fun _ fun_desc -> Array.iter f fun_desc.body) funs;
  Printf.fprintf oc "\n\
\                       -> %6d function calls\n\
\n\
Terminal calls         -> %6d  (%.2f%%)\n\
Static calls           -> %6d  (%.2f%%)\n\
Partial calls          -> %6d  (%.2f%%)\n\
\n\
Static applies         -> %6d  (%.2f%%)\n\
Special static applies -> %6d  (%.2f%%)\n\
\n"
    !apps
    !terms (percentage !terms !apps)
    !statics (percentage !statics !apps)
    !partials (percentage !partials !apps)
    !stapp (percentage !stapp !apps)
    !notc (percentage !notc !stapp)
;;

let xconst_fun_tys oc fun_tys =
  let fun_nb = ref 0 in
  let arg_nb = ref 0 in
  let ptr_arg_nb = ref 0 in
  let ptr_res_nb = ref 0 in
  let run_gc_nb = ref 0 in
  let f _ (ptr_args, ptr_res, run_gc) =
    incr fun_nb;
    arg_nb := !arg_nb + Array.length ptr_args;
    Array.iter (fun b -> if b then incr ptr_arg_nb) ptr_args;
    if !ptr_res then incr ptr_res_nb;
    if !run_gc then incr run_gc_nb;
  in
  IMap.iter f (IMap.remove 0 fun_tys);
  Printf.fprintf oc "\n\
\                       -> %6d functions\n\
\                       -> %6d arguments\n\
\n\
Pointer arguments      -> %6d  (%.2f%%)\n\
Pointer results        -> %6d  (%.2f%%)\n\
Funs that may run GC   -> %6d  (%.2f%%)\n\n"
    !fun_nb !arg_nb !ptr_arg_nb (percentage !ptr_arg_nb !arg_nb)
    !ptr_res_nb (percentage !ptr_res_nb !fun_nb) !run_gc_nb
    (percentage !run_gc_nb !fun_nb)
;;

let xconst_ids oc dzeta_code =
  let id_cnt = ref 0 in
  let cell_cnt = ref 0 in
  let ptr_cnt = ref 0 in
  let read_cnt = ref 0 in
  let count _ (_, _, vd_map, ptr_set, read_set, _, _) =
    let f id vd =
      incr id_cnt;
      if vd = VCell then (
        incr cell_cnt;
        if ISet.mem id ptr_set then incr ptr_cnt;
        if ISet.mem id read_set then incr read_cnt;
      )
    in
    IMap.iter f vd_map;
  in
  IMap.iter count dzeta_code;
  Printf.fprintf oc "\n\
\                       -> %6d identifiers\n\
\n\
Value cells            -> %6d  (%.2f%% of idents)\n\
Pointers               -> %6d  (%.2f%% of idents, %.2f%% of cells)\n\
Read cells             -> %6d  (%.2f%% of idents, %.2f%% of cells)\n\n"
    !id_cnt
    !cell_cnt (percentage !cell_cnt !id_cnt)
    !ptr_cnt (percentage !ptr_cnt !id_cnt) (percentage !ptr_cnt !cell_cnt)
    !read_cnt (percentage !read_cnt !id_cnt) (percentage !read_cnt !cell_cnt)
;;

let analyse oc funs dzeta_code fun_tys tc_set =
  print_flag oc " Functions ";
  functions oc funs dzeta_code tc_set;
  print_flag oc " Function calls ";
  calls oc funs tc_set;
  print_flag oc " Function types ";
  xconst_fun_tys oc fun_tys;
  print_flag oc " Values ";
  xconst_ids oc dzeta_code;
  print_flag oc " End ";
  flush oc;
;;
