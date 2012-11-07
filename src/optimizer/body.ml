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

(***)

let test_useenv dzeta_code fun_desc =
  let (_, _, _, _, _, _, use_env) = IMap.find fun_desc.fun_id dzeta_code in
  use_env
;;

let test_inlinable funs dzeta_code fun_desc =
  match !Options.arch with
    | GEN_ARCH | NO_ARCH -> true
    | X86 | X86_64 ->
      let body = fun_desc.body in
      let cfun_arity =
        if test_useenv dzeta_code fun_desc then fun_desc.arity + 1
        else fun_desc.arity
      in
      let len = Array.length body in
      let rec f i =
        if i = len then true else
          match body.(i).bc with
            | DynamicAppterm (nargs, _) when
                nargs >= cfun_arity &&
                  (!Options.arch <> X86_64 || nargs >= 6) ->
              false
            | PartialAppterm (nargs, _) when nargs >= cfun_arity -> false
            | SpecialAppterm _ -> false
            | StaticAppterm (nargs, _, ptr) ->
              let callee_nargs =
                if test_useenv dzeta_code (IMap.find ptr.pointed.index funs)
                then nargs + 1
                else nargs
              in
              if
                callee_nargs > cfun_arity &&
                  (!Options.arch <> X86_64 || callee_nargs > 6)
              then false else f (i + 1)
            | _ -> f (i + 1)
      in
      f 0
;;

(***)

let compute_tc_set funs dzeta_code =
  let pass id fun_desc ((set, _) as acc) =
    if ISet.mem id set then acc else
      let body = fun_desc.body in
      let len = Array.length body in
      let cfun_arity =
        if test_useenv dzeta_code fun_desc then fun_desc.arity + 1
        else fun_desc.arity
      in
      let rec f i =
        if i = len then false else
          match body.(i).bc with
            | StaticAppterm (nargs, _, ptr) ->
              let callee_arity =
                if test_useenv dzeta_code (IMap.find ptr.pointed.index funs)
                then nargs + 1
                else nargs
              in
              if callee_arity > cfun_arity then true
              else if ISet.mem ptr.pointed.index set then true
              else f (i + 1)
            | DynamicAppterm _ | SpecialAppterm _ -> true
            | _ -> f (i + 1)
      in
      if f 0 then (ISet.add id set, true) else acc
  in
  let rec fixpoint acc =
    let (new_acc, flag) = IMap.fold pass funs (acc, false) in
    if flag then fixpoint new_acc else new_acc
  in
  fixpoint ISet.empty
;;

let compute_maximum_arity funs =
  let f acc instr =
    match instr.bc with
      | DynamicApply n | StaticApply (n, _) | PartialApply n
      | DynamicAppterm (n, _) | StaticAppterm (n, _, _)
      | PartialAppterm (n, _) | SpecialAppterm (n, _) -> max acc n
      | _ -> acc
  in
  let g _ fun_desc acc =
    Array.fold_left f (max acc fun_desc.arity) fun_desc.body
  in
  IMap.fold g funs 0
;;

(***)

let compute_nexts instr =
  match instr.bc with
    | Acc _ | Push | Pop _ | Assign _ | Envacc _ | DynamicApply _
    | StaticApply _ | PartialApply _ | Closure _ | Closurerec _
    | Offsetclosure _ | Getglobal _ | Getglobalfield _ | Setglobal _ | Atom _
    | Makeblock _ | Makefloatblock _ | Getfield _ | Getfloatfield _ | Setfield _
    | Setfloatfield _ | Setvectitem | Setstringchar | Poptrap | Checksignals
    | Ccall _ | Const _ | Offsetref _ | Restart | Unapp _ | Binapp _
    | Getmethod | Getpubmet _ | Getdynmet ->
      [ instr.index + 1 ]

    | Pushretaddr _ ->      (* Warning *)
      [ instr.index + 1 ] (* Warning *)

    | Grab _ ->
      [ instr.index + 1 ; instr.index - 1 ]
    | Branch ptr ->
      [ ptr.pointed.index ]

    | CondBranch cond_branch -> [
      instr.index + 1;
      (Instr.ptr_of_cond_branch cond_branch).pointed.index;
    ]

    | Pushtrap ptr ->                         (* Warning *)
      [ instr.index + 1 ; ptr.pointed.index ] (* Warning *)

    | Switch (_, _, tbl) ->
      Array.fold_left (fun acc ptr -> ptr.pointed.index :: acc) [] tbl

    | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _ | SpecialAppterm _
    | Return _ | Raise | Stop ->
      [ ]
;;

let update_pointed map instr =
  let f ptr = { ofs = -1 ; pointed = IMap.find ptr.pointed.index map } in
  match instr.bc with
    | Acc _ | Push | Pop _ | Assign _ | Envacc _ | DynamicApply _
    | StaticApply _ | PartialApply _ | Offsetclosure _ | Getglobal _
    | Getglobalfield _ | Setglobal _ | Atom _ | Makeblock _ | Makefloatblock _
    | Getfield _ | Getfloatfield _ | Setfield _ | Setfloatfield _
    | Setvectitem | Setstringchar | Poptrap | Checksignals | Ccall _ | Const _
    | Offsetref _ | Restart | Getmethod | Getpubmet _ | Getdynmet | Grab _
    | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _ | SpecialAppterm _
    | Return _ | Raise | Unapp _ | Binapp _ | Stop -> ()

    | Closure _ | Closurerec _ -> ((* do not touch ptr *))

    | Pushretaddr ptr -> instr.bc <- Pushretaddr (f ptr)
    | Branch ptr -> instr.bc <- Branch (f ptr)
    | CondBranch cb -> instr.bc <- CondBranch (Instr.ptr_map_cond_branch f cb)
    | Pushtrap ptr -> instr.bc <- Pushtrap (f ptr)

    | Switch (sl, st, tbl) -> instr.bc <- Switch (sl, st, Array.map f tbl)
;;

let mark_connex code =
  let nb_instr = Array.length code in
  let fun_ids = Array.make nb_instr (-1) in
  let rec num_connex fun_id ind =
    if ind < 0 || ind >= nb_instr then failwith "invalid bytecode: open";
    if fun_ids.(ind) <> fun_id then (
      if fun_ids.(ind) <> -1 then failwith "overlap functions";
      fun_ids.(ind) <- fun_id;
      List.iter (num_connex fun_id) (compute_nexts code.(ind));
    );
  in
  let manage_closure instr =
    let f ptr =
      let index = ptr.pointed.index in num_connex index index
    in
    match instr.bc with
      | Closure (_, ptr) -> f ptr
      | Closurerec (_, _, ptr, tbl) -> f ptr; Array.iter f tbl;
      | _ -> ()
  in
  num_connex 0 0;
  Array.iter manage_closure code;
  fun_ids
;;

let make_index fun_ids =
  let nb_instr = Array.length fun_ids in
  let rec f ind index =
    if ind = -1 then index else
      let fun_id = fun_ids.(ind) in
      let new_index =
        try IMap.add fun_id (ind :: IMap.find fun_id index) index
        with Not_found -> IMap.add fun_id [ ind ] index
      in
      f (ind - 1) new_index
  in
  IMap.remove (-1) (f (nb_instr - 1) IMap.empty)
;;

let fix_envaccs funs =
  let offset_envaccs ofs fun_id =
    let { fun_id = _ ; arity = _ ; body = body ; is_special = _ } =
      IMap.find fun_id funs
    in
    for i = 0 to Array.length body - 1 do
      match body.(i).bc with
        | Envacc n -> body.(i).bc <- Envacc (n + ofs)
        | _ -> ()
    done;
  in
  let manage_closure instr =
    match instr.bc with
      | Closure (_, ptr) -> offset_envaccs 1 ptr.pointed.index
      | Closurerec (_, _, ptr, tbl) ->
        let nfunc = 1 + Array.length tbl in
        let f i ptr = offset_envaccs (nfunc - i - 1) ptr.pointed.index in
        f (-1) ptr;
        Array.iteri f tbl;
      | _ -> ()
  in
  let manage_fun_desc _ fun_desc =
    Array.iter manage_closure fun_desc.body;
  in
  IMap.iter manage_fun_desc funs;
;;

let update_is_pointed body =
  let clear_is_pointed instr = instr.is_pointed <- false in
  let tag_pointed_ptr ptr = ptr.pointed.is_pointed <- true in
  let tag_pointed_instr instr =
    match instr.bc with
      | Acc _ | Push | Pop _ | Assign _ | Envacc _ | DynamicApply _
      | StaticApply _ | PartialApply _ | Offsetclosure _ | Getglobal _
      | Getglobalfield _ | Setglobal _ | Atom _ | Makeblock _ | Makefloatblock _
      | Getfield _ | Getfloatfield _ | Setfield _ | Setfloatfield _
      | Setvectitem | Setstringchar | Poptrap | Checksignals | Ccall _ | Const _
      | Offsetref _ | Restart | Getmethod | Getpubmet _ | Getdynmet | Grab _
      | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _ | SpecialAppterm _
      | Return _ | Raise | Unapp _ | Binapp _ | Stop -> ()

      | Closure _ | Closurerec _ | Pushretaddr _ -> ((* do not touch ptr *))

      | Branch ptr -> tag_pointed_ptr ptr
      | CondBranch cb -> tag_pointed_ptr (Instr.ptr_of_cond_branch cb)
      | Pushtrap ptr -> tag_pointed_ptr ptr

      | Switch (_, _, tbl) -> Array.iter tag_pointed_ptr tbl
  in
  Array.iter clear_is_pointed body;
  Array.iter tag_pointed_instr body;
;;

let is_special_tail_call body =
  let len = Array.length body in
  let rec f i x y =
    if i = len then false else
      match body.(i).bc with
        | Pushtrap _ ->
          if y then true else f (i + 1) true false
        | Ccall (n, _) when n >= 6 ->
          if y then true else f (i + 1) true false
        | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _
        | SpecialAppterm _ ->
          if x then true else f (i + 1) false true
        | _ ->
          f (i + 1) x y
  in
  f 0 false false
;;

let set_special_appterms body =
  let f i instr =
    match instr.bc with
      | DynamicAppterm (n, s) | StaticAppterm (n, s, _)
      | PartialAppterm (n, s) ->
        body.(i).bc <- SpecialAppterm (n, s)
      | _ -> ()
  in
  Array.iteri f body
;;

let make_funs index code =
  let make_block fun_id inds =
    let (old_body, arity) =
      match List.map (fun ind -> code.(ind)) inds with
        | { addr = _ ; index = _ ; bc = Restart ; is_pointed = _ } ::
            { addr = _ ; index = _ ; bc = Grab n ; is_pointed = _ } :: rest ->
          (rest, n + 1)
        | rest -> (rest, if fun_id = 0 then 0 else 1)
    in
    let size = List.length old_body in
    let new_body = Array.make size Instr.fake in
    let f (new_ind, map) old_instr =
      let old_ind = old_instr.index in
      let new_instr = { old_instr with index = new_ind } in
      new_body.(new_ind) <- new_instr;
      (new_ind + 1, IMap.add old_ind new_instr map)
    in
    let (_, map) = List.fold_left f (0, IMap.empty) old_body in
    Array.iter (update_pointed map) new_body;
    update_is_pointed new_body;
    let is_special = is_special_tail_call new_body in
    if is_special then set_special_appterms new_body;
    {
      arity = arity;
      fun_id = fun_id;
      body = new_body;
      is_special = is_special;
    }
  in
  let foldf fun_id inds blocks =
    IMap.add fun_id (make_block fun_id inds) blocks
  in
  let funs = IMap.fold foldf index IMap.empty in
  fix_envaccs funs;
  funs
;;

let create code =
  Options.verb_start "+ Decompiling functions.";
  let fun_ids = mark_connex code in
  Options.message ".";
  let index = make_index fun_ids in
  Options.message ".";
  let funs = make_funs index code in
  Options.verb_stop ();
  funs
;;
