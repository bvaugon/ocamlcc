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

open Macroc;;
open Types;;
open Tools;;

exception Dead_code;;

let max_young_wosize = 256;;
let double_wosize = 2;;
let closure_tag = Obj.closure_tag;;

let compute_location funs dbug fun_id =
  try
    let fun_desc = IMap.find fun_id funs in
    Some (IMap.find fun_desc.body.(0).addr dbug)
  with Not_found ->
    None
;;

let compute_fun_signature fun_id use_env arity =
  if fun_id = 0 then {
    fs_ret_type = TVoid;
    fs_name     = "ocamlcc_bytecode_main";
    fs_params   = [];
    fs_static   = true;
  } else (
    let gen_param i = { vd_type = TValue; vd_name = "p" ^ string_of_int i } in
    let fname = "f" ^ string_of_int fun_id in
    match !Options.arch with
      | NO_ARCH -> {
        fs_ret_type = TValue;
        fs_name     = fname;
        fs_params   = [ gen_param 0 ];
        fs_static   = true;
      }
      | GEN_ARCH | X86 | X86_64 ->
        let params =
          let rec gen_params i acc =
            if i < 0 then acc else gen_params (i - 1) (gen_param i :: acc)
          in
          let init =
            if use_env then [ { vd_type = TValue; vd_name = "env" } ] else []
          in
          gen_params (arity - 1) init
        in {
          fs_ret_type = TValue;
          fs_name     = fname;
          fs_params   = params;
          fs_static   = true;
        }
  )
;;

let compute_fun_decl fun_desc use_env inlinable location = {
  fdc_location  = location;
  fdc_signature = compute_fun_signature fun_desc.fun_id use_env fun_desc.arity;
  fdc_noinline  = not inlinable;
};;

let compute_fun_def fun_desc use_env locals body location = {
  fdf_location  = location;
  fdf_signature = compute_fun_signature fun_desc.fun_id use_env fun_desc.arity;
  fdf_locals    = locals;
  fdf_body      = body;
  fdf_special   = if fun_desc.is_special then Some fun_desc.fun_id else None;
};;

let compute_fun_locals arity var_nb use_tmp arg_depths read_args =
  let gen_var c i = { vd_type = TValue; vd_name = Printf.sprintf "%c%d" c i } in
  let params =
    match !Options.arch with
      | GEN_ARCH | X86 | X86_64 -> []
      | NO_ARCH ->
        let rec f i acc =
          if i <= 0 then acc else
            if ISet.mem i read_args && not (IMap.mem i arg_depths) then
              f (i - 1) (gen_var 'p' i :: acc)
            else
              f (i - 1) acc
        in
        f (arity - 1) []
  in
  let vars =
    let rec f i acc = if i < 0 then acc else f (i - 1) (gen_var 'v' i :: acc) in
    f (var_nb - 1) []
  in
  let tmp = if use_tmp then [ { vd_type = TValue; vd_name = "tmp" } ] else [] in
  let sp = [ { vd_type = TPValue; vd_name = "sp" } ] in
  params @ vars @ tmp @ sp
;;

let compute_fun_init puti use_env arity arg_depths read_args =
  puti (IAffect (LSp, ELvalue (LGlobal "caml_extern_sp")));
  match !Options.arch with
    | NO_ARCH ->
      if use_env then
        puti (IAffect (LArray (LSp, -1),
                       ELvalue (LGlobal "ocamlcc_global_env")));
      for i = 0 to arity - 1 do
        try
          let ofs = IMap.find i arg_depths in
          if i = 0 then
            puti (IAffect (LArray (LSp, -ofs), ELvalue (LParam 0)))
          else
            let c = LArray (LGlobal "ocamlcc_global_params", i - 1) in
            puti (IAffect (LArray (LSp, -ofs), ELvalue c))
        with Not_found ->
          if i <> 0 && ISet.mem i read_args then
            let c = LArray (LGlobal "ocamlcc_global_params", i - 1) in
            puti (IAffect (LParam i, ELvalue c))
      done
    | GEN_ARCH | X86 | X86_64 ->
      if use_env then puti (IAffect (LArray (LSp, -1), ELvalue LEnv));
      for i = 0 to arity - 1 do
        try
          let ofs = IMap.find i arg_depths in
          puti (IAffect (LArray (LSp, -ofs), ELvalue (LParam i)))
        with Not_found -> ()
      done
;;

let compute_fun prims dbug funs fun_infos tc_set fun_id {
  fun_desc  = fun_desc;
  states    = states;
  idvd_map  = idvd_map;
  ptr_set   = ptr_set;
  read_set  = read_set;
  read_args = read_args;
} =
  let body = fun_desc.body in
  let instrs = ref [] in
  let puti instr = instrs := instr :: !instrs in
  let putm macro = puti (IMacro macro) in
  let catch_list = ref [] in
  let env_usages = (IMap.find fun_id fun_infos).env_usages in
  let use_env = Body.test_useenv fun_infos fun_desc in
  let cfun_arity = if use_env then fun_desc.arity + 1 else fun_desc.arity in
  let is_read id = ISet.mem id read_set in
  let is_ptr id = ISet.mem id ptr_set in
  let is_cell id = IMap.find id idvd_map = VCell in
  let is_arg id =
    match IMap.find id idvd_map with
      | VArg _ -> true
      | _ -> false
  in
  let use_tmp = ref false in
  let (args_ofs, arg_depths) =
    let f (n, depth, map) id =
      if is_ptr id then (n + 1, depth + 1, IMap.add n depth map)
      else (n + 1, depth, map)
    in
    match states.(0) with
      | None -> assert false
      | Some state ->
        let init_pdepth = if use_env then 2 else 1 in
        let (_, depth, map) =
          Stk.fold_left f (0, init_pdepth, IMap.empty) state.stack
        in
        (depth, map)
  in
  let (var_nb, var_levels, ptr_depths) =
    let add id depth map =
      try assert (IMap.find id map = depth); map
      with Not_found -> IMap.add id depth map
    in
    let f id (vmap, pmap, vlevel, pdepth) =
      if is_arg id then (vmap, pmap, vlevel, pdepth)
      else if is_ptr id then (vmap, add id pdepth pmap, vlevel, pdepth + 1)
      else if is_read id then (add id vlevel vmap, pmap, vlevel + 1, pdepth)
      else (vmap, pmap, vlevel, pdepth)
    in
    let g ((var_nb, vmap, pmap) as acc) state_opt =
      match state_opt with
        | None -> acc
        | Some state ->
          let init_pdepth = args_ofs in
          let init_vmap =
            if is_read state.accu then add state.accu 0 vmap else vmap
          in
          let (vmap', pmap', vlevel', _) =
            Stk.fold_right f state.stack
              (init_vmap, pmap, 1, init_pdepth)
          in
          (max var_nb vlevel', vmap', pmap')
    in
    let (var_nb, vmap, pmap) =
      Array.fold_left g (0, IMap.empty, IMap.empty) states
    in
    ((if IMap.is_empty vmap then 0 else var_nb), vmap, pmap)
  in
  let get_state ind =
    match states.(ind) with
      | None -> raise Dead_code
      | Some state -> state
  in
  let get_accu_id ind = (get_state ind).accu in
  let get_stack_id ind n = Stk.acc n (get_state ind).stack in
  let compute_frame_size ind =
    let f acc id = if is_ptr id && not (is_arg id) then acc + 1 else acc in
    let init_size = args_ofs - 1 in
    Stk.fold_left f init_size (get_state ind).stack
  in
  let export_constint n = EVal_int n in
  let export_glob n = EGlob n in
  let export_glob_field (n, p) = EGlobField (n, p) in
  let export_atom tag = EAtom tag in
  let export_clsr ofs = EOffset (ELvalue (LArray (LSp, -1)), ofs * 3 / 2) in
  let export_envacc n =
    let orig_env_index = match fun_desc.env_desc with
      | ENone -> assert false
      | ENonRec _ -> n
      | ERec (_, ofs, _) -> n - 3 * ofs
    in
    let rec f i new_env_index =
      if env_usages.(i) then
        if i = orig_env_index then new_env_index
        else f (i + 1) new_env_index
      else
        f (i + 1) (new_env_index - 1)
    in
    EField (ELvalue (LArray (LSp, -1)), f 0 n + 2)
  in
  let export_arg n =
    try ELvalue (LArray (LSp, -(IMap.find n arg_depths)))
    with Not_found -> ELvalue (LParam n)
  in
  let export_cell id =
    if is_ptr id && IMap.mem id ptr_depths then
      LArray (LSp, -(IMap.find id ptr_depths))
    else
      LVar (IMap.find id var_levels)
  in
  let export_val_desc id =
    match IMap.find id idvd_map with
      | VConst n -> export_constint n
      | VGlob n -> export_glob n
      | VGlobField (n, p) -> export_glob_field (n, p)
      | VAtom tag -> export_atom tag
      | VClsr ofs -> export_clsr ofs
      | VEnv n -> export_envacc n
      | VArg n -> export_arg n
      | VPtr _ -> assert false
      | VCell -> ELvalue (export_cell id)
  in
  let export_move src_id dst_id =
    if is_read dst_id then (
      assert (is_cell dst_id);
      let src = export_val_desc src_id in
      let dst = export_cell dst_id in
      putm (MOVE (src, dst));
    )
  in
  let export_gen_acc export_x x ind =
    let dst_id = get_accu_id (ind + 1) in
    if is_read dst_id then (
      assert (is_cell dst_id);
      let src = export_x x in
      let dst = export_cell dst_id in
      putm (MOVE (src, dst));
    )
  in
  let export_unapp ind unop =
    let dst_id = get_accu_id (ind + 1) in
    if is_read dst_id then (
      assert (is_cell dst_id);
      let src_id = get_accu_id ind in
      let src = export_val_desc src_id in
      let dst = export_cell dst_id in
      putm (
        match unop with
          | Boolnot     -> BOOLNOT (src, dst)
          | Offsetint n -> OFFSETINT (n, src, dst)
          | Negint      -> NEGINT (src, dst)
          | Isint       -> ISINT (src, dst)
          | Vectlength  -> use_tmp := true; VECTLENGTH (src, dst)
      )
    )
  in
  let export_binapp ind binop =
    let dst_id = get_accu_id (ind + 1) in
    let may_raise = binop = Divint || binop = Modint in
    if is_read dst_id || may_raise then (
      assert (is_cell dst_id);
      let op1_id = get_accu_id ind in
      let op2_id = get_stack_id ind 0 in
      let op1 = export_val_desc op1_id in
      let op2 = export_val_desc op2_id in
      putm (
        match binop with
          | Divint | Modint ->
            use_tmp := true;
            let frame_sz = compute_frame_size (ind + 1) in
            let dst =
              if is_read dst_id then Some (export_cell dst_id) else None
            in
            if binop = Divint then DIVINT (op1, op2, dst, frame_sz)
            else MODINT (op1, op2, dst, frame_sz)
          | _ ->
        let dst = export_cell dst_id in
        match binop with
          | Divint | Modint -> assert false
          | Addint        -> ADDINT (op1, op2, dst)
          | Subint        -> SUBINT (op1, op2, dst)
          | Mulint        -> MULINT (op1, op2, dst)
          | Andint        -> ANDINT (op1, op2, dst)
          | Orint         -> ORINT (op1, op2, dst)
          | Xorint        -> XORINT (op1, op2, dst)
          | Lslint        -> LSLINT (op1, op2, dst)
          | Lsrint        -> LSRINT (op1, op2, dst)
          | Asrint        -> ASRINT (op1, op2, dst)
          | Eq            -> EQ (op1, op2, dst)
          | Neq           -> NEQ (op1, op2, dst)
          | Ltint         -> LTINT (op1, op2, dst)
          | Leint         -> LEINT (op1, op2, dst)
          | Gtint         -> GTINT (op1, op2, dst)
          | Geint         -> GEINT (op1, op2, dst)
          | Ultint        -> ULTINT (op1, op2, dst)
          | Ugeint        -> UGEINT (op1, op2, dst)
          | Getvectitem   -> GETVECTITEM (op1, op2, dst)
          | Getstringchar -> GETSTRINGCHAR (op1, op2, dst)
      )
    )
  in
  let export_cond_branch ind cb =
    let accu_id = get_accu_id ind in
    let accu = export_val_desc accu_id in
    putm (
      match cb with
        | Branchif ptr     -> BRANCHIF (accu, ptr.pointed.addr)
        | Branchifnot ptr  -> BRANCHIFNOT (accu, ptr.pointed.addr)
        | Beq (n, ptr)     -> BEQ (n, accu, ptr.pointed.addr)
        | Bneq (n, ptr)    -> BNEQ (n, accu, ptr.pointed.addr)
        | Bltint (n, ptr)  -> BLTINT (n, accu, ptr.pointed.addr)
        | Bleint (n, ptr)  -> BLEINT (n, accu, ptr.pointed.addr)
        | Bgtint (n, ptr)  -> BGTINT (n, accu, ptr.pointed.addr)
        | Bgeint (n, ptr)  -> BGEINT (n, accu, ptr.pointed.addr)
        | Bultint (n, ptr) -> BULTINT (n, accu, ptr.pointed.addr)
        | Bugeint (n, ptr) -> BUGEINT (n, accu, ptr.pointed.addr)
    )
  in
  let export_instr ind instr =
    if List.mem instr.index !catch_list then (
      let dst_id = get_accu_id ind in
      let dst = if is_read dst_id then Some (export_cell dst_id) else None in
      catch_list := List.filter ((<>) instr.index) !catch_list;
      putm (CATCH_EXCEPTION (instr.addr, dst));
    ) else if instr.is_pointed then (
      puti (ILabel instr.addr);
    );
    try match instr.bc with
      | Acc n ->
        export_move (get_stack_id ind n) (get_accu_id (ind + 1));

      | Push ->
        export_move (get_accu_id ind) (get_stack_id (ind + 1) 0);

      | Pop _ ->
        ()

      | Assign n ->
        export_move (get_accu_id ind) (get_stack_id (ind + 1) n);

      | Envacc n ->
        export_gen_acc export_envacc (n - 2) ind;

      | Pushretaddr _ ->
        ()

      | DynamicApply nargs | PartialApply nargs | StaticApply (nargs, _)
      | DynamicAppterm (nargs, _) | PartialAppterm (nargs, _)
      | SpecialAppterm (nargs, _) | StaticAppterm (nargs, _, _) ->
        let get_dst () =
          let dst_id = get_accu_id (ind + 1) in
          if is_read dst_id then (
            assert (is_cell dst_id);
            Some (export_cell dst_id)
          ) else None
        in
        let args =
          let rec f i acc =
            if i = -1 then acc
            else f (i - 1) (export_val_desc (get_stack_id ind i) :: acc)
          in
          f (nargs - 1) []
        in
        let clsr_id = get_accu_id ind in
        let get_clsr () = export_val_desc clsr_id in
        begin match instr.bc with
          | DynamicApply _ ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            let dst = get_dst () in
            let clsr = get_clsr () in
            putm (DYNAMIC_APPLY (nargs, nargs + 1, curr_frame_sz, next_frame_sz,
                                 dst, Some clsr, args));

          | PartialApply _ ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            let dst = get_dst () in
            let clsr = get_clsr () in
            putm (PARTIAL_APPLY (nargs, nargs + 1, curr_frame_sz, next_frame_sz,
                                 dst, Some clsr, args));

          | StaticApply (_, ptr) ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            let useenv =
              Body.test_useenv fun_infos (IMap.find ptr.pointed.index funs)
            in
            let cfun_nargs = if useenv then nargs + 1 else nargs in
            let env = if useenv then Some (get_clsr ()) else None in
            let dst = get_dst () in
            if ISet.mem ptr.pointed.index tc_set then
              putm (STATIC_APPLY (nargs, cfun_nargs, curr_frame_sz,
                                  next_frame_sz, dst, ptr.pointed.index,
                                  env, args))
            else
              putm (STATIC_NOTC_APPLY (nargs, cfun_nargs, curr_frame_sz,
                                       next_frame_sz, dst, ptr.pointed.index,
                                       env, args));

          | DynamicAppterm (nargs, _) ->
            if !Options.trace then puti (ITrace (MLAppterm fun_id));
            let curr_frame_sz = compute_frame_size ind in
            let clsr = get_clsr () in
            if nargs >= cfun_arity && (!Options.arch <> X86_64 || nargs >= 6)
            then
              putm (DYNAMIC_SPECIAL_APPTERM (nargs, nargs + 1, curr_frame_sz,
                                             Some clsr, args))
            else
              putm (DYNAMIC_STANDARD_APPTERM (nargs, nargs + 1, curr_frame_sz,
                                              Some clsr, args));

          | PartialAppterm (nargs, _) ->
            if !Options.trace then puti (ITrace (MLAppterm fun_id));
            let curr_frame_sz = compute_frame_size ind in
            let clsr = get_clsr () in
            if nargs >= cfun_arity then
              putm (PARTIAL_SPECIAL_APPTERM(nargs, nargs + 1, curr_frame_sz,
                                            Some clsr, args))
            else
              putm (PARTIAL_STANDARD_APPTERM (nargs, nargs + 1, curr_frame_sz,
                                              Some clsr, args));

          | StaticAppterm (nargs, _, ptr) ->
            if !Options.trace then puti (ITrace (MLAppterm fun_id));
            let useenv =
              Body.test_useenv fun_infos (IMap.find ptr.pointed.index funs)
            in
            let env = if useenv then Some (get_clsr ()) else None in
            let callee_nargs = if useenv then nargs + 1 else nargs in
            if
              callee_nargs > cfun_arity &&
                (!Options.arch <> X86_64 || callee_nargs > 6)
            then
              putm (STATIC_SPECIAL_APPTERM (nargs, callee_nargs,
                                            ptr.pointed.index, env, args))
            else
              putm (STATIC_STANDARD_APPTERM(nargs, callee_nargs,
                                            ptr.pointed.index, env, args));

          | SpecialAppterm _ ->
            if !Options.trace then puti (ITrace (MLAppterm fun_id));
            let curr_frame_sz = compute_frame_size ind in
            let clsr = get_clsr () in
            putm (SPECIAL_SPECIAL_APPTERM (nargs, nargs+1, curr_frame_sz,
                                           Some clsr, args));

          | _ -> assert false
        end;

      | Return _ ->
        if !Options.trace then puti (ITrace (MLQuit fun_id));
        let accu_id = get_accu_id ind in
        putm (RETURN (export_val_desc accu_id));

      | Closure (env_size, ptr) ->
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          use_tmp := true;
          let ptr_env_usages =
            (IMap.find ptr.pointed.index fun_infos).env_usages
          in
          let new_closure_size =
            let count i b = if b then i else i + 1 in
            2 + env_size - Array.fold_left count 0 ptr_env_usages
          in
          let accu_id = get_accu_id ind in
          let index = ptr.pointed.index in
          let arity = (IMap.find index funs).arity in
          let frame_sz = compute_frame_size ind in
          if env_size > 0 && is_cell accu_id && ptr_env_usages.(0) then
            putm (MAKE_SAVED_YOUNG_BLOCK (
              export_cell accu_id, new_closure_size, closure_tag, LTmp,
              frame_sz))
          else
            putm (MAKE_YOUNG_BLOCK (new_closure_size, closure_tag, LTmp,
                                    frame_sz));
          putm (SET_YOUNG_FIELD (0, ELvalue LTmp,
                                 ECast (TValue, EFunPtr index)));
          putm (SET_YOUNG_FIELD (1, ELvalue LTmp, export_constint arity));
          let env_pos = ref 2 in
          if env_size > 0 then
            if ptr_env_usages.(0) then (
              putm (SET_YOUNG_FIELD (!env_pos, ELvalue LTmp,
                                     export_val_desc accu_id));
              incr env_pos;
            );
          for i = 1 to env_size - 1 do
            if ptr_env_usages.(i) then (
              let stk_id = get_stack_id ind (i - 1) in
              putm (SET_YOUNG_FIELD (!env_pos, ELvalue LTmp,
                                     export_val_desc stk_id));
              incr env_pos;
            );
          done;
          export_gen_acc (fun x -> x) (ELvalue LTmp) ind;
        );

      | Closurerec (fun_nb, env_size, ptr, ptrs) ->
        let dst_id = get_accu_id (ind + 1) in
        let closure_used =
          let rec f i =
            if i = fun_nb then false
            else if is_read (get_stack_id (ind + 1) i) then true
            else f (i + 1)
          in
          is_read dst_id || f 0
        in
        if closure_used then (
          use_tmp := true;
          let index = ptr.pointed.index in
          let ptr_env_usages = (IMap.find index fun_infos).env_usages in
          let new_closure_size =
            let count i b = if b then i else i + 1 in
            3 * fun_nb + env_size - 1 - Array.fold_left count 0 ptr_env_usages
          in
          let accu_id = get_accu_id ind in
          let arity = (IMap.find index funs).arity in
          let frame_sz = compute_frame_size ind in
          if env_size > 0 && is_cell accu_id && ptr_env_usages.(0) then
            putm (MAKE_SAVED_YOUNG_BLOCK (export_cell accu_id,
                                          new_closure_size,
                                          closure_tag, LTmp, frame_sz))
          else
            putm (MAKE_YOUNG_BLOCK (new_closure_size, closure_tag, LTmp,
                                    frame_sz));
          putm (SET_YOUNG_FIELD (0, ELvalue LTmp,
                                 ECast (TValue, EFunPtr index)));
          putm (SET_YOUNG_FIELD (1, ELvalue LTmp, export_constint arity));
          for i = 0 to Array.length ptrs - 1 do
            let index = ptrs.(i).pointed.index in
            let arity = (IMap.find index funs).arity in
            putm (SET_YOUNG_FIELD (3 * i + 2, ELvalue LTmp,
                                   EMakeHeader (3 * (i + 1), TInfix_tag,
                                                CCaml_white)));
            putm (SET_YOUNG_FIELD (3 * i + 3, ELvalue LTmp,
                                   ECast (TValue, EFunPtr index)));
            putm (SET_YOUNG_FIELD (3 * i + 4, ELvalue LTmp,
                                   export_constint arity));
          done;
          let env_pos = ref (3 * fun_nb - 1) in
          if env_size > 0 then
            if ptr_env_usages.(0) then (
              putm (SET_YOUNG_FIELD (!env_pos, ELvalue LTmp,
                                     export_val_desc accu_id));
              incr env_pos;
            );
          for i = 1 to env_size - 1 do
            if ptr_env_usages.(i) then (
              let stk_id = get_stack_id ind (i - 1) in
              putm (SET_YOUNG_FIELD (!env_pos, ELvalue LTmp,
                                     export_val_desc stk_id));
              incr env_pos;
            );
          done;
          for i = 0 to fun_nb - 1 do
            let stk_id = get_stack_id (ind + 1) (fun_nb - i - 1) in
            if is_read stk_id then (
              assert (is_cell stk_id);
              putm (MOVE (EOffset (ELvalue LTmp, 3 * i), export_cell stk_id));
            );
          done;
          export_gen_acc (fun x -> x) (ELvalue LTmp) ind;
        );

      | Offsetclosure ofs ->
        export_gen_acc export_clsr ofs ind

      | Getglobal n ->
        export_gen_acc export_glob n ind

      | Getglobalfield (n, p) ->
        export_gen_acc export_glob_field (n, p) ind;

      | Setglobal n ->
        let src_id = get_accu_id ind in
        putm (SETGLOBAL (n, export_val_desc src_id));
        export_gen_acc export_constint 0 ind;

      | Atom tag ->
        export_gen_acc export_atom tag ind;

      | Makeblock (sz, tag) ->
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          use_tmp := true;
          let frame_sz = compute_frame_size ind in
          let accu_id = get_accu_id ind in
          let is_young = sz <= max_young_wosize in
          let is_saved = is_cell accu_id in
          begin match (is_young, is_saved) with
            | (true, true) ->
              let accu = export_cell accu_id in
              putm (MAKE_SAVED_YOUNG_BLOCK (accu, sz, tag, LTmp, frame_sz));
            | (false, true) ->
              let accu = export_cell accu_id in
              putm (MAKE_SAVED_BLOCK (accu, sz, tag, LTmp, frame_sz));
            | (true, false) ->
              putm (MAKE_YOUNG_BLOCK (sz, tag, LTmp, frame_sz));
            | (false, false) ->
              putm (MAKE_BLOCK (sz, tag, LTmp, frame_sz));
          end;
          let export_field_init i id =
            let val_ = export_val_desc id in
            if is_young then putm (SET_YOUNG_FIELD (i, ELvalue LTmp, val_))
            else putm (INITFIELD (i, ELvalue LTmp, val_));
          in
          export_field_init 0 accu_id;
          for i = 1 to sz - 1 do
            export_field_init i (get_stack_id ind (i - 1));
          done;
          export_gen_acc (fun x -> x) (ELvalue LTmp) ind;
        );

      | Makefloatblock sz ->
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          use_tmp := true;
          let frame_sz = compute_frame_size ind in
          let accu_id = get_accu_id ind in
          let is_young = sz <= max_young_wosize / double_wosize in
          let is_saved = is_cell accu_id in
          begin match (is_young, is_saved) with
            | (true, true) ->
              let accu = export_cell accu_id in
              putm (MAKE_SAVED_YOUNG_FLOAT_BLOCK (accu, sz, LTmp, frame_sz));
            | (false, true) ->
              let accu = export_cell accu_id in
              putm (MAKE_SAVED_FLOAT_BLOCK (accu, sz, LTmp, frame_sz));
            | (true, false) ->
              putm (MAKE_YOUNG_FLOAT_BLOCK (sz, LTmp, frame_sz));
            | (false, false) ->
              putm (MAKE_FLOAT_BLOCK (sz, LTmp, frame_sz));
          end;
          let export_field_init i id =
            let val_ = export_val_desc id in
            putm (SETFLOATFIELD (i, ELvalue LTmp, val_));
          in
          export_field_init 0 accu_id;
          for i = 1 to sz - 1 do
            export_field_init i (get_stack_id ind (i - 1));
          done;
          export_gen_acc (fun x -> x) (ELvalue LTmp) ind;
        );

      | Getfield n ->
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          let dst = export_cell dst_id in
          let src_id = get_accu_id ind in
          let src = export_val_desc src_id in
          putm (GETFIELD (n, src, dst));
        );

      | Getfloatfield n ->
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          use_tmp := true;
          let dst = export_cell dst_id in
          let src_id = get_accu_id ind in
          let src = export_val_desc src_id in
          let frame_sz = compute_frame_size (ind + 1) in
          putm (GETFLOATFIELD (n, src, dst, frame_sz));
        );

      | Setfield n ->
        let blk_id = get_accu_id ind in
        let val_id = get_stack_id ind 0 in
        let blk = export_val_desc blk_id in
        let val_ = export_val_desc val_id in
        putm (SETFIELD (n, blk, val_));
        export_gen_acc export_constint 0 ind;

      | Setfloatfield n ->
        let blk_id = get_accu_id ind in
        let val_id = get_stack_id ind 0 in
        let blk = export_val_desc blk_id in
        let val_ = export_val_desc val_id in
        putm (SETFLOATFIELD (n, blk, val_));
        export_gen_acc export_constint 0 ind;

      | Checksignals ->
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_cell accu_id && is_read accu_id then
          putm (SAVED_CHECK_SIGNALS (export_cell accu_id, frame_sz))
        else
          putm (CHECK_SIGNALS frame_sz);

      | Ccall (n, p) ->
        use_tmp := true;
        if !Options.trace then puti (ITrace (CEnter prims.(p)));
        let frame_sz = compute_frame_size (ind + 1) in
        let accu_id = get_accu_id ind in
        let accu = export_val_desc accu_id in
        let dst_id = get_accu_id (ind + 1) in
        let dst = match is_read dst_id with
          | true -> assert (is_cell dst_id); Some (export_cell dst_id)
          | false -> None
        in
        let args =
          let rec f i acc =
            if i = -1 then acc
            else f (i - 1) (export_val_desc (get_stack_id ind i) :: acc)
          in
          accu :: f (n - 2) []
        in
        if n <= 5 then putm (CCALL (dst, prims.(p), frame_sz, args))
        else putm (BIG_CCALL (n, dst, prims.(p), frame_sz, args));
        if !Options.trace then puti (ITrace (CQuit prims.(p)));

      | Pushtrap ptr ->
        let dst_id = get_accu_id ptr.pointed.index in
        let dst = match is_read dst_id with
          | true -> Some (export_cell dst_id)
          | false -> None
        in
        let ukid = Id.create () in
        putm (PUSHTRAP (dst, ptr.pointed.addr, ukid));
        catch_list := ptr.pointed.index :: !catch_list;

      | Raise ->
        if !Options.trace then puti (ITrace MLRaise);
        let exn_id = get_accu_id ind in
        let exn = export_val_desc exn_id in
        putm (RAISE exn);

      | Poptrap ->
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_cell accu_id && is_read accu_id then
          putm (SAVED_POPTRAP (export_cell accu_id, frame_sz))
        else
          putm (POPTRAP frame_sz);

      | Const n ->
        export_gen_acc export_constint n ind;

      | Unapp unop ->
        export_unapp ind unop;

      | Offsetref ofs ->
        let ref_id = get_accu_id ind in
        let ref_ = export_val_desc ref_id in
        putm (OFFSETREF (ofs, ref_));
        export_gen_acc export_constint 0 ind;

      | Binapp binop ->
        export_binapp ind binop;

      | Setvectitem ->
        let indx_id = get_stack_id ind 0 in
        let blk_id = get_accu_id ind in
        let val_id = get_stack_id ind 1 in
        let indx = export_val_desc indx_id in
        let blk = export_val_desc blk_id in
        let val_ = export_val_desc val_id in
        putm (SETVECTITEM (indx, blk, val_));
        export_gen_acc export_constint 0 ind;

      | Setstringchar ->
        let indx_id = get_stack_id ind 0 in
        let str_id = get_accu_id ind in
        let char_id = get_stack_id ind 1 in
        let indx = export_val_desc indx_id in
        let str = export_val_desc str_id in
        let char = export_val_desc char_id in
        putm (SETSTRINGCHAR (indx, str, char));
        export_gen_acc export_constint 0 ind;

      | Branch ptr ->
        putm (BRANCH ptr.pointed.addr);

      | CondBranch cb ->
        export_cond_branch ind cb;

      | Switch (size_long, size_tag, tbl) ->
        assert (Array.length tbl = size_long + size_tag);
        let src_id = get_accu_id ind in
        let addrs = Array.map (fun ptr -> ptr.pointed.addr) tbl in
        puti (ISwitch {
          sw_src = export_val_desc src_id;
          sw_long_labels = Array.sub addrs 0 size_long;
          sw_tag_labels = Array.sub addrs size_long size_tag;
        });

      | Getmethod ->
        let meth_id = get_accu_id (ind + 1) in
        if is_read meth_id then (
          assert (is_cell meth_id);
          let tag_id = get_accu_id ind in
          let obj_id = get_stack_id ind 0 in
          let tag = export_val_desc tag_id in
          let obj = export_val_desc obj_id in
          let meth = export_cell meth_id in
          putm (GETMETHOD (tag, obj, meth));
        );

      | Getpubmet (tag, _) ->
        let obj_id = get_accu_id ind in
        let meth_id = get_accu_id (ind + 1) in
        let odst_id = get_stack_id (ind + 1) 0 in
        export_move obj_id odst_id;
        if is_read meth_id then (
          assert (is_cell meth_id);
          let obj = export_val_desc obj_id in
          let meth = export_cell meth_id in
          putm (GETPUBMET (tag, obj, meth));
        );

      | Getdynmet ->
        let meth_id = get_accu_id (ind + 1) in
        if is_read meth_id then (
          assert (is_cell meth_id);
          let tag_id = get_accu_id ind in
          let obj_id = get_stack_id ind 0 in
          let tag = export_val_desc tag_id in
          let obj = export_val_desc obj_id in
          let meth = export_cell meth_id in
          putm (GETDYNMET (tag, obj, meth));
        );

      | Stop ->
        putm STOP;

      | Restart ->
        assert false

      | Grab _ ->
        assert false

    with Dead_code -> ()
  in
  let location = compute_location funs dbug fun_id in
  let body =
    if !Options.trace && fun_id <> 0 then puti (ITrace (MLEnter fun_id));
    compute_fun_init puti use_env fun_desc.arity arg_depths read_args;
    Array.iteri export_instr body;
    assert (!catch_list = []);
    List.rev !instrs
  in
  let locals =
    compute_fun_locals fun_desc.arity var_nb !use_tmp arg_depths read_args
  in
  compute_fun_def fun_desc use_env locals body location
;;

let gen_macroc prims data dbug funs fun_infos ids_infos tc_set =
  Options.verb_start "+ Generating C code.";
  let main = IMap.find 0 ids_infos in
  let rest = IMap.remove 0 ids_infos in
  let fold_fun_decl _ ids_info acc =
    let use_env = Body.test_useenv fun_infos ids_info.fun_desc in
    let inlinable = Body.test_inlinable funs fun_infos ids_info.fun_desc in
    let location = compute_location funs dbug ids_info.fun_desc.fun_id in
    let fun_decl =
      compute_fun_decl ids_info.fun_desc use_env inlinable location
    in
    fun_decl :: acc
  in
  let fold_fun_def fun_id ids_info acc =
    let fun_def =
      compute_fun prims dbug funs fun_infos tc_set fun_id ids_info
    in
    fun_def :: acc
  in
  let max_arity = Body.compute_maximum_arity funs in
  let fun_decls = List.rev (IMap.fold fold_fun_decl rest []) in
  Options.message ".";
  let main_def = compute_fun prims dbug funs fun_infos tc_set 0 main in
  Options.message ".";
  let fun_defs = List.rev (main_def :: IMap.fold fold_fun_def rest []) in
  Options.verb_stop ();
  {
    mc_data      = data;
    mc_fun_decls = fun_decls;
    mc_fun_defs  = fun_defs;
    mc_max_arity = max_arity;
  }
;;
