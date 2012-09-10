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

open Printf;;
open Types;;
open Tools;;

exception Dead_code;;

let max_young_wosize = 256;;
let double_wosize = 2;;
let closure_tag = Obj.closure_tag;;

let export_fun_signature oc id use_env arity =
  if id = 0 then (
    fprintf oc "void ocamlcc_bytecode_main(void)";
  ) else (
    match !Options.arch with
      | NO_ARCH ->
        fprintf oc "value f%d(value p0)" id;
      | _ ->
        fprintf oc "value f%d(value p0" id;
        for i = 1 to arity - 1 do fprintf oc ", value p%d" i; done;
        if use_env then fprintf oc ", value env)"
        else fprintf oc ")";
  );
;;

let export_fun_decl_signature oc id use_env inlinable arity =
  export_fun_signature oc id use_env arity;
  if not inlinable then fprintf oc " %s" Applygen.attribute;
  fprintf oc ";\n"
;;

let export_fun_def_signature oc id use_env arity =
  export_fun_signature oc id use_env arity;
  fprintf oc " {\n";
;;

let export_fun_declarations oc arity var_nb use_tmp arg_depths =
  let cnt = ref 0 in
  let print c id =
    incr cnt;
    if !cnt = 1 then fprintf oc "  value %c%d" c id
    else if !cnt = 8 then (cnt := 0 ; fprintf oc ", %c%d;\n" c id)
    else fprintf oc ", %c%d" c id
  in
  begin match !Options.arch with
    | NO_ARCH ->
      for i = 1 to arity - 1 do
        if not (IMap.mem i arg_depths) then print 'p' i;
      done;
      if !cnt <> 0 then (
        cnt := 0;
        fprintf oc ";\n";
      );
    | _ -> ()
  end;
  for i = 0 to var_nb - 1 do print 'v' i done;
  if !cnt <> 0 then fprintf oc ";\n";
  if use_tmp then fprintf oc "  value tmp;\n";
  fprintf oc "  value *sp;\n";
;;

let export_fun_init oc use_env arity arg_depths =
  fprintf oc "  sp = caml_extern_sp;\n";
  match !Options.arch with
    | NO_ARCH ->
      if use_env then fprintf oc "  sp[-1] = ocamlcc_global_env;\n";
      for i = 0 to arity - 1 do
        try
          let ofs = IMap.find i arg_depths in
          if i = 0 then
            fprintf oc "  sp[%d] = p0;\n" ofs
          else
            fprintf oc "  sp[%d] = ocamlcc_global_params[%d];\n" ofs (i - 1);
        with Not_found ->
          if i <> 0 then
            fprintf oc "  p%d = ocamlcc_global_params[%d];\n" i (i - 1);
      done;
    | _ ->
      if use_env then fprintf oc "  sp[-1] = env;\n";
      for i = 0 to arity - 1 do
        try
          let ofs = IMap.find i arg_depths in
          fprintf oc "  sp[%d] = p%d;\n" ofs i;
        with Not_found -> ()
      done;
;;

let export_fun_foot oc =
  fprintf oc "}\n\n";
;;

let export_fun oc prims dbug funs fun_id
    (fun_desc, states, idvd_map, ptr_set, read_set) =
  let body = fun_desc.body in
  let instr_nb = Array.length body in
  let catch_list = ref [] in
  let use_env = Block.test_useenv fun_desc in
  let cfun_arity = if use_env then fun_desc.arity + 1 else fun_desc.arity in
  let is_read id = ISet.mem id read_set in
  let is_ptr id = ISet.mem id ptr_set in
  let is_cell id = IMap.find id idvd_map = VCell in
  let is_arg id =
    match IMap.find id idvd_map with
      | VArg _ -> true
      | _ -> false
  in
  let use_tmp =
    let rec f i =
      if i = instr_nb then false else
        match body.(i).bc with
          | Closure _ | Closurerec _ | Makeblock _ | Makefloatblock _ -> true
          | _ -> f (i + 1)
    in
    f 0
  in
  let (args_ofs, arg_depths) =
    let f (n, depth, map) id =
      if is_ptr id then (n + 1, depth - 1, IMap.add n depth map)
      else (n + 1, depth, map)
    in
    match states.(0) with
      | None -> assert false
      | Some state ->
        let init_pdepth = if use_env then -2 else -1 in
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
      else if is_ptr id then (vmap, add id pdepth pmap, vlevel, pdepth - 1)
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
    let init_size = -args_ofs - 1 in
    Stk.fold_left f init_size (get_state ind).stack
  in
  let export_const oc n = fprintf oc "Val_int(%d)" n in
  let export_glob oc n = fprintf oc "Glob(%d)" n in
  let export_glob_field oc (n, p) = fprintf oc "GlobField(%d, %d)" n p in
  let export_atom oc tag = fprintf oc "Atom(%d)" tag in
  let export_clsr oc ofs = fprintf oc "Offset(sp[-1], %d)" (ofs * 3 / 2) in
  let export_envacc oc n = fprintf oc "Field(sp[-1], %d)" (n + 2) in
  let export_arg oc n =
    try fprintf oc "sp[%d]" (IMap.find n arg_depths)
    with Not_found -> fprintf oc "p%d" n
  in
  let export_cell oc id =
    if is_ptr id && IMap.mem id ptr_depths then
      fprintf oc "sp[%d]" (IMap.find id ptr_depths)
    else
      fprintf oc "v%d" (IMap.find id var_levels)
  in
  let export_val_desc oc id =
    match IMap.find id idvd_map with
      | VConst n -> export_const oc n
      | VGlob n -> export_glob oc n
      | VGlobField (n, p) -> export_glob_field oc (n, p)
      | VAtom tag -> export_atom oc tag
      | VClsr ofs -> export_clsr oc ofs
      | VEnv n -> export_envacc oc n
      | VArg n -> export_arg oc n
      | VPtr _ -> assert false
      | VCell -> export_cell oc id
  in
  let export_move src_id dst_id =
    if is_read dst_id then (
      assert (is_cell dst_id);
      fprintf oc "  MOVE(%a, %a);\n" export_val_desc src_id export_cell dst_id;
    );
  in
  let export_gen_move src_pp src ind =
    let dst_id = get_accu_id (ind + 1) in
    if is_read dst_id then (
      assert (is_cell dst_id);
      fprintf oc "  MOVE(%a, %a);\n" src_pp src export_cell dst_id;
    );
  in
  let export_unapp ind unop =
    let macro =
      match unop with
        | Boolnot     -> "BOOLNOT"
        | Offsetint _ -> "OFFSETINT"
        | Negint      -> "NEGINT"
        | Isint       -> "ISINT"
        | Vectlength  -> "VECTLENGTH"
    in
    let src_id = get_accu_id ind in
    let dst_id = get_accu_id (ind + 1) in
    if is_read dst_id then (
      assert (is_cell dst_id);
      match unop with
        | Offsetint n ->
          fprintf oc "  %s(%d, %a, %a);\n" macro n export_val_desc src_id
            export_cell dst_id
        | _ ->
          fprintf oc "  %s(%a, %a);\n" macro export_val_desc src_id
            export_cell dst_id
    );
  in
  let export_binapp ind binop =
    let macro =
      match binop with
        | Addint        -> "ADDINT"
        | Subint        -> "SUBINT"
        | Mulint        -> "MULINT"
        | Divint        -> "DIVINT"
        | Modint        -> "MODINT"
        | Andint        -> "ANDINT"
        | Orint         -> "ORINT"
        | Xorint        -> "XORINT"
        | Lslint        -> "LSLINT"
        | Lsrint        -> "LSRINT"
        | Asrint        -> "ASRINT"
        | Eq            -> "EQ"
        | Neq           -> "NEQ"
        | Ltint         -> "LTINT"
        | Leint         -> "LEINT"
        | Gtint         -> "GTINT"
        | Geint         -> "GEINT"
        | Ultint        -> "ULTINT"
        | Ugeint        -> "UGEINT"
        | Getvectitem   -> "GETVECTITEM"
        | Getstringchar -> "GETSTRINGCHAR"
    in
    let op1_id = get_accu_id ind in
    let op2_id = get_stack_id ind 0 in
    let dst_id = get_accu_id (ind + 1) in
    if is_read dst_id then (
      assert (is_cell dst_id);
      fprintf oc "  %s(%a, %a, %a" macro export_val_desc op1_id
        export_val_desc op2_id export_cell dst_id;
      match binop with
        | Divint | Modint ->
          fprintf oc ", %d);\n" (compute_frame_size (ind + 1));
        | _ ->
          fprintf oc ");\n";
    );
  in
  let export_cond_branch ind cb =
    let macro =
      match cb with
        | Branchif _    -> "BRANCHIF"
        | Branchifnot _ -> "BRANCHIFNOT"
        | Beq _         -> "BEQ"
        | Bneq _        -> "BNEQ"
        | Bltint _      -> "BLTINT"
        | Bleint _      -> "BLEINT"
        | Bgtint _      -> "BGTINT"
        | Bgeint _      -> "BGEINT"
        | Bultint _     -> "BULTINT"
        | Bugeint _     -> "BUGEINT"
    in
    let accu_id = get_accu_id ind in
    match cb with
      | Branchif ptr | Branchifnot ptr ->
        fprintf oc "  %s(%a, L%05X);\n" macro export_val_desc accu_id
          ptr.pointed.addr;
      | Beq (n, ptr) | Bneq (n, ptr) | Bltint (n, ptr) | Bleint (n, ptr)
      | Bgtint (n, ptr) | Bgeint (n, ptr) | Bultint (n, ptr)
      | Bugeint (n, ptr) ->
        fprintf oc "  %s(%d, %a, L%05X);\n" macro n export_val_desc accu_id
          ptr.pointed.addr;
  in
  let export_instr ind instr =
    if List.mem instr.index !catch_list then (
      let dst_id = get_accu_id ind in
      catch_list := List.filter ((<>) instr.index) !catch_list;
      if is_read dst_id then
        fprintf oc "  CATCH_EXCEPTION(L%05X, %a = exn);\n"
          instr.addr export_cell dst_id
      else
        fprintf oc "  CATCH_EXCEPTION(L%05X, (void) 0);\n" instr.addr
    ) else if instr.is_pointed then (
      fprintf oc " L%05X:\n" instr.addr;
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
        export_gen_move export_envacc (n - 2) ind;
      | Pushretaddr _ ->
        ()
      | DynamicApply nargs | PartialApply nargs | StaticApply (nargs, _)
      | DynamicAppterm (nargs, _) | PartialAppterm (nargs, _)
      | SpecialAppterm (nargs, _) | StaticAppterm (nargs, _, _) ->
        let print_dst () =
          let dst_id = get_accu_id (ind + 1) in
          if is_read dst_id then (
            assert (is_cell dst_id);
            fprintf oc ", %a =" export_cell dst_id;
          ) else (
            fprintf oc ", ";
          );
        in
        let clsr_id = get_accu_id ind in
        let print_env = ref true in
        begin match instr.bc with
          | DynamicApply _ ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            fprintf oc "  DYNAMIC_APPLY(%d, %d, %d" nargs
              curr_frame_sz next_frame_sz;
            print_dst ();
          | PartialApply _ ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            fprintf oc "  PARTIAL_APPLY(%d, %d, %d" nargs
              curr_frame_sz next_frame_sz;
            print_dst ();
          | StaticApply (_, ptr) ->
            let curr_frame_sz = compute_frame_size ind in
            let next_frame_sz = compute_frame_size (ind + 1) in
            fprintf oc "  STATIC_APPLY(%d, %d, %d" nargs
              curr_frame_sz next_frame_sz;
            print_dst ();
            fprintf oc ", f%d" ptr.pointed.index;
            print_env :=
              Block.test_useenv (IMap.find ptr.pointed.index funs);
          | DynamicAppterm (nargs, _) ->
            if !Options.trace then
              fprintf oc "  printf(\"<-> f%d\\n\");\n" fun_id;
            let curr_frame_sz = compute_frame_size ind in
            if
              nargs >= cfun_arity &&
                (!Options.arch <> X86_64 || nargs >= 6)
            then
              fprintf oc "  DYNAMIC_SPECIAL_APPTERM(%d, %d, %d" nargs
                (nargs + 1) curr_frame_sz
            else
              fprintf oc "  DYNAMIC_STANDARD_APPTERM(%d, %d" nargs
                curr_frame_sz;
          | PartialAppterm (nargs, _) ->
            if !Options.trace then
              fprintf oc "  printf(\"<-> f%d\\n\");\n" fun_id;
            let curr_frame_sz = compute_frame_size ind in
            if nargs >= cfun_arity then
              fprintf oc "  PARTIAL_SPECIAL_APPTERM(%d, %d, %d" nargs
                (nargs + 1) curr_frame_sz
            else
              fprintf oc "  PARTIAL_STANDARD_APPTERM(%d, %d" nargs
                curr_frame_sz;
          | SpecialAppterm _ ->
            if !Options.trace then
              fprintf oc "  printf(\"<-> f%d\\n\");\n" fun_id;
            let curr_frame_sz = compute_frame_size ind in
            fprintf oc "  SPECIAL_SPECIAL_APPTERM(%d, %d, %d" nargs
              (nargs + 1) curr_frame_sz
          | StaticAppterm (nargs, _, ptr) ->
            print_env :=
              Block.test_useenv (IMap.find ptr.pointed.index funs);
            let callee_nargs = if !print_env then nargs + 1 else nargs in
            if !Options.trace then
              fprintf oc "  printf(\"<-> f%d\\n\");\n" fun_id;
            if
              callee_nargs > cfun_arity &&
                (!Options.arch <> X86_64 || callee_nargs > 6)
            then
              fprintf oc "  STATIC_SPECIAL_APPTERM(%d, %d, " nargs callee_nargs
            else
              fprintf oc "  STATIC_STANDARD_APPTERM(%d, " nargs;
            fprintf oc "f%d" ptr.pointed.index;
          | _ -> assert false
        end;
        if !Options.arch = NO_ARCH then
          if !print_env then
            fprintf oc ", %a" export_val_desc clsr_id
          else
            fprintf oc ", Val_unit";
        for i = 0 to nargs - 1 do
          fprintf oc ", %a" export_val_desc (get_stack_id ind i);
        done;
        if !print_env && !Options.arch <> NO_ARCH then
          fprintf oc ", %a" export_val_desc clsr_id;
        fprintf oc ");\n";
      | Return _ ->
        if !Options.trace then
          fprintf oc "  printf(\"<-- f%d\\n\");\n" fun_id;
        let accu_id = get_accu_id ind in
        fprintf oc "  RETURN(%a);\n" export_val_desc accu_id;
      | Closure (n, ptr) ->
        let index = ptr.pointed.index in
        let arity = (IMap.find index funs).arity in
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size ind in
        if is_read dst_id then (
          assert (is_cell dst_id);
          if n > 0 && is_cell (get_accu_id ind) then
            fprintf oc "  MAKE_SAVED_YOUNG_BLOCK(%a, " export_cell
              (get_accu_id ind)
          else
            fprintf oc "  MAKE_YOUNG_BLOCK(";
          fprintf oc "%d, %d, tmp, %d);\n" (2 + n) closure_tag frame_sz;
          fprintf oc "  SET_YOUNG_FIELD(0, tmp, (value) &f%d);\n" index;
          fprintf oc "  SET_YOUNG_FIELD(1, tmp, %a);\n" export_const arity;
          if n > 0 then
            fprintf oc "  SET_YOUNG_FIELD(2, tmp, %a);\n" export_val_desc
              (get_accu_id ind);
          for i = 1 to n - 1 do
            fprintf oc "  SET_YOUNG_FIELD(%d, tmp, %a);\n" (i + 2)
              export_val_desc (get_stack_id ind (i - 1));
          done;
          fprintf oc "  MOVE(tmp, %a);\n" export_cell dst_id;
        );
      | Closurerec (f, v, o, t) ->
        let arity = (IMap.find o.pointed.index funs).arity in
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size ind in
        assert (is_cell dst_id);
        if v > 0 && is_cell (get_accu_id ind) then
          fprintf oc "  MAKE_SAVED_YOUNG_BLOCK(%a, " export_cell
            (get_accu_id ind)
        else
          fprintf oc "  MAKE_YOUNG_BLOCK(";
        fprintf oc "%d, %d, tmp, %d);\n" (3 * f + v - 1) closure_tag
          frame_sz;
        fprintf oc "  SET_YOUNG_FIELD(0, tmp, (value) &f%d);\n"
          o.pointed.index;
        fprintf oc "  SET_YOUNG_FIELD(1, tmp, %a);\n" export_const arity;
        for i = 0 to Array.length t - 1 do
          let index = t.(i).pointed.index in
          let arity = (IMap.find index funs).arity in
          fprintf oc
            "  SET_YOUNG_FIELD(%d, tmp, Make_header(%d, Infix_tag, Caml_white));\n"
            (3 * i + 2) (3 * (i + 1));
          fprintf oc "  SET_YOUNG_FIELD(%d, tmp, (value) &f%d);\n"
            (3 * i + 3) index;
          fprintf oc "  SET_YOUNG_FIELD(%d, tmp, %a);\n" (3 * i + 4)
            export_const arity;
        done;
        if v > 0 then
          fprintf oc "  SET_YOUNG_FIELD(%d, tmp, %a);\n" (3 * f - 1)
            export_val_desc (get_accu_id ind);
        for i = 1 to v - 1 do
          fprintf oc "  SET_YOUNG_FIELD(%d, tmp, %a);\n" (3 * f - 1 + i)
            export_val_desc (get_stack_id ind (i - 1));
        done;
        for i = 0 to f - 1 do
          let stk_id = get_stack_id (ind + 1) (f - i - 1) in
          if is_read stk_id then (
            assert (is_cell stk_id);
            fprintf oc "  MOVE(Offset(tmp, %d), %a);\n"
              (3 * i) export_cell stk_id;
          );
        done;
        fprintf oc "  MOVE(tmp, %a);\n" export_cell dst_id;
      | Offsetclosure ofs ->
        export_gen_move export_clsr ofs ind
      | Getglobal n ->
        export_gen_move export_glob n ind
      | Getglobalfield (n, p) ->
        export_gen_move export_glob_field (n, p) ind;
      | Setglobal n ->
        let src_id = get_accu_id ind in
        fprintf oc "  SETGLOBAL(%d, %a);\n" n export_val_desc src_id;
        export_gen_move export_const 0 ind;
      | Atom tag ->
        export_gen_move export_atom tag ind;
      | Makeblock (sz, tag) ->
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_read dst_id then (
          let (saved, mkblk, stfld) =
            match (sz <= max_young_wosize, is_cell accu_id) with
              | (true, true) ->
                (true, "MAKE_SAVED_YOUNG_BLOCK", "SET_YOUNG_FIELD")
              | (false, true) ->
                (true, "MAKE_SAVED_BLOCK", "SETFIELD")
              | (true, false) ->
                (false, "MAKE_YOUNG_BLOCK", "SET_YOUNG_FIELD")
              | (false, false) ->
                (false, "MAKE_BLOCK", "SETFIELD")
          in
          assert (is_cell dst_id);
          fprintf oc "  %s(" mkblk;
          if saved then fprintf oc "%a, " export_val_desc accu_id;
          fprintf oc "%d, %d, tmp, %d);\n" sz tag frame_sz;
          fprintf oc "  %s(0, tmp, %a);\n" stfld export_val_desc accu_id;
          for i = 1 to sz - 1 do
            fprintf oc "  %s(%d, tmp, %a);\n" stfld i export_val_desc
              (get_stack_id ind (i - 1));
          done;
          fprintf oc "  MOVE(tmp, %a);\n" export_cell dst_id;
        );
      | Makefloatblock sz ->
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_read dst_id then (
          let (saved, mkblk, stfld) =
            match
              (sz <= max_young_wosize / double_wosize, is_cell accu_id)
            with
              | (true, true) ->
                (true, "MAKE_SAVED_YOUNG_FLOAT_BLOCK", "SETFLOATFIELD")
              | (false, true) ->
                (true, "MAKE_SAVED_FLOAT_BLOCK", "SETFLOATFIELD")
              | (true, false) ->
                (false, "MAKE_YOUNG_FLOAT_BLOCK", "SETFLOATFIELD")
              | (false, false) ->
                (false, "MAKE_FLOAT_BLOCK", "SETFLOATFIELD")
          in
          assert (is_cell dst_id);
          fprintf oc "  %s(" mkblk;
          if saved then fprintf oc "%a, " export_cell accu_id;
          fprintf oc "%d, tmp, %d);\n" sz frame_sz;
          fprintf oc "  %s(0, tmp, %a);\n" stfld export_val_desc accu_id;
          for i = 1 to sz - 1 do
            fprintf oc "  %s(%d, tmp, %a);\n" stfld i export_val_desc
              (get_stack_id ind (i - 1));
          done;
          fprintf oc "  MOVE(tmp, %a);\n" export_cell dst_id;
        );
      | Getfield n ->
        let src_id = get_accu_id ind in
        let dst_id = get_accu_id (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          fprintf oc "  GETFIELD(%d, %a, %a);\n" n export_val_desc src_id
            export_cell dst_id
        );
      | Getfloatfield n ->
        let src_id = get_accu_id ind in
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size (ind + 1) in
        if is_read dst_id then (
          assert (is_cell dst_id);
          fprintf oc "  GETFLOATFIELD(%d, %a, %a, %d);\n" n export_val_desc
            src_id export_cell dst_id frame_sz;
        );
      | Setfield n ->
        let blk_id = get_accu_id ind in
        let val_id = get_stack_id ind 0 in
        fprintf oc "  SETFIELD(%d, %a, %a);\n" n export_val_desc blk_id
          export_val_desc val_id;
        export_gen_move export_const 0 ind;
      | Setfloatfield n ->
        let blk_id = get_accu_id ind in
        let val_id = get_stack_id ind 0 in
        fprintf oc "  SETFLOATFIELD(%d, %a, %a);\n" n export_val_desc blk_id
          export_val_desc val_id;
        export_gen_move export_const 0 ind;
      | Checksignals ->
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_cell accu_id && is_read accu_id then
          fprintf oc "  SAVED_CHECK_SIGNALS(%a, %d);\n" export_cell accu_id
            frame_sz
        else
          fprintf oc "  CHECK_SIGNALS(%d);\n" frame_sz;
      | Ccall (n, p) ->
        if !Options.trace then
          fprintf oc "  printf(\"--> %s\\n\");\n" prims.(p);
        let dst_id = get_accu_id (ind + 1) in
        let frame_sz = compute_frame_size (ind + 1) in
        if n <= 5 then fprintf oc "  CCALL("
        else fprintf oc "  BIG_CCALL(%d, " n;
        if is_read dst_id then (
          assert (is_cell dst_id);
          fprintf oc "%a =, " export_cell dst_id;
        ) else (
          fprintf oc ", ";
        );
        fprintf oc "%s, %d, %a" prims.(p) frame_sz export_val_desc
          (get_accu_id ind);
        for i = 0 to n - 2 do
          fprintf oc ", %a" export_val_desc (get_stack_id ind i);
        done;
        fprintf oc ");\n";
        if !Options.trace then
          fprintf oc "  printf(\"<-- %s\\n\");\n" prims.(p);
      | Pushtrap ptr ->
        let dst_id = get_accu_id ptr.pointed.index in
        let ukid = Id.create () in
        fprintf oc "  PUSHTRAP(";
        if is_read dst_id then (
          fprintf oc "%a = caml_exn_bucket"  export_cell dst_id;
        );
        fprintf oc ", L%05X, %d);\n" ptr.pointed.addr ukid;
        catch_list := ptr.pointed.index :: !catch_list;
      | Raise ->
        if !Options.trace then fprintf oc "  printf(\"<< RAISE >>\\n\");\n";
        let exn_id = get_accu_id ind in
        fprintf oc "  RAISE(%a);\n" export_val_desc exn_id;
      | Poptrap ->
        let frame_sz = compute_frame_size ind in
        let accu_id = get_accu_id ind in
        if is_cell accu_id && is_read accu_id then
          fprintf oc "  SAVED_POPTRAP(%a, %d);\n" export_cell accu_id
            frame_sz
        else
          fprintf oc "  POPTRAP(%d);\n" frame_sz;
      | Const n ->
        export_gen_move export_const n ind;
      | Unapp unop ->
        export_unapp ind unop;
      | Offsetref ofs ->
        let ref_id = get_accu_id ind in
        fprintf oc "  OFFSETREF(%d, %a);\n" ofs export_val_desc ref_id;
        export_gen_move export_const 0 ind;
      | Binapp binop ->
        export_binapp ind binop;
      | Setvectitem ->
        let blk_id = get_accu_id ind in
        let indx_id = get_stack_id ind 0 in
        let val_id = get_stack_id ind 1 in
        fprintf oc "  SETVECTITEM(%a, %a, %a);\n" export_val_desc indx_id
          export_val_desc blk_id export_val_desc val_id;
        export_gen_move export_const 0 ind;
      | Setstringchar ->
        let str_id = get_accu_id ind in
        let indx_id = get_stack_id ind 0 in
        let char_id = get_stack_id ind 1 in
        fprintf oc "  SETSTRINGCHAR(%a, %a, %a);\n" export_val_desc indx_id
          export_val_desc str_id export_val_desc char_id;
        export_gen_move export_const 0 ind;
      | Branch ptr ->
        fprintf oc "  BRANCH(L%05X);\n" ptr.pointed.addr;
      | CondBranch cb ->
        export_cond_branch ind cb;
      | Switch (size_long, size_tag, tbl) ->
        let tbl_sz = Array.length tbl in
        let src_id = get_accu_id ind in
        if size_long = 0 then (
          fprintf oc "  switch (Tag_val(%a)) {\n" export_val_desc src_id;
          for i = 0 to tbl_sz - 1 do
            if i < tbl_sz - 1 then fprintf oc "    case %d: " i
            else fprintf oc "    default: ";
            fprintf oc "goto L%05X;\n" tbl.(i).pointed.addr;
          done;
          fprintf oc "  }\n";
        ) else if size_tag = 0 then (
          fprintf oc "  switch (Long_val(%a)) {\n" export_val_desc src_id;
          for i = 0 to tbl_sz - 1 do
            if i < tbl_sz - 1 then fprintf oc "    case %d: " i
            else fprintf oc "    default: ";
            fprintf oc "goto L%05X;\n" tbl.(i).pointed.addr;
          done;
          fprintf oc "  }\n";
        ) else (
          fprintf oc "  {\n";
          fprintf oc "    value tmp = %a;\n" export_val_desc src_id;
          fprintf oc "    if (Is_block(tmp)) {\n";
          fprintf oc "      switch (Tag_val(tmp)) {\n";
          for i = size_long to tbl_sz - 1 do
            if i < tbl_sz - 1 then
              fprintf oc "        case %d: " (i - size_long)
            else
              fprintf oc "        default: ";
            fprintf oc "goto L%05X;\n" tbl.(i).pointed.addr;
          done;
          fprintf oc "      }\n";
          fprintf oc "    } else {\n";
          fprintf oc "      switch (Long_val(tmp)) {\n";
          for i = 0 to tbl_sz - 1 do
            if i < tbl_sz - 1 then fprintf oc "        case %d: " i
            else fprintf oc "        default: ";
            fprintf oc "goto L%05X;\n" tbl.(i).pointed.addr;
          done;
          fprintf oc "      }\n";
          fprintf oc "    }\n";
          fprintf oc "  }\n";
        );
      | Getmethod ->
        let tag_id = get_accu_id ind in
        let obj_id = get_stack_id ind 0 in
        let meth_id = get_accu_id (ind + 1) in
        if is_read meth_id then (
          assert (is_cell meth_id);
          fprintf oc "  GETMETHOD(%a, %a, %a);\n" export_val_desc tag_id
            export_val_desc obj_id export_cell meth_id;
        );
      | Getpubmet (tag, _) ->
        let obj_id = get_accu_id ind in
        let meth_id = get_accu_id (ind + 1) in
        let odst_id = get_stack_id (ind + 1) 0 in
        export_move obj_id odst_id;
        if is_read meth_id then (
          assert (is_cell meth_id);
          fprintf oc "  GETPUBMET(%d, %a, %a);\n" tag export_val_desc obj_id
            export_cell meth_id
        );
      | Getdynmet ->
        let tag_id = get_accu_id ind in
        let obj_id = get_stack_id ind 0 in
        let meth_id = get_accu_id (ind + 1) in
        if is_read meth_id then (
          assert (is_cell meth_id);
          fprintf oc "  GETDYNMET(%a, %a, %a);\n" export_val_desc tag_id
            export_val_desc obj_id export_cell meth_id;
        );
      | Stop ->
        fprintf oc "  VM_STOP();\n"
      | Restart -> assert false
      | Grab _ -> assert false
    with Dead_code -> ()
  in
  Printer.print_comment "" true funs dbug oc fun_id;
  export_fun_def_signature oc fun_id use_env fun_desc.arity;
  if !Options.trace && fun_id <> 0 then
    fprintf oc "  printf(\"--> f%d\\n\");\n" fun_id;
  if fun_desc.is_special then
    fprintf oc "  OCAMLCC_SPECIAL_TAIL_CALL_HEADER(%d)\n" fun_id;
  export_fun_declarations oc fun_desc.arity var_nb use_tmp arg_depths;
  export_fun_init oc use_env fun_desc.arity arg_depths;
  Array.iteri export_instr body;
  export_fun_foot oc;
  assert (!catch_list = []);
;;

let export oc prims dbug funs dzeta_code =
  let main = IMap.find 0 dzeta_code in
  let rest = IMap.remove 0 dzeta_code in
  let f id (fd, _, _, _, _) =
    export_fun_decl_signature oc id (Block.test_useenv fd)
      (Block.test_inlinable funs fd) fd.arity
  in
  (*Printer.print_dzeta_code stdout dzeta_code;*)
  IMap.iter f rest;
  fprintf oc "\n";
  IMap.iter (export_fun oc prims dbug funs) rest;
  export_fun oc prims dbug funs 0 main;
;;

let run output_C_file prims data dbug funs dzeta_code max_arity =
  Options.verb_start "+ Generating %S..." output_C_file;
  let oc = open_out output_C_file in
  if !Options.no_main then Printf.fprintf oc "#define OCAMLCC_NO_MAIN\n";
  Printf.fprintf oc "#define OCAMLCC_GLOBAL_DATA_LENGTH %d\n"
    (String.length data.dump);
  Printf.fprintf oc "#define OCAMLCC_MAXIMUM_ARITY %d\n" max_arity;
  Printf.fprintf oc "#define OCAMLCC_ARCH_%a\n" Printer.print_arch
    !Options.arch;
  Printf.fprintf oc "#define OCAMLCC_SIGNAL_%a\n" Printer.print_sigconf
    !Options.sigconf;
  Printf.fprintf oc "#define OCAMLCC_EXCEPTION_%a\n" Printer.print_except
    !Options.except;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "#if !defined(__GNUC__)\n";
  Printf.fprintf oc
    "  #error - Incompatible code: compiler should be GNU C compatible\n";
  Printf.fprintf oc "#endif\n";
  begin match !Options.arch with
    | NO_ARCH -> ()
    | X86     ->
      Printf.fprintf oc
        "#if (!defined(__i386__) && !defined(__i486__)     \\\n     \
              && !defined(__i585__) && !defined(__i686__))\n";
      Printf.fprintf oc
        "  #error - Incompatible code: architecture should be x86\n";
      Printf.fprintf oc "#endif\n\n";
    | X86_64 ->
      Printf.fprintf oc
        "#if !defined(__x86_64__)\n";
      Printf.fprintf oc
        "  #error - Incompatible code: architecture should be x86-64\n";
      Printf.fprintf oc "#endif\n";
  end;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "#include <ocamlcc.h>\n\n";
  Data.export oc data;
  Printf.fprintf oc "\n";
  Applygen.run oc max_arity;
  export oc prims dbug funs dzeta_code;
  close_out oc;
  Options.verb_stop ();
;;
