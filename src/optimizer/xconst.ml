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

let compute_ids arity body =
  let len = Array.length body in
  let states = Array.make len None in
  let idvd_map = ref IMap.empty in
  let copies = ref [] in
  let equivs = ref [] in
  let new_idval val_desc =
    let id = Id.create () in
    idvd_map := IMap.add id val_desc !idvd_map;
    id
  in
  let copy_idval old_id =
    let new_id = new_idval (IMap.find old_id !idvd_map) in
    copies := (old_id, new_id) :: !copies;
    new_id
  in
  let new_const n = new_idval (VConst n) in
  let new_glob n = new_idval (VGlob n) in
  let new_glob_field n p = new_idval (VGlobField (n, p)) in
  let new_atom tag = new_idval (VAtom tag) in
  let new_clsr ofs = new_idval (VClsr ofs) in
  let new_env n = new_idval (VEnv n) in
  let new_arg n = new_idval (VArg n) in
  let new_cell () = new_idval VCell in
  let new_ptr ptr = new_idval (VPtr ptr) in
  let merge_ids id1 id2 = if id1 <> id2 then equivs := (id1, id2) :: !equivs in
  let merge_stacks stack stack' = Stk.iter2 merge_ids stack stack' in
  let set_state accu stack ind =
    match states.(ind) with
      | None ->
        states.(ind) <- Some { accu = accu ; stack = stack };
        true
      | Some { accu = accu' ; stack = stack' } ->
        assert (Stk.size stack = Stk.size stack');
        merge_ids accu accu';
        merge_stacks stack stack';
        false
  in
  let rec f accu stack ind =
    assert (ind >= 0 && ind < len);
    if set_state accu stack ind then
      match body.(ind).bc with
        | Acc n ->
          let new_accu = copy_idval (Stk.acc n stack) in
          f new_accu stack (ind + 1);
        | Push ->
          let new_stack = Stk.push (copy_idval accu) stack in
          f accu new_stack (ind + 1);
        | Pop n ->
          let new_stack = Stk.pop n stack in
          f accu new_stack (ind + 1);
        | Assign n ->
          let new_accu = new_const 0 in
          let accu_copy = copy_idval accu in
          merge_ids accu_copy (Stk.acc n stack);
          let new_stack = Stk.assign n accu_copy stack in
          f new_accu new_stack (ind + 1);
        | Envacc n ->
          let new_accu = new_env (n - 2) in
          f new_accu stack (ind + 1);
        | Pushretaddr ptr ->
          let new_stack = Stk.push (new_ptr ptr) stack in
          f accu new_stack (ind + 1);
        | DynamicApply ((1|2|3) as n) | PartialApply ((1|2|3) as n)
        | StaticApply ((1|2|3) as n, _) ->
          let new_accu = new_cell () in
          let new_stack = Stk.pop n stack in
          f new_accu new_stack (ind + 1)
        | DynamicApply n | PartialApply n | StaticApply (n, _) ->
          begin match IMap.find (Stk.acc n stack) !idvd_map with
            | VPtr ptr ->
              if ptr.pointed.index <> ind + 1 then
                begin match body.(ind + 1).bc with
                  | Branch ptr' ->
                    assert (ptr.pointed.index = ptr'.pointed.index)
                  | _ ->
                    assert false
                end;
              let new_accu = new_cell () in
              let new_stack = Stk.pop (n + 1) stack in
              f new_accu new_stack (ind + 1)
            | _ -> assert false
          end
        | DynamicAppterm (_, _) | PartialAppterm (_, _)
        | SpecialAppterm (_, _) | StaticAppterm (_, _, _) ->
          ();
        | Return _ ->
          ();
        | Closure (n, _) ->
          let new_accu = new_cell () in
          let new_stack =
            if n = 0 then stack else Stk.pop (n - 1) stack
          in
          f new_accu new_stack (ind + 1);
        | Closurerec (nf, nv, _, _) ->
          let rec g stk i =
            if i = nf then stk else g (Stk.push (new_cell ()) stk) (i + 1)
          in
          let new_accu = new_cell () in
          let new_stack0 =
            if nv = 0 then stack else Stk.pop (nv - 1) stack
          in
          let new_stack1 = Stk.push (copy_idval new_accu) new_stack0 in
          let new_stack2 = g new_stack1 1 in
          f new_accu new_stack2 (ind + 1);
        | Offsetclosure ofs ->
          let new_accu = new_clsr ofs in
          f new_accu stack (ind + 1);
        | Getglobal n ->
          let new_accu = new_glob n in
          f new_accu stack (ind + 1);
        | Getglobalfield (n, p) ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1);
          ignore (n, p, new_glob_field);
        (* (* WARNING: compression of mutable global array access *)
           let new_accu = new_glob_field n p in
           f new_accu stack (ind + 1);
        *)
        | Setglobal _ ->
          let new_accu = new_const 0 in
          f new_accu stack (ind + 1);
        | Atom tag ->
          let new_accu = new_atom tag in
          f new_accu stack (ind + 1);
        | Makeblock (size, _) | Makefloatblock size ->
          let new_accu = new_cell () in
          let new_stack = Stk.pop (size - 1) stack in
          f new_accu new_stack (ind + 1);
        | Getfield _ ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1);
        | Getfloatfield _ ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1);
        | Setfield _ | Setfloatfield _ ->
          let new_accu = new_const 0 in
          let new_stack = Stk.pop 1 stack in
          f new_accu new_stack (ind + 1);
        | Checksignals ->
          f accu stack (ind + 1);
        | Ccall (narg, _) ->
          let new_accu = new_cell () in
          let new_stack = Stk.pop (narg - 1) stack in
          f new_accu new_stack (ind + 1);
        | Pushtrap ptr ->
          let exn_id = new_cell () in
          let new_stack = Stk.push (new_cell ()) stack in
          f exn_id stack ptr.pointed.index;
          f accu new_stack (ind + 1);
        | Raise ->
          ();
        | Poptrap ->
          let new_stack = Stk.pop 1 stack in
          f accu new_stack (ind + 1);
        | Const n ->
          let new_accu = new_const n in
          f new_accu stack (ind + 1);
        | Unapp _ ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1);
        | Offsetref _ ->
          let new_accu = new_const 0 in
          f new_accu stack (ind + 1);
        | Binapp _ ->
          let new_accu = new_cell () in
          let new_stack = Stk.pop 1 stack in
          f new_accu new_stack (ind + 1);
        | Setvectitem | Setstringchar ->
          let new_accu = new_const 0 in
          let new_stack = Stk.pop 2 stack in
          f new_accu new_stack (ind + 1);
        | Branch ptr ->
          f accu stack ptr.pointed.index;
        | CondBranch cb ->
          f accu stack (Instr.ptr_of_cond_branch cb).pointed.index;
          f accu stack (ind + 1);
        | Switch (_, _, ptrs) ->
          Array.iter (fun ptr -> f accu stack ptr.pointed.index) ptrs;
        | Getmethod ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1)
        | Getpubmet (_, _) ->
          let new_accu = new_cell () in
          let new_stack = Stk.push (copy_idval accu) stack in
          f new_accu new_stack (ind + 1)
        | Getdynmet ->
          let new_accu = new_cell () in
          f new_accu stack (ind + 1)
        | Stop ->
          ()
        | Restart -> assert false
        | Grab _ -> assert false
  in
  let init_accu = new_clsr 0 in
  let init_stack =
    let rec f i acc =
      if i = -1 then acc else
        f (i - 1) (Stk.push (new_arg i) acc)
    in
    f (arity - 1) Stk.empty
  in
  f init_accu init_stack 0;
  (states, !equivs, !copies, !idvd_map)
;;

let unify_ids states equivs copies idvd_map =
  (* eqset_map: id -> the id equivalence class *)
  let eqset_map =
    let f map (id1, id2) =
      let s1 = try IMap.find id1 map with Not_found -> ISet.singleton id1 in
      let s2 = try IMap.find id2 map with Not_found -> ISet.singleton id2 in
      let s = ISet.union s1 s2 in
      ISet.fold (fun id acc -> IMap.add id s acc) s map
    in
    List.fold_left f IMap.empty equivs
  in
  (* eq_sets: list of equivalence classes *)
  let eq_sets =
    let f _ set sets = if List.memq set sets then sets else set :: sets in
    IMap.fold f eqset_map []
  in
  (* id_repr_map: id -> unique representative of id's equivalence class *)
  let id_repr_map = IMap.map ISet.min_elt eqset_map in
  (* Tools *)
  let id_repr id =
    try IMap.find id id_repr_map
    with Not_found -> id
  in
  let id_eqset id =
    try IMap.find id eqset_map
    with Not_found -> ISet.singleton id
  in
  (* idvd_map': fix of idvd_map with equivalences *)
  let idvd_map' =
    let f map set =
      let rec compute_vd set old_vd =
        if ISet.is_empty set then old_vd else
          let new_id = ISet.choose set in
          let new_vd = IMap.find new_id idvd_map in
          let new_set = ISet.remove new_id set in
          if old_vd = new_vd then compute_vd new_set old_vd
          else VCell
      in
      let first_id = ISet.choose set in
      let first_vd = IMap.find first_id idvd_map in
      let first_set = ISet.remove first_id set in
      let vd_repr = compute_vd first_set first_vd in
      let g id map = IMap.add id vd_repr map in
      ISet.fold g set map
    in
    List.fold_left f idvd_map eq_sets
  in
  (* idvd_map'': fix of idvd_map' with copies *)
  let idvd_map'' =
    let f ((_, map) as acc) (id1, id2) =
      match (IMap.find id1 map, IMap.find id2 map) with
        | (VCell, VCell) -> acc
        | (VCell, _) ->
          let g id map = IMap.add id VCell map in
          (true, ISet.fold g (id_eqset id2) map)
        | _ -> acc
    in
    let rec fix_point map =
      let (flag, new_map) = List.fold_left f (false, map) copies in
      if flag then fix_point new_map else new_map
    in
    fix_point idvd_map'
  in
  (* idvd_map''': restriction of idvd_map'' according to id_repr *)
  let idvd_map''' =
    let f id vd map = if id_repr id = id then IMap.add id vd map else map in
    IMap.fold f idvd_map'' IMap.empty
  in
  (* states': fix of states according to id_repr *)
  let states' =
    let map_state { accu = accu ; stack = stack } =
      { accu = id_repr accu ; stack = Stk.map id_repr stack }
    in
    let map_state_opt state_opt =
      match state_opt with
        | None -> None
        | Some state -> Some (map_state state)
    in
    Array.map map_state_opt states
  in
  (states', idvd_map''')
;;

let compute_gc_read prims body states fun_tys =
  let gc_read = ref ISet.empty in
  let run_gc = ref false in
  let f ind instr =
    match instr.bc with
      | DynamicApply _ | PartialApply _ ->
        run_gc := true;
        begin match (states.(ind), states.(ind + 1)) with
          | (Some { accu = accu ; stack = _ },
             Some { accu = _ ; stack = stack }) ->
            gc_read :=
              Stk.fold_left (fun acc id -> ISet.add id acc)
              (ISet.add accu !gc_read) stack
          | _ -> ()
        end;
      | StaticApply (_, ptr) ->
        let (_, _, r_gc) = IMap.find ptr.pointed.index fun_tys in
        if !r_gc then (
          run_gc := true;
          match (states.(ind), states.(ind + 1)) with
            | (Some { accu = accu ; stack = _ },
               Some { accu = _ ; stack = stack }) ->
              gc_read :=
                Stk.fold_left (fun acc id -> ISet.add id acc)
                (ISet.add accu !gc_read) stack
            | _ -> ()
        );
      | DynamicAppterm (_, _) | PartialAppterm (_, _)
      | SpecialAppterm (_, _) ->
        run_gc := true;
      | StaticAppterm (_, _, ptr) ->
        let (_, _, r_gc) = IMap.find ptr.pointed.index fun_tys in
        if !r_gc then run_gc := true;
      | Closure _ | Closurerec _ | Makeblock _ | Makefloatblock _
      | Getfloatfield _ ->
        run_gc := true;
        begin match states.(ind) with
          | Some { accu = accu ; stack = stack } ->
            gc_read :=
              Stk.fold_left (fun acc id -> ISet.add id acc)
              (ISet.add accu !gc_read) stack
          | None -> ()
        end;
      | Ccall (_, index) ->
        let r_gc =
          match Prim.describe prims.(index) with
            | None -> true
            | Some (r_gc, _, _) -> r_gc
        in
        if r_gc then (
          run_gc := true;
          match states.(ind) with
            | Some { accu = accu ; stack = stack } ->
              gc_read :=
                Stk.fold_left (fun acc id -> ISet.add id acc)
                (ISet.add accu !gc_read) stack
            | None -> ()
        );
      | Checksignals | Poptrap when !Options.sigconf = Reactive ->
        run_gc := true;
        begin match states.(ind) with
          | Some { accu = accu ; stack = stack } ->
            gc_read :=
              Stk.fold_left (fun acc id -> ISet.add id acc)
              (ISet.add accu !gc_read) stack
          | None -> ()
        end;
      | _ -> ()
  in
  Array.iteri f body;
  (!gc_read, !run_gc)
;;

let compute_ptrs prims body states idvd_map gc_read fun_tys =
  let set_add set id = set := ISet.add id !set in
  let map_add map id1 id2 =
    try map := IMap.add id1 (ISet.add id2 (IMap.find id1 !map)) !map
    with Not_found -> map := IMap.add id1 (ISet.singleton id2) !map
  in
  let get_accu_id ind =
    match states.(ind) with
      | None -> raise Not_found
      | Some { accu = accu ; stack = _ } -> accu
  in
  let get_stack_id ind n =
    match states.(ind) with
      | None -> raise Not_found
      | Some { accu = _ ; stack = stack } -> (Stk.acc n stack)
  in
  let return_ptr = ref false in
  let int_set = ref ISet.empty in
  let return_set = ref ISet.empty in
  let ptr_write_set = ref ISet.empty in
  let ptr_read_set = ref ISet.empty in
  let int_write_set = ref ISet.empty in
  let int_read_set = ref ISet.empty in
  let move_write_map = ref IMap.empty in
  let move_read_map = ref IMap.empty in
  let force_int id = set_add int_set id in
  let return id = set_add return_set id in
  let ptr_write id = set_add ptr_write_set id in
  let ptr_read id = set_add ptr_read_set id in
  let int_write id = set_add int_write_set id in
  let int_read id = set_add int_read_set id in
  let move from_id to_id =
    map_add move_read_map to_id from_id;
    map_add move_write_map from_id to_id;
  in
  let f ind instr =
    try match instr.bc with
      | Acc n ->
        move (get_stack_id ind n) (get_accu_id (ind + 1));
      | Push ->
        move (get_accu_id ind) (get_stack_id (ind + 1) 0);
      | Pop _ ->
        ()
      | Assign n ->
        move (get_accu_id ind) (get_stack_id (ind + 1) n);
      | Envacc _ ->
        ptr_write (get_accu_id (ind + 1));
      | Pushretaddr _ ->
        ()
      | DynamicApply narg | PartialApply narg ->
        ptr_read (get_accu_id ind);
        for i = 0 to narg - 1 do ptr_read (get_stack_id ind i) done;
        ptr_write (get_accu_id (ind + 1));
      | StaticApply (narg, ptr) ->
        let (ptr_args, ptr_res, _) = IMap.find ptr.pointed.index fun_tys in
        ptr_read (get_accu_id ind);
        for i = 0 to narg - 1 do
          if ptr_args.(i) then ptr_read (get_stack_id ind i)
          else int_read (get_stack_id ind i)
        done;
        if !ptr_res then ptr_write (get_accu_id (ind + 1))
        else int_write (get_accu_id (ind + 1));
      | DynamicAppterm (narg, _) | PartialAppterm (narg, _)
      | SpecialAppterm (narg, _) ->
        ptr_read (get_accu_id ind);
        for i = 0 to narg - 1 do ptr_read (get_stack_id ind i) done;
        return_ptr := true;
      | StaticAppterm (narg, _, ptr) ->
        let (ptr_args, ptr_res, _) = IMap.find ptr.pointed.index fun_tys in
        ptr_read (get_accu_id ind);
        for i = 0 to narg - 1 do
          if ptr_args.(i) then ptr_read (get_stack_id ind i)
          else int_read (get_stack_id ind i)
        done;
        if !ptr_res then return_ptr := true;
      | Return _ ->
        return (get_accu_id ind);
        ptr_read (get_accu_id ind);
      | Closure (nv, _) ->
        if nv <> 0 then (
          ptr_read (get_accu_id ind);
          for i = 0 to nv - 2 do ptr_read (get_stack_id ind i) done;
        );
        ptr_write (get_accu_id (ind + 1));
      | Closurerec (nf, nv, _, _) ->
        if nv <> 0 then (
          ptr_read (get_accu_id ind);
          for i = 0 to nv - 2 do ptr_read (get_stack_id ind i) done;
        );
        ptr_write (get_accu_id (ind + 1));
        int_read (get_accu_id (ind + 1));
        for i = 0 to nf - 1 do ptr_write (get_stack_id (ind + 1) i) done;
      | Offsetclosure _ ->
        ptr_write (get_accu_id (ind + 1));
      | Getglobal _ ->
        ptr_write (get_accu_id (ind + 1));
      | Getglobalfield (_, _) ->
        ptr_write (get_accu_id (ind + 1));
      | Setglobal _ ->
        ptr_read (get_accu_id ind);
        int_write (get_accu_id (ind + 1));
      | Atom _ ->
        int_write (get_accu_id (ind + 1));
      | Makeblock (size, _) | Makefloatblock size ->
        ptr_read (get_accu_id ind);
        for i = 0 to size - 2 do ptr_read (get_stack_id ind i) done;
        ptr_write (get_accu_id (ind + 1));
      | Getfield _ | Getfloatfield _ ->
        ptr_read (get_accu_id ind);
        ptr_write (get_accu_id (ind + 1));
      | Setfield _ | Setfloatfield _ ->
        ptr_read (get_accu_id ind);
        ptr_read (get_stack_id ind 0);
        int_write (get_accu_id (ind + 1));
      | Checksignals ->
        ();
      | Ccall (narg, index) ->
        begin match Prim.describe prims.(index) with
          | None ->
            ptr_read (get_accu_id ind);
            for i = 0 to narg - 2 do ptr_read (get_stack_id ind i) done;
            ptr_write (get_accu_id (ind + 1));
          | Some (_, res, args) ->
            if List.length args <> narg then (
              let msg =
                Printf.sprintf "invalid argument number: %s" prims.(index)
              in
              failwith msg;
            );
            let rec f i args =
              match args with
                | [] -> ()
                | arg :: rest ->
                  if arg = Integer then (
                    force_int (get_stack_id ind i);
                    int_read (get_stack_id ind i);
                  ) else ptr_read (get_stack_id ind i);
                  f (i + 1) rest
            in
            begin match args with
              | [] -> assert false
              | arg :: rest ->
                if arg = Integer then (
                  force_int (get_accu_id ind);
                  int_read (get_accu_id ind);
                ) else ptr_read (get_accu_id ind);
                f 0 rest;
            end;
            if res = Integer then int_write (get_accu_id (ind + 1))
            else ptr_write (get_accu_id (ind + 1));
        end
      | Pushtrap ptr ->
        let exn_id = get_accu_id ptr.pointed.index in
        ptr_write exn_id;
      | Raise ->
        ptr_read (get_accu_id ind);
      | Poptrap ->
        ();
      | Const _ ->
        int_write (get_accu_id (ind + 1));
      | Unapp Vectlength ->
        ptr_read (get_accu_id ind);
        int_write (get_accu_id (ind + 1));
      | Unapp Isint ->
        int_read (get_accu_id ind);
        int_write (get_accu_id (ind + 1));
      | Unapp (Boolnot | Offsetint _ | Negint) ->
        force_int (get_accu_id ind);
        int_read (get_accu_id ind);
        int_write (get_accu_id (ind + 1));
      | Offsetref _ ->
        ptr_read (get_accu_id ind);
        int_write (get_accu_id (ind + 1));
      | Binapp (Addint | Subint | Mulint | Divint | Modint | Andint | Orint |
          Xorint | Lslint | Lsrint | Asrint | Ltint | Leint | Gtint |
              Geint | Ultint | Ugeint) ->
        force_int (get_accu_id ind);
        force_int (get_stack_id ind 0);
        int_read (get_accu_id ind);
        int_read (get_stack_id ind 0);
        int_write (get_accu_id (ind + 1));
      | Binapp (Eq | Neq) ->
        ptr_read (get_accu_id ind);
        ptr_read (get_stack_id ind 0);
        int_write (get_accu_id (ind + 1));
      | Binapp Getvectitem ->
        force_int (get_stack_id ind 0);
        ptr_read (get_accu_id ind);
        int_read (get_stack_id ind 0);
        ptr_write (get_accu_id (ind + 1));
      | Binapp Getstringchar ->
        force_int (get_stack_id ind 0);
        ptr_read (get_accu_id ind);
        int_read (get_stack_id ind 0);
        int_write (get_accu_id (ind + 1));
      | Setvectitem ->
        force_int (get_stack_id ind 0);
        ptr_read (get_accu_id ind);
        int_read (get_stack_id ind 0);
        ptr_read (get_stack_id ind 1);
        int_write (get_accu_id (ind + 1));
      | Setstringchar ->
        force_int (get_stack_id ind 0);
        force_int (get_stack_id ind 1);
        ptr_read (get_accu_id ind);
        int_read (get_stack_id ind 0);
        int_read (get_stack_id ind 1);
        int_write (get_accu_id (ind + 1));
      | Branch _ ->
        ();
      | CondBranch _ ->
          (* WARNING: do not enable this:
             force_int (get_accu_id ind); *)
        int_read (get_accu_id ind);
      | Switch (_, size_tag, _) ->
        if size_tag = 0 then int_read (get_accu_id ind)
        else ptr_read (get_accu_id ind);
      | Getmethod ->
        force_int (get_accu_id ind);
        int_read (get_accu_id ind);
        ptr_read (get_stack_id ind 0);
        ptr_write (get_accu_id (ind + 1));
      | Getpubmet (_, _) ->
        ptr_read (get_accu_id ind);
        ptr_write (get_accu_id (ind + 1));
        ptr_write (get_stack_id (ind + 1) 0);
      | Getdynmet ->
        force_int (get_accu_id ind);
        int_read (get_accu_id ind);
        ptr_read (get_stack_id ind 0);
        ptr_write (get_accu_id (ind + 1));
      | Stop ->
        ();
      | Restart -> assert false
      | Grab _ -> assert false
    with Not_found -> ()
  in
  let fix_moves map set =
    let rec f id =
      try ISet.iter g (IMap.find id map)
      with Not_found -> ()
    and g id =
      if not (ISet.mem id !set) then (
        set := ISet.add id !set;
        f id;
      )
    in
    ISet.iter f !set
  in
  begin match states.(0) with
    | None -> assert false;
    | Some state ->
      Stk.iter (fun id -> ptr_write id) state.stack;
  end;
  Array.iteri f body;
  fix_moves !move_read_map int_set;
  fix_moves !move_write_map int_set;
  fix_moves !move_read_map ptr_read_set;
  fix_moves !move_read_map int_read_set;
  fix_moves !move_write_map ptr_write_set;
  fix_moves !move_write_map int_write_set;
  let ptr_res =
    ISet.iter
      (fun id -> if ISet.mem id !ptr_write_set then return_ptr := true)
      !return_set;
    !return_ptr
  in
  let cell_set =
    let f id vd acc = if vd = VCell then ISet.add id acc else acc in
    IMap.fold f idvd_map ISet.empty
  in
  let arg_set =
    let f id vd acc =
      match vd with
        | VArg _ -> ISet.add id acc
        | _ -> acc
    in
    IMap.fold f idvd_map ISet.empty
  in
  let gc_read = ref gc_read in
  let read_set =
    ISet.inter (ISet.union !ptr_read_set !int_read_set) cell_set
  in
  let read_args =
    ISet.fold
      (fun id acc ->
         if ISet.mem id !ptr_read_set || ISet.mem id !int_read_set then
           match IMap.find id idvd_map with
             | VArg n -> ISet.add n acc
             | _ -> assert false
         else
           acc
      ) arg_set ISet.empty
  in
  if !Options.no_xconst then (
    ISet.iter (fun id -> ptr_read id; ptr_write id; int_read id; int_write id;
                 gc_read := ISet.add id !gc_read) cell_set;
    int_set := ISet.empty;
  );
  let ptr_set =
    ISet.diff (ISet.inter (ISet.inter !ptr_write_set !ptr_read_set)
                 (ISet.union (ISet.inter !gc_read cell_set) arg_set)) !int_set
  in
  (* Remark: if id is not read then id is not a pointer or not a variable. *)
  (ptr_set, read_set, ptr_res, read_args)
;;

let run prims funs =
  let () = Options.verb_start "+ Computing cell types" in
  let compute_arg_ids arity states =
    match states.(0) with
    | None -> assert false
    | Some state ->
        let arg_ids = Array.make arity 0 in
        Stk.iteri (fun i id -> arg_ids.(i) <- id) state.stack;
        arg_ids
  in
  let extract_infos id fun_desc acc =
    let arity = fun_desc.arity and body = fun_desc.body in
    let (states, equivs, copies, idvd_map) = compute_ids arity body in
    let (states', idvd_map') = unify_ids states equivs copies idvd_map in
    let arg_ids = compute_arg_ids fun_desc.arity states' in
    (id, fun_desc, states', idvd_map', arg_ids) :: acc
  in
  let infos = IMap.fold extract_infos funs [] in
  let fun_tys =
    let f id fun_desc acc =
      IMap.add id (Array.make fun_desc.arity false, ref false, ref false) acc
    in
    IMap.fold f funs IMap.empty
  in
  let update_fun_tys (sets_map, flag) (id, fun_desc, states, idvd_map, arg_ids)=
    let (gc_read, r_gc) = compute_gc_read prims fun_desc.body states fun_tys in
    let (ptr_set, read_set, p_res, read_args) =
      compute_ptrs prims fun_desc.body states idvd_map gc_read fun_tys
    in
    let (ptr_args, ptr_res, run_gc) = IMap.find id fun_tys in
    let new_flag = ref flag in
    for i = 0 to fun_desc.arity - 1 do
      if ISet.mem arg_ids.(i) ptr_set && not ptr_args.(i) then (
        ptr_args.(i) <- true;
        new_flag := true;
      );
    done;
    if p_res && not !ptr_res then (
      ptr_res := true;
      new_flag := true;
    );
    if r_gc && not !run_gc then (
      run_gc := true;
      new_flag := true;
    );
    (IMap.add id (ptr_set, read_set, read_args) sets_map, !new_flag)
  in
  let rec fix_point () =
    Options.message ".";
    let (sets_map, flag) =
      List.fold_left update_fun_tys (IMap.empty, false) infos
    in
    if flag then fix_point () else sets_map
  in
  let sets_map = fix_point () in
  let compute_dzeta_code acc (id, fun_desc, states, idvd_map, _) =
    let (ptr_set, read_set, read_args) = IMap.find id sets_map in
    IMap.add id (fun_desc, states, idvd_map, ptr_set, read_set, read_args) acc
  in
  Options.verb_stop();
  let dzeta_code = List.fold_left compute_dzeta_code IMap.empty infos in
  (dzeta_code, fun_tys)
;;
