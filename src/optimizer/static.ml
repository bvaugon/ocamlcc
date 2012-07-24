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

module FPSet = Set.Make (
  struct
    type t = orig desc
    let compare (x : t) (y : t) = compare x y
  end
);;

let interp current_closure body =
  let len = Array.length body in
  let desc_set_tbl = Array.make len FPSet.empty in
  let rec f accu stack ind =
    assert (ind >= 0 && ind < len);
    let desc = { accu = accu; stack = stack } in
    if not (FPSet.mem desc desc_set_tbl.(ind)) then (
      desc_set_tbl.(ind) <- FPSet.add desc desc_set_tbl.(ind);
      match body.(ind).bc with
        | Acc n ->
          if n >= Stk.size stack
          then f OUnknown stack (ind + 1)
          else f (Stk.acc n stack) stack (ind + 1)
        | Push ->
          f accu (Stk.push accu stack) (ind + 1)
        | Pop n ->
          f accu (Stk.pop n stack) (ind + 1)
        | Assign n ->
          f OUnknown (Stk.assign n accu stack) (ind + 1)
        | Envacc n ->
          begin match current_closure with
            | EClosure (_, env) ->
              let env_n = n - 2 in
              assert (env_n >= 0 && env_n < Array.length env);
              f env.(env_n) stack (ind + 1)
            | EClosurerec (ptrs, env, ofs) ->
              let env_n = n - 3 * (Array.length ptrs - ofs) + 1 in
              assert (env_n >= 0 && env_n < Array.length env);
              f env.(env_n) stack (ind + 1)
            | EUnknown ->
              f OUnknown stack (ind + 1)
          end
        | Pushretaddr ptr ->
          f OUnknown stack ptr.pointed.index;
          let origs = [ OUnknown ; OUnknown ; OUnknown ] in
          f accu (Stk.push_list origs stack) (ind + 1)
        | DynamicApply ((1|2|3) as n) | StaticApply ((1|2|3) as n, _)
        | PartialApply ((1|2|3) as n) ->
          f OUnknown (Stk.pop n stack) (ind + 1)
        | DynamicApply _ | StaticApply _ | PartialApply _ ->
          ((* Nothing to do here, see Pushretaddr *))
        | Closure (nv, ptr) ->
          let env = Array.make nv accu in
          for i = 1 to nv - 1 do
            env.(i) <- Stk.acc (i - 1) stack
          done;
          let stack0 =
            if nv = 0 then stack else Stk.pop (nv - 1) stack
          in
          f (OClosure (ptr, env)) stack0 (ind + 1)
        | Closurerec (nf, nv, ptr, tbl) ->
          let env = Array.make nv accu in
          for i = 1 to nv - 1 do
            env.(i) <- Stk.acc (i - 1) stack
          done;
          let stack0 =
            if nv = 0 then stack else Stk.pop (nv - 1) stack
          in
          let ptrs = Array.append [| ptr |] tbl in
          let accu0 = OClosurerec (ptrs, env, 0) in
          let rec g i stack1 =
            if i = nf then stack1 else
              g (i + 1) (Stk.push (OClosurerec (ptrs, env, i)) stack1)
          in
          f accu0 (g 1 (Stk.push accu0 stack0)) (ind + 1)
        | Offsetclosure n ->
          begin match current_closure with
            | EUnknown ->
              f OUnknown stack (ind + 1)
            | EClosure (_, _) ->
              assert false
            | EClosurerec (ptrs, env, ofs) ->
              f (OClosurerec (ptrs, env, ofs + n / 2)) stack
                (ind + 1)
          end
        | Getglobal n ->
          f (OGlobal n) stack (ind + 1)
        | Getglobalfield (n, p) ->
          f (OGlobalField (n, p)) stack (ind + 1)
        | Setglobal _ ->
          f accu stack (ind + 1)
        | Atom _ ->
          f OUnknown stack (ind + 1)
        | Makeblock (size, _) | Makefloatblock size ->
          f OUnknown (Stk.pop (size - 1) stack) (ind + 1)
        | Getfield _ | Getfloatfield _ ->
          f OUnknown stack (ind + 1)
        | Setfield _ | Setfloatfield _ ->
          f OUnknown (Stk.pop 1 stack) (ind + 1)
        | Pushtrap ptr ->
          let origs = [ OUnknown ; OUnknown ; OUnknown ; OUnknown ] in
          f accu stack ptr.pointed.index;
          f accu (Stk.push_list origs stack) (ind + 1)
        | Poptrap ->
          f accu (Stk.pop 4 stack) (ind + 1)
        | Checksignals ->
          f accu stack (ind + 1)
        | Ccall (narg, _) ->
          f OUnknown (Stk.pop (narg - 1) stack) (ind + 1)
        | Const _ | Offsetref _ | Unapp _ ->
          f OUnknown stack (ind + 1)
        | Binapp _ ->
          f OUnknown (Stk.pop 1 stack) (ind + 1)
        | Setvectitem | Setstringchar ->
          f OUnknown (Stk.pop 2 stack) (ind + 1)
        | Branch ptr ->
          f accu stack ptr.pointed.index
        | CondBranch cb ->
          f accu stack (Instr.ptr_of_cond_branch cb).pointed.index;
          f accu stack (ind + 1)
        | Switch (_, _, ptrs) ->
          Array.iter (fun ptr -> f accu stack ptr.pointed.index) ptrs
        | Getmethod ->
          f OUnknown stack (ind + 1)
        | Getpubmet (_, _) ->
          f OUnknown (Stk.push accu stack) (ind + 1)
        | Getdynmet ->
          f OUnknown stack (ind + 1)

        | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _
        | SpecialAppterm _ | Raise | Return _ | Stop -> ()

        | Restart -> assert false
        | Grab _ -> assert false
    )
  in
  f OUnknown Stk.empty 0;
  desc_set_tbl
;;

let merge desc_set_tbl =
  let rec merge_origs orig orig' =
    if orig = orig' then orig else
      match (orig, orig') with
        | (OClosure (ptr, origs), OClosure (ptr', origs')) when ptr = ptr' ->
          OClosure (ptr, merge_origs_arrays origs origs')
        | (OClosurerec (ptrs, origs, ofs), OClosurerec (ptrs', origs', ofs'))
            when ofs = ofs' && ptrs = ptrs' ->
          OClosurerec (ptrs, merge_origs_arrays origs origs', ofs)
        | (_, _) ->
          OUnknown
  and merge_origs_arrays origs origs' =
    let len = Array.length origs and len' = Array.length origs' in
    assert (len = len');
    Array.init len (fun i -> merge_origs origs.(i) origs'.(i))
  in
  let merge_stacks stack stack' = Stk.map2 merge_origs stack stack' in
  let merge_descs desc desc' =
    let accu = merge_origs desc.accu desc'.accu in
    let stack = merge_stacks desc.stack desc'.stack in
    { accu = accu ; stack = stack }
  in
  let merge_desc_set desc_set =
    if FPSet.is_empty desc_set then None else
      let one = FPSet.choose desc_set in
      let rest = FPSet.remove one desc_set in
      Some (FPSet.fold merge_descs rest one)
  in
  let desc_option_tbl = Array.map merge_desc_set desc_set_tbl in
  desc_option_tbl
;;

let check_globals { values = values ; dump = _ } funs =
  let len = List.length values in
  let checks = Array.make len (0, true, true) in
  let rec throw_values i vs = match vs with
    | [] -> ()
    | VInt 0 :: rest ->
      checks.(i) <- (0, false, false); throw_values (i + 1) rest
    | _ :: rest ->
      throw_values (i + 1) rest
  in
  let check_instr instr = match instr.bc with
    | Setglobal n ->
      assert (n >= 0 && n < len);
      let (c, g, gf) = checks.(n) in
      checks.(n) <- (c+1, g, gf)
    | Getglobal n ->
      assert (n >= 0 && n < len);
      let (c, _, gf) = checks.(n) in
      checks.(n) <- (c, true, gf)
    | Getglobalfield (n, _) ->
      assert (n >= 0 && n < len);
      let (c, g, _) = checks.(n) in
      checks.(n) <- (c, g, true)
    | _ -> ()
  in
  let check_body body = Array.iter check_instr body in
  let filter (set_count, glob, glob_field) =
    set_count = 1 && (glob || glob_field) && not (glob && glob_field)
  in
  throw_values 0 values;
  IMap.iter (fun _ fd -> check_body fd.body) funs;
  Array.map filter checks
;;

let search_closures body desc_option_tbl =
  let len = Array.length body in
  let make_origs { accu = accu ; stack = stack } sz =
    let origs = Array.make sz accu in
    for i = 1 to sz - 1 do
      origs.(i) <- Stk.acc (i - 1) stack
    done;
    origs
  in
  let rec f i acc =
    if i = len then acc else
      match desc_option_tbl.(i) with
        | None -> f (i + 1) acc
        | Some desc ->
          match body.(i).bc with
            | Closure (sz, ptr) ->
              let env = EClosure (ptr, make_origs desc sz) in
              f (i + 1) ((ptr.pointed.index, env) :: acc)
            | Closurerec (_, sz, ptr, ptrs) ->
              let ptrs = Array.append [| ptr |] ptrs in
              let origs = make_origs desc sz in
              let mkindenvs ofs ptr =
                (ptr.pointed.index, EClosurerec (ptrs, origs, ofs))
              in
              let indenvs = Array.to_list (Array.mapi mkindenvs ptrs) in
              f (i + 1) (indenvs @ acc)
            | _ ->
              f (i + 1) acc
  in
  f 0 []
;;

let update_glob_infos glob_infos checks body desc_option_tbl =
  let glob_of_orig orig = match orig with
    | OUnknown -> GUnknown
    | OClosure (ptr, _) -> GClosure ptr
    | OClosurerec (ptrs, _, ofs) -> GClosurerec (ptrs, ofs)
    | OGlobal n -> glob_infos.(n)
    | OGlobalField (m, p) ->
      match glob_infos.(m) with
        | GModule globs ->
          assert (p >= 0 && p < Array.length globs);
          globs.(p)
        | _ ->
          GUnknown
  in
  let f i instr =
    match instr.bc with
      | Setglobal n ->
        if checks.(n) then
          begin match desc_option_tbl.(i) with
            | None -> ()
            | Some { accu = OUnknown ; stack = _ } ->
              if i > 2 then (
                match (body.(i - 2).bc, body.(i - 1).bc) with
                  | (Makeblock (size, 0), Pop _) ->
                    begin match desc_option_tbl.(i - 2) with
                      | None -> ()
                      | Some { accu = accu ; stack = stack } ->
                        assert (Stk.size stack >= size - 1);
                        assert (size > 0);
                        let globs = Array.make size GUnknown in
                        globs.(0) <- glob_of_orig accu;
                        for i = 1 to size - 1 do
                          globs.(i) <-
                            glob_of_orig (Stk.acc (i - 1) stack);
                        done;
                        glob_infos.(n) <- GModule globs;
                    end
                  | _ -> ()
              )
            | Some { accu = accu ; stack = _ } ->
              glob_infos.(n) <- glob_of_orig accu;
          end
      | _ -> ()
  in
  Array.iteri f body;
;;

let set_applies glob_infos body desc_option_tbl funs =
  let set_static_apply i narg ptr =
    let fun_desc = IMap.find ptr.pointed.index funs in
    if narg = fun_desc.arity then
      body.(i).bc <- StaticApply (narg, ptr)
    else if narg < fun_desc.arity then
      body.(i).bc <- PartialApply narg
  in
  let set_static_appterm i narg slotsize ptr =
    let fun_desc = IMap.find ptr.pointed.index funs in
    if narg = fun_desc.arity then
      body.(i).bc <- StaticAppterm (narg, slotsize, ptr)
    else if narg < fun_desc.arity then
      body.(i).bc <- PartialAppterm (narg, slotsize)
  in
  let maybe_set_static set_static i =
    match desc_option_tbl.(i) with
      | None -> ()
      | Some { accu = accu ; stack = _ } ->
        match accu with
          | OGlobal n ->
            begin match glob_infos.(n) with
              | GClosure ptr ->
                set_static ptr
              | GClosurerec (ptrs, ofs) ->
                set_static ptrs.(ofs)
              | _ -> ()
            end
          | OGlobalField (n, p) ->
            begin match glob_infos.(n) with
              | GModule globs ->
                begin match globs.(p) with
                  | GClosure ptr ->
                    set_static ptr
                  | GClosurerec (ptrs, ofs) ->
                    set_static ptrs.(ofs)
                  | _ -> ()
                end
              | _ -> ()
            end
          | OClosure (ptr, _) ->
            set_static ptr
          | OClosurerec (ptrs, _, ofs) ->
            set_static ptrs.(ofs)
          | OUnknown -> ()
  in
  let f i instr =
    match instr.bc with
      | DynamicApply narg ->
        maybe_set_static (set_static_apply i narg) i
      | DynamicAppterm (narg, slotsize) ->
        maybe_set_static (set_static_appterm i narg slotsize) i
      | _ -> ()
  in
  Array.iteri f body;
;;

let compute_applies data funs =
  Options.verb_start "+ Folding closures...";
  let checks = check_globals data funs in
  let glob_nb = Array.length checks in
  let glob_infos = Array.make glob_nb GUnknown in
  let rec f env body =
    let desc_set_tbl = interp env body in
    let desc_option_tbl = merge desc_set_tbl in
    let indenvs = search_closures body desc_option_tbl in
    update_glob_infos glob_infos checks body desc_option_tbl;
    set_applies glob_infos body desc_option_tbl funs;
    List.iter (fun (index, env) -> f env (IMap.find index funs).body)
      indenvs;
  in
  f EUnknown (IMap.find 0 funs).body;
  Options.verb_stop ();
;;
