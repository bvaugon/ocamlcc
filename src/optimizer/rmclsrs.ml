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
    type t = int desc
    let compare (x : t) (y : t) = compare x y
  end
);;

let interp arity body env_uses =
  let len = Array.length body in
  let used = Array.make len false in
  let states = Array.make len FPSet.empty in
  let touch ind = if ind >= 0 then used.(ind) <- true in
  let touch_stack n stack =
    for i = 0 to n - 1 do touch (Stk.acc i stack) done;
  in
  let env_used ptr =
    let set = IMap.find ptr.pointed.index env_uses in
    not (ISet.is_empty set)
  in
  let rec f accu stack ind =
    assert (ind >= 0 && ind < len);
    let desc = { accu = accu ; stack = stack } in
    if not (FPSet.mem desc states.(ind)) then (
      states.(ind) <- FPSet.add desc states.(ind);
      match body.(ind).bc with
        | Acc n ->
          touch (Stk.acc n stack);
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Push ->
          let new_stack = Stk.push accu stack in
          f accu new_stack (ind + 1);
        | Pop n ->
          let new_stack = Stk.pop n stack in
          f accu new_stack (ind + 1);
        | Assign n ->
          touch accu;
          let new_accu = ind in
          let new_stack = Stk.assign n ind stack in
          f new_accu new_stack (ind + 1);
        | Envacc _ ->
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Pushretaddr ptr ->
          f accu stack ptr.pointed.index;
          let new_stack = Stk.push ind stack in
          f accu new_stack (ind + 1);
        | DynamicApply ((1|2|3) as n) | PartialApply ((1|2|3) as n) ->
          touch accu;
          touch_stack n stack;
          let new_accu = ind in
          let new_stack = Stk.pop n stack in
          f new_accu new_stack (ind + 1);
        | StaticApply ((1|2|3) as n, ptr) ->
          if env_used ptr then touch accu;
          touch_stack n stack;
          let new_accu = ind in
          let new_stack = Stk.pop n stack in
          f new_accu new_stack (ind + 1);
        | DynamicApply n | PartialApply n ->
          touch accu;
          touch_stack n stack;
          ((* Nothing to do, see Pushretaddr *))
        | StaticApply (n, ptr) ->
          if env_used ptr then touch accu;
          touch_stack n stack;
          ((* Nothing to do, see Pushretaddr *))
        | DynamicAppterm (n, _) | PartialAppterm (n, _)
        | SpecialAppterm (n, _) ->
          touch accu;
          touch_stack n stack;
        | StaticAppterm (n, _, ptr) ->
          if env_used ptr then touch accu;
          touch_stack n stack;
        | Return _ ->
          touch accu;
        | Closure (n, ptr) ->
          let new_accu = ind in
          if n = 0 then f new_accu stack (ind + 1) else
            let new_stack = Stk.pop (n - 1) stack in
            let uses_set = IMap.find ptr.pointed.index env_uses in
            let rec g i =
              if i <= n - 1 then (
                if ISet.mem (i + 3) uses_set then
                  touch (Stk.acc i stack);
                g (i + 1)
              )
            in
            g 0;
            if ISet.mem 2 uses_set then touch accu;
            f new_accu new_stack (ind + 1);
        | Closurerec (nf, nv, ptr, ptrs) ->
          let rec mpush_stack i n stk =
            if i = 0 then stk
            else mpush_stack (i - 1) n (Stk.push n stk)
          in
          let new_accu = ind in
          let new_stack1 =
            if nv <> 0 then Stk.pop (nv - 1) stack else stack
          in
          let new_stack2 = mpush_stack nf new_accu new_stack1 in
          if nv = 0 then f new_accu new_stack2 (ind + 1) else
            let uses_set =
              let shift ofs set =
                ISet.fold (fun n acc ->
                  ISet.add (if n = 0 then 0 else n - ofs) acc)
                  set ISet.empty
              in
              let rec g i acc =
                if i >= nf - 1 then acc else
                  let ptri_set =
                    IMap.find ptrs.(i).pointed.index env_uses
                  in
                  let shifted_set = shift (3 * (nf - 2 - i)) ptri_set in
                  let new_acc = ISet.union acc shifted_set in
                  g (i + 1) new_acc
              in
              let ptr_set = IMap.find ptr.pointed.index env_uses in
              let init_acc = shift (3 * (nf - 1)) ptr_set in
              g 0 init_acc
            in
            let rec h i =
              if i <= nv - 1 then (
                if ISet.mem (i + 3) uses_set then
                  touch (Stk.acc i stack);
                h (i + 1)
              )
            in
            h 0;
            if ISet.mem 2 uses_set then touch accu;
            f new_accu new_stack2 (ind + 1);
        | Offsetclosure _ | Getglobal _ | Getglobalfield _ ->
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Setglobal _ ->
          touch accu;
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Atom _ ->
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Makeblock (size, _) | Makefloatblock size ->
          touch accu;
          touch_stack (size - 1) stack;
          let new_accu = ind in
          let new_stack = Stk.pop (size - 1) stack in
          f new_accu new_stack (ind + 1);
        | Getfield _ | Getfloatfield _ ->
          touch accu;
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Setfield _ | Setfloatfield _ ->
          touch accu;
          touch (Stk.acc 0 stack);
          let new_accu = ind in
          let new_stack = Stk.pop 1 stack in
          f new_accu new_stack (ind + 1);
        | Checksignals ->
          f accu stack (ind + 1);
        | Ccall (narg, _) ->
          touch accu;
          touch_stack (narg - 1) stack;
          let new_accu = ind in
          let new_stack = Stk.pop (narg - 1) stack in
          f new_accu new_stack (ind + 1);
        | Pushtrap ptr ->
          f accu stack ptr.pointed.index;
          f accu stack (ind + 1);
        | Raise ->
          touch accu;
        | Poptrap ->
          f accu stack (ind + 1);
        | Const _ ->
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Unapp _ ->
          touch accu;
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Offsetref _ ->
          touch accu;
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Binapp _ ->
          touch accu;
          touch (Stk.acc 0 stack);
          let new_accu = ind in
          let new_stack = Stk.pop 1 stack in
          f new_accu new_stack (ind + 1);
        | Setvectitem | Setstringchar ->
          touch accu;
          touch_stack 2 stack;
          let new_accu = ind in
          let new_stack = Stk.pop 2 stack in
          f new_accu new_stack (ind + 1);
        | Branch ptr ->
          f accu stack ptr.pointed.index;
        | CondBranch cb ->
          touch accu;
          f accu stack (Instr.ptr_of_cond_branch cb).pointed.index;
          f accu stack (ind + 1);
        | Switch (_, _, ptrs) ->
          touch accu;
          Array.iter (fun ptr -> f accu stack ptr.pointed.index) ptrs;
        | Getmethod ->
          touch accu;
          touch (Stk.acc 0 stack);
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Getpubmet (_, _) ->
          touch accu;
          let new_accu = ind in
          let new_stack = Stk.push accu stack in
          f new_accu new_stack (ind + 1);
        | Getdynmet ->
          touch accu;
          touch (Stk.acc 0 stack);
          let new_accu = ind in
          f new_accu stack (ind + 1);
        | Stop ->
          ()

        | Restart -> assert false
        | Grab _ -> assert false
    )
  in
  let args_stack =
    let rec g n acc =
      if n = 0 then acc else g (n - 1) (Stk.push (-1) acc)
    in
    g arity Stk.empty
  in
  f (-1) args_stack 0;
  used
;;

let replace_unused flag body used =
  let f i instr =
    if not used.(i) then
      let new_bc =
        match instr.bc with
          | Acc _ | Envacc _ | Offsetclosure _ | Getglobal _ | Getglobalfield _
          | Atom _ | Makeblock (1, _) | Makefloatblock 1 | Getfield _
          | Getfloatfield _ | Const _ | Unapp _ | Getmethod | Getdynmet -> Const 0

          | Getpubmet (_, _) -> Push

          | Binapp _ -> Pop 1

          | Makeblock (size, _) | Makefloatblock size -> Pop (size - 1)

          | Closure ((0 | 1), _) -> Const 0

          | Closure (n, _) -> Pop (n - 1)

          | Closurerec (nf, nv, _, _) ->
            let delta = nf - (if nv = 0 then 0 else nv - 1) in
            if delta = 0 then Const 0
            else if delta < 0 then Pop (-delta)
            else if delta = 1 then Push
            else instr.bc

          | Push | Pop _ | Assign _ | Pushretaddr _ | DynamicApply _
          | PartialApply _ | StaticApply _ | DynamicAppterm _ | PartialAppterm _
          | SpecialAppterm _ | StaticAppterm _ | Return _ | Setglobal _
          | Setfield _ | Setfloatfield _ | Checksignals | Ccall (_, _)
          | Pushtrap _ | Raise | Poptrap | Offsetref _ | Setvectitem
          | Setstringchar | Branch _ | CondBranch _ | Switch (_, _, _)
          | Stop -> instr.bc

          | Restart -> assert false
          | Grab _ -> assert false
      in
      if new_bc <> instr.bc then (
        body.(i) <- { instr with bc = new_bc };
        flag := true;
      )
  in
  Array.iteri f body;
;;

let compute_env_uses used_map funs =
  let f used_opt acc instr =
    match instr.bc with
      | Envacc n -> ISet.add n acc
      | Offsetclosure _ ->
        begin match used_opt with
          | None -> acc
          | Some used -> if used.(instr.index) then ISet.add 0 acc else acc
        end
      | _ -> acc
  in
  let g fun_id fd acc =
    let used_opt =
      try Some (IMap.find fun_id used_map)
      with Not_found -> None
    in
    IMap.add fun_id (Array.fold_left (f used_opt) ISet.empty fd.body) acc
  in
  let h _ fd acc =
    Array.fold_left (
      fun acc instr ->
        match instr.bc with
          | Closurerec (_, _, ptr, ptrs) ->
            let b =
              let f b ptr =
                b || ISet.mem 0 (IMap.find ptr.pointed.index acc)
              in
              Array.fold_left f (f false ptr) ptrs
            in
            if b then
              let f acc ptr =
                IMap.add ptr.pointed.index
                  (ISet.add 0 (IMap.find ptr.pointed.index acc)) acc
              in
              Array.fold_left f (f acc ptr) ptrs
            else acc
          | _ -> acc
    ) acc fd.body
  in
  let env_uses1 = IMap.fold g funs IMap.empty in
  let env_uses2 = IMap.fold h funs env_uses1 in
  env_uses2
;;

let run funs =
  let flag = ref true in
  Options.verb_start "+ Removing unused closures";
  while !flag do
    flag := false;
    Options.message ".";
    let env_uses1 = compute_env_uses IMap.empty funs in
    let used1 = IMap.map (fun fd -> interp fd.arity fd.body env_uses1) funs in
    let env_uses2 = compute_env_uses used1 funs in
    let f _ fd =
      let used = interp fd.arity fd.body env_uses2 in
      replace_unused flag fd.body used
    in
    IMap.iter f funs;
  done;
  Options.verb_stop ();
;;
