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

let interp arity body =
  let rec pop_somes n bufsize stkmap =
    if n = 0 then stkmap else (
      assert (Stk.acc 0 stkmap = Some (bufsize - 1));
      pop_somes (n - 1) (bufsize - 1) (Stk.pop 1 stkmap)
    )
  in
  let rec pop_nones n stkmap =
    if n = 0 then stkmap else (
      assert (Stk.acc 0 stkmap = None);
      pop_nones (n - 1) (Stk.pop 1 stkmap)
    )
  in
  let rec push_somes n bufsize stkmap =
    if n = 0 then stkmap else
      push_somes (n - 1) (bufsize + 1) (Stk.push (Some bufsize) stkmap)
  in
  let rec push_nones n stkmap =
    if n = 0 then stkmap else push_nones (n - 1) (Stk.push None stkmap)
  in
  let len = Array.length body in
  let stkmaps = Array.make len None in
  let rec f bufsize stkmap ind =
    assert (ind >= 0 && ind < len);
    match stkmaps.(ind) with
      | Some (bufsize', stkmap') ->
        assert (bufsize' = bufsize && stkmap' = stkmap);
      | None ->
        stkmaps.(ind) <- Some (bufsize, stkmap);
        match body.(ind).bc with
          | Acc n ->
            assert (Stk.acc n stkmap <> None);
            f bufsize stkmap (ind + 1)
          | Push ->
            f (bufsize + 1) (Stk.push (Some bufsize) stkmap) (ind + 1)
          | Pop n ->
            let rec g new_bufsize new_stkmap n =
              if n = 0 then (new_bufsize, new_stkmap) else
                match Stk.acc 0 new_stkmap with
                  | None -> g new_bufsize (Stk.pop 1 new_stkmap) (n - 1)
                  | Some old_bufsize ->
                    assert (old_bufsize = new_bufsize - 1);
                    g old_bufsize (Stk.pop 1 new_stkmap) (n - 1)
            in
            let (new_bufsize, new_stkmap) = g bufsize stkmap n in
            f new_bufsize new_stkmap (ind + 1)
          | Assign n ->
            assert (Stk.acc n stkmap <> None);
            f bufsize stkmap (ind + 1)
          | Envacc _ ->
            f bufsize stkmap (ind + 1)
          | Pushretaddr ptr ->
            f bufsize stkmap ptr.pointed.index;
            let new_stkmap1 = push_nones 2 stkmap in
            let new_stkmap2 = Stk.push (Some bufsize) new_stkmap1 in
            f (bufsize + 1) new_stkmap2 (ind + 1)
          | DynamicApply ((1|2|3) as n) | StaticApply ((1|2|3) as n, _)
          | PartialApply ((1|2|3) as n) ->
            let new_stkmap = pop_somes n bufsize stkmap in
            f (bufsize - n) new_stkmap (ind + 1)
          | DynamicApply _ | StaticApply _ | PartialApply _ ->
            ((* Nothing to do here, see Pushretaddr *))
          | Closure (nv, _) ->
            let (new_bufsize, new_stkmap) =
              if nv = 0 then (bufsize, stkmap)
              else (bufsize - (nv - 1), pop_somes (nv - 1) bufsize stkmap)
            in
            f new_bufsize new_stkmap (ind + 1)
          | Closurerec (nf, nv, _, _) ->
            let (new_bufsize1, new_stkmap1) =
              if nv = 0 then (bufsize, stkmap)
              else (bufsize - (nv - 1), pop_somes (nv - 1) bufsize stkmap)
            in
            let new_stkmap2 = push_somes nf new_bufsize1 new_stkmap1 in
            let new_bufsize2 = new_bufsize1 + nf in
            f new_bufsize2 new_stkmap2 (ind + 1)
          | Offsetclosure _ | Getglobal _ | Getglobalfield (_, _)
          | Setglobal _ | Atom _ | Getfield _ | Getfloatfield _ ->
            f bufsize stkmap (ind + 1)
          | Makeblock (size, _) | Makefloatblock size ->
            f (bufsize - (size - 1)) (pop_somes (size - 1) bufsize stkmap)
              (ind + 1)
          | Setfield _ | Setfloatfield _ ->
            f (bufsize - 1) (pop_somes 1 bufsize stkmap) (ind + 1)
          | Pushtrap ptr ->
            f bufsize stkmap ptr.pointed.index;
            let new_stkmap1 = push_nones 3 stkmap in
            let new_stkmap2 = Stk.push (Some bufsize) new_stkmap1 in
            f (bufsize + 1) new_stkmap2 (ind + 1)
          | Poptrap ->
            let new_stkmap1 = pop_somes 1 bufsize stkmap in
            let new_stkmap2 = pop_nones 3 new_stkmap1 in
            f (bufsize - 1) new_stkmap2 (ind + 1)
          | Checksignals ->
            f bufsize stkmap (ind + 1)
          | Ccall (narg, _) ->
            f (bufsize - (narg - 1)) (pop_somes (narg - 1) bufsize stkmap)
              (ind + 1)
          | Const _ | Offsetref _ | Unapp _ ->
            f bufsize stkmap (ind + 1)
          | Binapp _ ->
            f (bufsize - 1) (pop_somes 1 bufsize stkmap) (ind + 1)
          | Setvectitem | Setstringchar ->
            f (bufsize - 2) (pop_somes 2 bufsize stkmap) (ind + 1)
          | Branch ptr ->
            f bufsize stkmap ptr.pointed.index
          | CondBranch cb ->
            f bufsize stkmap (Instr.ptr_of_cond_branch cb).pointed.index;
            f bufsize stkmap (ind + 1)
          | Switch (_, _, ptrs) ->
            Array.iter (fun ptr -> f bufsize stkmap ptr.pointed.index) ptrs
          | Getmethod ->
            f bufsize stkmap (ind + 1)
          | Getpubmet (_, _) ->
            f (bufsize + 1) (Stk.push (Some bufsize) stkmap) (ind + 1)
          | Getdynmet ->
            f bufsize stkmap (ind + 1)

          | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _
          | SpecialAppterm _ | Return _ | Raise | Stop -> ()

          | Restart -> assert false
          | Grab _ -> assert false
  in
  let args_stkmap = push_somes arity 0 Stk.empty in
  f arity args_stkmap 0;
  stkmaps
;;

let remap_stack_access body stkmaps =
  let set_on_image ind instr f n =
    match stkmaps.(ind) with
      | None -> ()
      | Some (bufsize, stkmap) ->
        match Stk.acc n stkmap with
          | None -> assert false
          | Some ofs -> instr.bc <- f (bufsize - ofs - 1)
  in
  let f ind instr =
    match instr.bc with
      | Acc n -> set_on_image ind instr (fun m -> Acc m) n
      | Assign n -> set_on_image ind instr (fun m -> Assign m) n
      | Pop n ->
        begin match stkmaps.(ind) with
          | None -> ()
          | Some (_, stkmap) ->
            let rec g cnt i =
              if i = n then cnt else
                match Stk.acc i stkmap with
                  | None -> g cnt (i + 1)
                  | Some _ -> g (cnt + 1) (i + 1)
            in
            instr.bc <- Pop (g 0 0)
        end
      | Return _ ->
        begin match stkmaps.(ind) with
          | None -> ()
          | Some (bufsize, _) -> instr.bc <- Return bufsize
        end
      | Push | Envacc _ | Pushretaddr _ | DynamicApply _ | StaticApply _
      | PartialApply _ | DynamicAppterm _ | StaticAppterm _ | PartialAppterm _
      | SpecialAppterm _ | Closure _ | Closurerec _ | Offsetclosure _
      | Getglobal _ | Getglobalfield _ | Setglobal _ | Atom _ | Getfield _
      | Getfloatfield _ | Makeblock _ | Makefloatblock _ | Setfield _
      | Setfloatfield _ | Pushtrap _ | Poptrap | Raise | Checksignals | Ccall _
      | Const _ | Offsetref _ | Setvectitem | Setstringchar | Branch _
      | Switch _ | Getmethod | Getpubmet _ | Getdynmet | Unapp _ | Binapp _
      | CondBranch _ | Stop -> ()

      | Restart -> assert false
      | Grab _ -> assert false
  in
  Array.iteri f body
;;

let remap_stack funs =
  let f arity body =
    let stkmaps = interp arity body in
    remap_stack_access body stkmaps
  in
  Options.verb_start "+ Remapping stack access...";
  IMap.iter (fun _ fd -> f fd.arity fd.body) funs;
  Options.verb_stop ();
;;
