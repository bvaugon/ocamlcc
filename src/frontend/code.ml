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

let compute_ptrs code =
  let nb_instr = Array.length code in
  let nb_bc = code.(nb_instr - 1).addr + 1 in
  let indirect = Array.make nb_bc None in
  let grep_instr instr = indirect.(instr.addr) <- Some instr in
  let search bc_ind =
    if bc_ind < 0 || bc_ind >= nb_bc then failwith "invalid offset";
    match indirect.(bc_ind) with
      | None -> failwith "invalid offset";
      | Some instr -> instr
  in
  let affect_ptr instr =
    let update_pointed delta ptr =
      let pointed = search (instr.addr + delta + ptr.ofs) in
      ptr.pointed <- pointed;
      pointed.is_pointed <- true;
    in
    match instr.bc with
      | Pushretaddr ptr | Branch ptr | (CondBranch (Branchif ptr))
      | CondBranch (Branchifnot ptr) | Pushtrap ptr ->
        update_pointed 1 ptr;

      | Closure (_, ptr) ->
        update_pointed 2 ptr

      | CondBranch (Beq (_, ptr) | Bneq (_, ptr) | Bltint (_, ptr)
                       | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr)
                       | Bultint (_, ptr) | Bugeint (_, ptr)) ->
        update_pointed 2 ptr;

      | Closurerec (_, _, ptr, tab) ->
        update_pointed 3 ptr;
        Array.iter (update_pointed 3) tab;

      | Switch (_, _, tab) ->
        Array.iter (update_pointed 2) tab

      | _ -> ();
  in
  (* WARNING: instruction must be grep last to first (addr collision) *)
  for i = Array.length code - 1 downto 0 do grep_instr code.(i) done;
  Array.iter affect_ptr code;
;;

let parse ic index =
  let (offset, length) =
    try Index.find_section index Code
    with Not_found -> failwith "code section not found"
  in
  seek_in ic offset;
  let cpt = ref 0 in
  let nb_bc = length lsr 2 in
  let read =
    let buf4 = String.create 4 in
    fun () ->
      incr cpt;
      if !cpt > nb_bc then raise End_of_file;
      really_input ic buf4 0 4;
      let res =
        (int_of_char buf4.[0]) lor (int_of_char buf4.[1] lsl 8) lor
          (int_of_char buf4.[2] lsl 16) lor (int_of_char buf4.[3] lsl 24)
      in
      match Sys.word_size with
        | 32 -> res
        | 64 -> (res lsl 32) asr 32
        | ws -> failwith (Printf.sprintf "Unsupported architecture: \
word size is %d" ws)
  in
  let rec f acc =
    let addr = !cpt in
    match
      try Some (Instr.parse read)
      with End_of_file -> None
    with
      | Some bcs ->
        let make_instr bc = {
          addr = addr;
          index = 0;
          bc = bc;
          is_pointed = false;
        } in
        let instrs = List.rev_map make_instr bcs in
        f (instrs @ acc)
      | None -> acc
  in
  let code = Array.of_list (f []) in
  let nb_instr = Array.length code in
  for i = 0 to nb_instr / 2 - 1 do
    let s = nb_instr - i - 1 in
    let tmp = code.(i) in
    code.(i) <- code.(s);
    code.(s) <- tmp;
    code.(i).index <- i;
    code.(s).index <- s;
  done;
  code.(nb_instr / 2).index <- nb_instr / 2;
  compute_ptrs code;
  code
;;

let compare body1 body2 =
  let len1 = Array.length body1 and len2 = Array.length body2 in
  let h ptrs = Array.map (fun ptr -> ptr.pointed.index) ptrs in
  let g f i x y =
    match compare x y with
      | 0 -> f (i + 1)
      | res -> res
  in
  let rec f i =
    if i = len1 then 0 else
      let bc1 = body1.(i).bc and bc2 = body2.(i).bc in
      let hc1 = Instr.hcode_of_bc bc1 and hc2 = Instr.hcode_of_bc bc2 in
      if hc1 < hc2 then 1 else if hc2 < hc1 then -1 else
          match (bc1, bc2) with
            | (Acc n1, Acc n2)
            | (Pop n1, Pop n2)
            | (Assign n1, Assign n2)
            | (Envacc n1, Envacc n2)
            | (DynamicApply n1, DynamicApply n2)
            | (PartialApply n1, PartialApply n2)
            | (Return n1, Return n2)
            | (Grab n1, Grab n2)
            | (Offsetclosure n1, Offsetclosure n2)
            | (Getglobal n1, Getglobal n2)
            | (Setglobal n1, Setglobal n2)
            | (Atom n1, Atom n2)
            | (Makefloatblock n1, Makefloatblock n2)
            | (Getfield n1, Getfield n2)
            | (Getfloatfield n1, Getfloatfield n2)
            | (Setfield n1, Setfield n2)
            | (Setfloatfield n1, Setfloatfield n2)
            | (Const n1, Const n2)
            | (Offsetref n1, Offsetref n2) ->
              if n1 = n2 then f (i + 1) else compare n1 n2

            | (Push, Push)
            | (Restart, Restart)
            | (Setvectitem, Setvectitem)
            | (Setstringchar, Setstringchar)
            | (Poptrap, Poptrap)
            | (Raise, Raise)
            | (Checksignals, Checksignals)
            | (Getmethod, Getmethod)
            | (Getdynmet, Getdynmet)
            | (Stop, Stop) ->
              f (i + 1)

            | (StaticAppterm (n1, p1, ptr1), StaticAppterm (n2, p2, ptr2)) ->
              g f i (n1, p1, ptr1.pointed.index) (n2, p2, ptr2.pointed.index)

            | (CondBranch (Beq (n1, ptr1)), CondBranch (Beq (n2, ptr2)))
            | (CondBranch (Bneq (n1, ptr1)), CondBranch (Bneq (n2, ptr2)))
            | (CondBranch (Bltint (n1, ptr1)), CondBranch (Bltint (n2, ptr2)))
            | (CondBranch (Bleint (n1, ptr1)), CondBranch (Bleint (n2, ptr2)))
            | (CondBranch (Bgtint (n1, ptr1)), CondBranch (Bgtint (n2, ptr2)))
            | (CondBranch (Bgeint (n1, ptr1)), CondBranch (Bgeint (n2, ptr2)))
            | (CondBranch (Bultint (n1, ptr1)), CondBranch (Bultint (n2, ptr2)))
            | (CondBranch (Bugeint (n1, ptr1)), CondBranch (Bugeint (n2, ptr2)))
            | (StaticApply (n1, ptr1), StaticApply (n2, ptr2))
            | (Closure (n1, ptr1), Closure (n2, ptr2)) ->
              g f i (n1, ptr1.pointed.index) (n2, ptr2.pointed.index)

            | (Closurerec (n1, p1, ptr1, ptrs1),
               Closurerec (n2, p2, ptr2, ptrs2)) ->
              g f i (n1, p1, ptr1.pointed.index, h ptrs1)
                (n2, p2, ptr2.pointed.index, h ptrs2)

            | (Pushretaddr ptr1, Pushretaddr ptr2)
            | (Pushtrap ptr1, Pushtrap ptr2)
            | (CondBranch (Branchif ptr1), CondBranch (Branchif ptr2))
            | (CondBranch (Branchifnot ptr1), CondBranch (Branchifnot ptr2))
            | (Branch ptr1, Branch ptr2) ->
              g f i ptr1.pointed.index ptr2.pointed.index

            | (Switch (n1, p1, ptrs1), Switch (n2, p2, ptrs2)) ->
              g f i (n1, p1, h ptrs1) (n2, p2, h ptrs2)

            | (Unapp u1, Unapp u2) ->
              g f i u1 u2

            | (Binapp b1, Binapp b2) ->
              g f i b1 b2

            | (DynamicAppterm (n1, p1), DynamicAppterm (n2, p2))
            | (PartialAppterm (n1, p1), PartialAppterm (n2, p2))
            | (SpecialAppterm (n1, p1), SpecialAppterm (n2, p2))
            | (Getglobalfield (n1, p1), Getglobalfield (n2, p2))
            | (Makeblock (n1, p1), Makeblock (n2, p2))
            | (Ccall (n1, p1), Ccall (n2, p2))
            | (Getpubmet (n1, p1), Getpubmet (n2, p2)) ->
              g f i (n1, p1) (n2, p2)

            | _ ->
              assert false
  in
  if len1 < len2 then 1 else if len2 < len1 then -1 else f 0
;;
