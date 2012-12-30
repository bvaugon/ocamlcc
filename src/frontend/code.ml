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
    if bc_ind < 0 || bc_ind >= nb_bc then
      failwith "invalid bytecode executable file (bad code pointer offset)";
    match indirect.(bc_ind) with
      | None ->
        failwith "invalid bytecode executable file (bad code pointer offset)"
      | Some instr ->
        instr
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
    with Not_found ->
      failwith "invalid bytecode executable file (CODE section not found)"
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
        | ws -> failwith (Printf.sprintf "unsupported architecture: \
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
