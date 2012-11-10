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

open Printf
open Types
open Tools

let print_arch oc arch =
  match arch with
    | GEN_ARCH -> output_string oc "GEN"
    | NO_ARCH  -> output_string oc "NONE"
    | X86      -> output_string oc "X86"
    | X86_64   -> output_string oc "X86_64"
;;

let print_sigconf oc sigconf =
  match sigconf with
    | Efficient -> output_string oc "EFFICIENT"
    | Reactive -> output_string oc "REACTIVE"
;;

let print_except oc except =
  match except with
    | Setjmp -> output_string oc "SETJMP"
    | Trycatch -> output_string oc "TRY_CATCH"
;;

let print_index oc index =
  let string_of_sname sname =
    match sname with
      | Code -> "CODE" | Dlpt -> "DLPT" | Dlls -> "DLLS" | Prim -> "PRIM"
      | Data -> "DATA" | Symb -> "SYMB" | Crcs -> "CRCS" | Dbug -> "DBUG"
  in
  let print_descr (name, offset, length) =
    Printf.fprintf oc "%s %8d %8d\n" (string_of_sname name) offset length
  in
  Printf.fprintf oc "\
*******************\n\
*****  Index  *****\n\
*******************\n\
\n\
<name>    <@>   <size>\n\
";
  List.iter print_descr index
;;

let rec string_of_bc bc =
  match bc with
    | Acc n                 -> sprintf "ACC %d" n
    | Push                  -> sprintf "PUSH"
    | Pop n                 -> sprintf "POP %d" n
    | Assign n              -> sprintf "ASSIGN %d" n
    | Envacc n              -> sprintf "ENVACC %d" n
    | Pushretaddr ptr       -> sprintf "PUSHRETADDR %d" ptr.pointed.index
    | DynamicApply n        -> sprintf "DYNAMIC_APPLY %d" n
    | StaticApply (n, ptr)  -> sprintf "STATIC_APPLY %d f%d" n ptr.pointed.index
    | PartialApply n        -> sprintf "PARTIAL_APPLY %d" n
    | DynamicAppterm (n, s) -> sprintf "DYNAMIC_APPTERM %d %d" n s
    | StaticAppterm (n, s, ptr) ->
      sprintf "STATIC_APPTERM %d %d f%d" n s ptr.pointed.index
    | PartialAppterm (n, s) -> sprintf "PARTIAL_APPTERM %d %d" n s
    | SpecialAppterm (n, s) -> sprintf "SPECIAL_APPTERM %d %d" n s
    | Return n              -> sprintf "RETURN %d" n
    | Restart               -> sprintf "RESTART"
    | Grab n                -> sprintf "GRAB %d" n
    | Closure (n,ptr)       -> sprintf "CLOSURE %d %d" n ptr.pointed.index
    | Closurerec (f, v, o, t) ->
      let b = Buffer.create 16 in
      bprintf b "CLOSUREREC %d %d %d [" f v o.pointed.index;
      Array.iter (fun ptr -> bprintf b " %d " ptr.pointed.index) t;
      bprintf b "]";
      Buffer.contents b
    | Offsetclosure (-2)     -> sprintf "OFFSETCLOSUREM2"
    | Offsetclosure n        -> sprintf "OFFSETCLOSURE %d" n
    | Getglobal n            -> sprintf "GETGLOBAL %d" n
    | Getglobalfield (n,p)   -> sprintf "GETGLOBALFIELD %d %d"n p
    | Setglobal n       -> sprintf "SETGLOBAL %d" n
    | Atom n            -> sprintf "ATOM %d" n
    | Makeblock (n,t)   -> sprintf "MAKEBLOCK %d %d" n t
    | Makefloatblock n  -> sprintf "MAKEFLOATBLOCK %d" n
    | Getfield n        -> sprintf "GETFIELD %d" n
    | Getfloatfield n   -> sprintf "GETFLOATFIELD %d" n
    | Setfield n        -> sprintf "SETFIELD %d" n
    | Setfloatfield n   -> sprintf "SETFLOATFIELD %d" n
    | Setvectitem       -> sprintf "SETVECTITEM"
    | Setstringchar     -> sprintf "SETSTRINGCHAR"
    | Branch ptr        -> sprintf "BRANCH %d" ptr.pointed.index
    | Switch (size_long, size_tag, tab) ->
      let b = Buffer.create 16 in
      bprintf b "SWITCH %d %d [" size_tag size_long;
      Array.iter
        (fun ptr -> bprintf b " %d " ptr.pointed.index) tab;
      bprintf b "]";
      Buffer.contents b
    | Pushtrap ptr      -> sprintf "PUSHTRAP %d" ptr.pointed.index
    | Poptrap           -> sprintf "POPTRAP"
    | Raise             -> sprintf "RAISE"
    | Checksignals      -> sprintf "CHECKSIGNALS"
    | Ccall (n, ind)    -> sprintf "CCALL %d %d" n ind
    | Const n           -> sprintf "CONST %d" n
    | Offsetref ofs     -> sprintf "OFFSETREF %d" ofs
    | Unapp unop        -> string_of_unop unop
    | Binapp binop      -> string_of_binop binop
    | CondBranch cb     -> string_of_cond_branch cb
    | Getmethod         -> sprintf "GETMETHOD"
    | Getpubmet (v,ofs) -> sprintf "GETPUBMET %d %d" v ofs
    | Getdynmet         -> sprintf "GETDYNMET"
    | Stop              -> sprintf "STOP"

and string_of_unop unop =
  match unop with
    | Boolnot           -> sprintf "BOOLNOT"
    | Offsetint ofs     -> sprintf "OFFSETINT %d" ofs
    | Negint            -> sprintf "NEGINT"
    | Isint             -> sprintf "ISINT"
    | Vectlength        -> sprintf "VECTLENGTH"

and string_of_binop binop =
  match binop with
    | Addint            -> sprintf "ADDINT"
    | Subint            -> sprintf "SUBINT"
    | Mulint            -> sprintf "MULINT"
    | Divint            -> sprintf "DIVINT"
    | Modint            -> sprintf "MODINT"
    | Andint            -> sprintf "ANDINT"
    | Orint             -> sprintf "ORINT"
    | Xorint            -> sprintf "XORINT"
    | Lslint            -> sprintf "LSLINT"
    | Lsrint            -> sprintf "LSRINT"
    | Asrint            -> sprintf "ASRINT"
    | Eq                -> sprintf "EQ"
    | Neq               -> sprintf "NEQ"
    | Ltint             -> sprintf "LTINT"
    | Leint             -> sprintf "LEINT"
    | Gtint             -> sprintf "GTINT"
    | Geint             -> sprintf "GEINT"
    | Ultint            -> sprintf "ULTINT"
    | Ugeint            -> sprintf "UGEINT"
    | Getvectitem       -> sprintf "GETVECTITEM"
    | Getstringchar     -> sprintf "GETSTRINGCHAR"

and string_of_cond_branch cond_branch =
  match cond_branch with
    | Branchif ptr      -> sprintf "BRANCHIF %d" ptr.pointed.index
    | Branchifnot ptr   -> sprintf "BRANCHIFNOT %d" ptr.pointed.index
    | Beq (v,ptr)       -> sprintf "BEQ %d %d" v ptr.pointed.index
    | Bneq (v,ptr)      -> sprintf "BNEQ %d %d" v ptr.pointed.index
    | Bltint (v,ptr)    -> sprintf "BLINT %d %d" v ptr.pointed.index
    | Bleint (v,ptr)    -> sprintf "BLEINT %d %d" v ptr.pointed.index
    | Bgtint (v,ptr)    -> sprintf "BGTINT %d %d" v ptr.pointed.index
    | Bgeint (v,ptr)    -> sprintf "BGEINT %d %d" v ptr.pointed.index
    | Bultint (v,ptr)   -> sprintf "BULTINT %d %d" v ptr.pointed.index
    | Bugeint (v,ptr)   -> sprintf "BUGEINT %d %d" v ptr.pointed.index
;;

let print_instr oc instr =
  fprintf oc "%-5d   @ = %-5d    %s\n" (instr.index) (instr.addr)
    (string_of_bc instr.bc);
;;

let print_code oc code =
  Printf.fprintf oc "\n\
******************\n\
***  Bytecode  ***\n\
******************\n\
\n\
";
  Array.iter (print_instr oc) code;
;;

let print_prim oc prim =
  Printf.fprintf oc "\n\
*******************\n\
***  Externals  ***\n\
*******************\n\
\n\
";
  Array.iteri (Printf.fprintf oc "%-3d   %s\n") prim
;;

let rec print_orig oc = function
  | OGlobal n ->
    Printf.fprintf oc "OGlobal %d" n;
  | OGlobalField (n, p) ->
    Printf.fprintf oc "OGlobalField (%d, %d)" n p;
  | OClosure (ptr, origs) ->
    Printf.fprintf oc "OClosure (%d, [ " ptr.pointed.index;
    Array.iter (Printf.fprintf oc "%a " print_orig) origs;
    Printf.fprintf oc "])";
  | OClosurerec (ptrs, origs, ofs) ->
    Printf.fprintf oc "OClosurerec ([ ";
    Array.iter (fun ptr -> Printf.fprintf oc "%d " ptr.pointed.index) ptrs;
    Printf.fprintf oc "], [ ";
    Array.iter (Printf.fprintf oc "%a " print_orig) origs;
    Printf.fprintf oc "], %d)" ofs;
  | OUnknown ->
    Printf.fprintf oc "OUnknown";
;;

let rec print_glob oc = function
  | GClosure ptr ->
    Printf.fprintf oc "GClosure %d" ptr.pointed.index
  | GClosurerec (ptrs, ofs) ->
    Printf.fprintf oc "GClosurerec ([ ";
    Array.iter (fun ptr -> Printf.fprintf oc "%d " ptr.pointed.index) ptrs;
    Printf.fprintf oc "], %d)" ofs;
  | GModule globs ->
    Printf.fprintf oc "GModule [ ";
    Array.iter (Printf.fprintf oc "%a " print_glob) globs;
    Printf.fprintf oc "]\n";
  | GUnknown ->
    Printf.fprintf oc "GUnknown"

let print_desc oc desc =
  Printf.fprintf oc "\naccu: %a\n" print_orig desc.accu;
  Stk.print print_orig oc desc.stack;
;;

let print_location oc {
  loc_start = {
    Lexing.pos_fname = fname;
    Lexing.pos_lnum = start_lnum;
    Lexing.pos_cnum = start_cnum;
    Lexing.pos_bol = start_bol;
  };
  loc_end = {
    Lexing.pos_fname = _;
    Lexing.pos_lnum = _;
    Lexing.pos_cnum = end_cnum;
    Lexing.pos_bol = _;
  };
  loc_ghost = ghost;
} =
  if ghost then Printf.fprintf oc "?"
  else
    Printf.fprintf oc "File %S, line %d, characters %d-%d" fname start_lnum
      (start_cnum - start_bol) (end_cnum - start_bol)
;;

let print_dbug oc dbug =
  let f n loc = Printf.fprintf oc "  %d: %a\n" n print_location loc in
  Printf.fprintf oc "{\n";
  IMap.iter f dbug;
  Printf.fprintf oc "}\n";
;;

let print_comment indent newline funs dbug oc fun_id =
  try
    let fun_desc = IMap.find fun_id funs in
    let loc = IMap.find fun_desc.body.(0).addr dbug in
    Printf.fprintf oc "%s// %a" indent print_location loc;
    if newline then Printf.fprintf oc "\n";
  with Not_found -> ()
;;

let print_val_desc oc vd =
  match vd with
    | VConst n -> Printf.fprintf oc "Const(%d)" n
    | VGlob n -> Printf.fprintf oc "Glob(%d)" n
    | VGlobField (n, p) -> Printf.fprintf oc "GlobField(%d, %d)" n p
    | VAtom t  -> Printf.fprintf oc "Atom(%d)" t
    | VClsr o -> Printf.fprintf oc "Clsr(%d)" o
    | VEnv n -> Printf.fprintf oc "Env(%d)" n
    | VArg n -> Printf.fprintf oc "Arg(%d)" n
    | VPtr ptr -> Printf.fprintf oc "Ptr(%d)" ptr.pointed.index
    | VCell -> Printf.fprintf oc "Cell"
;;

let print_ids_infos oc ids_infos =
  let print_body_states oc body states idvd_map =
    let pi instr = Printf.fprintf oc "%s\n" (string_of_bc instr.bc) in
    let ps { accu = accu ; stack = stack } =
      Printf.fprintf oc "        %a[%d]" print_val_desc
        (IMap.find accu idvd_map) accu;
      Printf.fprintf oc " [ ";
      Stk.iter
        (fun id -> Printf.fprintf oc "%a[%d] " print_val_desc
          (IMap.find id idvd_map) id) stack;
      Printf.fprintf oc "]\n";
    in
    let pso so =
      match so with
        | None -> Printf.fprintf oc "        --\n";
        | Some s -> ps s;
    in
    let len = Array.length body in
    for i = 0 to len - 1 do
      pso states.(i);
      Printf.fprintf oc "%d: " i;
      pi body.(i);
    done;
    Printf.fprintf oc "\n";
  in
  let print_val_desc_map oc idvd_map ptr_set read_set =
    let f id vd =
      if ISet.mem id ptr_set then Printf.fprintf oc "P "
      else if ISet.mem id read_set then Printf.fprintf oc "  "
      else Printf.fprintf oc "- ";
      Printf.fprintf oc "%d: %a\n" id print_val_desc vd;
    in
    IMap.iter f idvd_map;
    Printf.fprintf oc "\n";
  in
  let f _ ids_info =
    print_body_states oc ids_info.fun_desc.body ids_info.states
      ids_info.idvd_map;
    print_val_desc_map oc ids_info.idvd_map ids_info.ptr_set ids_info.read_set;
    Printf.fprintf oc "\n\n";
  in
  IMap.iter f ids_infos
;;
