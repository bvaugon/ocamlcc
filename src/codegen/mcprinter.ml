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
open Macroc
open Types

let rec print_instr oc instr =
  match instr with
    | IAffect (v, e) -> fprintf oc "  %a = %a;\n" print_lvalue v print_expr e
    | IMacro macro -> fprintf oc "  %a;\n" print_macro macro
    | ITrace trace -> fprintf oc "  %a;\n" print_trace trace
    | ISwitch switch -> print_switch oc switch
    | ILabel label -> fprintf oc " %a:\n" print_label label

and print_macro oc macro =
  match macro with
    | MOVE (src, dst) ->
      fprintf oc "MOVE(%a, %a)" print_expr src print_lvalue dst
    | OFFSETINT (n, src, dst) ->
      fprintf oc "OFFSETINT(%d, %a, %a)" n print_expr src print_lvalue
        dst
    | BOOLNOT (src, dst) ->
      fprintf oc "BOOLNOT(%a, %a)" print_expr src print_lvalue dst
    | NEGINT (src, dst) ->
      fprintf oc "NEGINT(%a, %a)" print_expr src print_lvalue dst
    | ISINT (src, dst) ->
      fprintf oc "ISINT(%a, %a)" print_expr src print_lvalue dst
    | VECTLENGTH (src, dst) ->
      fprintf oc "VECTLENGTH(%a, %a)" print_expr src print_lvalue dst
    | ADDINT (op1, op2, dst) ->
      fprintf oc "ADDINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | SUBINT (op1, op2, dst) ->
      fprintf oc "SUBINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | MULINT (op1, op2, dst) ->
      fprintf oc "MULINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | DIVINT (op1, op2, dst, frame_sz) ->
      fprintf oc "DIVINT(%a, %a, %a, %d)" print_expr op1 print_expr op2
        print_dst dst frame_sz
    | MODINT (op1, op2, dst, frame_sz) ->
      fprintf oc "MODINT(%a, %a, %a, %d)" print_expr op1 print_expr op2
        print_dst dst frame_sz
    | ANDINT (op1, op2, dst) ->
      fprintf oc "ANDINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | ORINT (op1, op2, dst) ->
      fprintf oc "ORINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | XORINT (op1, op2, dst) ->
      fprintf oc "XORINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | LSLINT (op1, op2, dst) ->
      fprintf oc "LSLINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | LSRINT (op1, op2, dst) ->
      fprintf oc "LSRINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | ASRINT (op1, op2, dst) ->
      fprintf oc "ASRINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | EQ (op1, op2, dst) ->
      fprintf oc "EQ(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | NEQ (op1, op2, dst) ->
      fprintf oc "NEQ(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | LTINT (op1, op2, dst) ->
      fprintf oc "LTINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | LEINT (op1, op2, dst) ->
      fprintf oc "LEINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | GTINT (op1, op2, dst) ->
      fprintf oc "GTINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | GEINT (op1, op2, dst) ->
      fprintf oc "GEINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | ULTINT (op1, op2, dst) ->
      fprintf oc "ULTINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | UGEINT (op1, op2, dst) ->
      fprintf oc "UGEINT(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | GETVECTITEM (op1, op2, dst) ->
      fprintf oc "GETVECTITEM(%a, %a, %a)" print_expr op1 print_expr op2
        print_lvalue dst
    | GETSTRINGCHAR (op1, op2, dst) ->
      fprintf oc "GETSTRINGCHAR(%a, %a, %a)" print_expr op1 print_expr
        op2 print_lvalue dst
    | BRANCH lab ->
      fprintf oc "BRANCH(%a)" print_label lab
    | BRANCHIF (src, lab) ->
      fprintf oc "BRANCHIF(%a, %a)" print_expr src print_label lab
    | BRANCHIFNOT (src, lab) ->
      fprintf oc "BRANCHIFNOT(%a, %a)" print_expr src print_label lab
    | BEQ (n, src, lab) ->
      fprintf oc "BEQ(%d, %a, %a)" n print_expr src print_label lab
    | BNEQ (n, src, lab) ->
      fprintf oc "BNEQ(%d, %a, %a)" n print_expr src print_label lab
    | BLTINT (n, src, lab) ->
      fprintf oc "BLTINT(%d, %a, %a)" n print_expr src print_label lab
    | BLEINT (n, src, lab) ->
      fprintf oc "BLEINT(%d, %a, %a)" n print_expr src print_label lab
    | BGTINT (n, src, lab) ->
      fprintf oc "BGTINT(%d, %a, %a)" n print_expr src print_label lab
    | BGEINT (n, src, lab) ->
      fprintf oc "BGEINT(%d, %a, %a)" n print_expr src print_label lab
    | BULTINT (n, src, lab) ->
      fprintf oc "BULTINT(%d, %a, %a)" n print_expr src print_label lab
    | BUGEINT (n, src, lab) ->
      fprintf oc "BUGEINT(%d, %a, %a)" n print_expr src print_label lab
    | DYNAMIC_APPLY (nargs, cfun_nargs, curr_fsz, next_fsz, dst, env, args) ->
      fprintf oc "DYNAMIC_APPLY(%d, %d, %d, %d, %a, %a)" nargs cfun_nargs
        curr_fsz next_fsz print_lvalue_opt dst print_env_args (env, args, false)
    | PARTIAL_APPLY (nargs, cfun_nargs, curr_fsz, next_fsz, dst, env, args) ->
      fprintf oc "PARTIAL_APPLY(%d, %d, %d, %d, %a, %a)" nargs cfun_nargs
        curr_fsz next_fsz print_lvalue_opt dst print_env_args (env, args, false)
    | STATIC_APPLY (nargs, cfun_nargs, curr_fsz, next_fsz, dst, f, env, args) ->
      fprintf oc "STATIC_APPLY(%d, %d, %d, %d, %a, %a, %a)" nargs
        cfun_nargs curr_fsz next_fsz print_lvalue_opt dst print_mlfun f
        print_env_args (env, args, true)
    | STATIC_NOTC_APPLY (nargs, cfun_nargs, curr_fsz, next_fsz, dst, f,
                         env, args) ->
      fprintf oc "STATIC_NOTC_APPLY(%d, %d, %d, %d, %a, %a, %a)" nargs
        cfun_nargs curr_fsz next_fsz print_lvalue_opt dst print_mlfun f
        print_env_args (env, args, true)
    | DYNAMIC_STANDARD_APPTERM (nargs, cfun_nargs, curr_fsz, env, args) ->
      fprintf oc "DYNAMIC_STANDARD_APPTERM(%d, %d, %d, %a)" nargs cfun_nargs
        curr_fsz print_env_args (env, args, false)
    | DYNAMIC_SPECIAL_APPTERM (nargs, cfun_nargs, curr_fsz, env, args) ->
      fprintf oc "DYNAMIC_SPECIAL_APPTERM(%d, %d, %d, %a)" nargs cfun_nargs
        curr_fsz print_env_args (env, args, false)
    | PARTIAL_STANDARD_APPTERM (nargs, cfun_nargs, curr_fsz, env, args) ->
      fprintf oc "PARTIAL_STANDARD_APPTERM(%d, %d, %d, %a)" nargs cfun_nargs
        curr_fsz print_env_args (env, args, false)
    | PARTIAL_SPECIAL_APPTERM (nargs, cfun_nargs, curr_fsz, env, args) ->
      fprintf oc "PARTIAL_SPECIAL_APPTERM(%d, %d, %d, %a)" nargs cfun_nargs
        curr_fsz print_env_args (env, args, false)
    | STATIC_STANDARD_APPTERM (nargs, cfun_nargs, f, env, args) ->
      fprintf oc "STATIC_STANDARD_APPTERM(%d, %d, %a, %a)" nargs cfun_nargs
        print_mlfun f print_env_args (env, args, true)
    | STATIC_SPECIAL_APPTERM (nargs, cfun_nargs, f, env, args) ->
      fprintf oc "STATIC_SPECIAL_APPTERM(%d, %d, %a, %a)" nargs cfun_nargs
        print_mlfun f print_env_args (env, args, false)
    | SPECIAL_SPECIAL_APPTERM (nargs, cfun_nargs, curr_fsz, env, args) ->
      fprintf oc "SPECIAL_SPECIAL_APPTERM(%d, %d, %d, %a)" nargs cfun_nargs
        curr_fsz print_env_args (env, args, false)
    | RETURN src ->
      fprintf oc "RETURN(%a)" print_expr src
    | MAKE_YOUNG_BLOCK (size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_YOUNG_BLOCK(%d, %d, %a, %d)" size tag print_lvalue dst
        frame_sz
    | MAKE_SAVED_YOUNG_BLOCK (to_save, size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_YOUNG_BLOCK(%a, %d, %d, %a, %d)" print_lvalue
        to_save size tag print_lvalue dst frame_sz
    | MAKE_BLOCK (size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_BLOCK(%d, %d, %a, %d)" size tag print_lvalue dst
        frame_sz
    | MAKE_SAVED_BLOCK (to_save, size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_BLOCK(%a, %d, %d, %a, %d)" print_lvalue to_save
        size tag print_lvalue dst frame_sz
    | MAKE_YOUNG_FLOAT_BLOCK (size, dst, frame_sz) ->
      fprintf oc "MAKE_YOUNG_FLOAT_BLOCK(%d, %a, %d)" size print_lvalue dst
        frame_sz
    | MAKE_SAVED_YOUNG_FLOAT_BLOCK (to_save, size, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_YOUNG_FLOAT_BLOCK(%a, %d, %a, %d)" print_lvalue
        to_save size print_lvalue dst frame_sz
    | MAKE_FLOAT_BLOCK (size, dst, frame_sz) ->
      fprintf oc "MAKE_FLOAT_BLOCK(%d, %a, %d)" size print_lvalue dst
        frame_sz
    | MAKE_SAVED_FLOAT_BLOCK (to_save, size, dst, frame_sz) ->
      fprintf oc "BLOCK(%a, %d, %a, %d)" print_lvalue to_save size
        print_lvalue dst frame_sz
    | SET_YOUNG_FIELD (ind, block, src) ->
      fprintf oc "SET_YOUNG_FIELD(%d, %a, %a)" ind print_expr block
        print_expr src
    | INITFIELD (ind, block, src) ->
      fprintf oc "INITFIELD(%d, %a, %a)" ind print_expr block print_expr src
    | SETFIELD (ind, block, src) ->
      fprintf oc "SETFIELD(%d, %a, %a)" ind print_expr block print_expr src
    | SETFLOATFIELD (ind, block, src) ->
      fprintf oc "SETFLOATFIELD(%d, %a, %a)" ind print_expr block
        print_expr src
    | GETFIELD (ind, block, dst) ->
      fprintf oc "GETFIELD(%d, %a, %a)" ind print_expr block print_lvalue
        dst
    | GETFLOATFIELD (ind, block, dst, frame_sz) ->
      fprintf oc "GETFLOATFIELD(%d, %a, %a, %d)" ind print_expr block
        print_lvalue dst frame_sz
    | SETGLOBAL (ind, src) ->
      fprintf oc "SETGLOBAL(%d, %a)" ind print_expr src
    | CHECK_SIGNALS (frame_sz) ->
      fprintf oc "CHECK_SIGNALS(%d)" frame_sz
    | SAVED_CHECK_SIGNALS (to_save, frame_sz) ->
      fprintf oc "SAVED_CHECK_SIGNALS(%a, %d)" print_lvalue to_save
        frame_sz
    | CCALL (dst, fname, frame_sz, args) ->
      fprintf oc "CCALL(%a, %a, %d, %a)" print_lvalue_opt dst print_cfun
        fname frame_sz print_args args
    | BIG_CCALL (nargs, dst, fname, frame_sz, args) ->
      fprintf oc "BIG_CCALL(%d, %a, %a, %d, %a)" nargs print_lvalue_opt dst
        print_cfun fname frame_sz print_args args
    | PUSHTRAP (Some restore_exn, lab, ukid) ->
      fprintf oc "PUSHTRAP(%a = caml_exn_bucket, %a, %d)" print_lvalue
        restore_exn print_label lab ukid
    | PUSHTRAP (None, lab, ukid) ->
      fprintf oc "PUSHTRAP(, %a, %d)" print_label lab ukid
    | RAISE exn ->
      fprintf oc "RAISE(%a)" print_expr exn
    | POPTRAP frame_sz ->
      fprintf oc "POPTRAP(%d)" frame_sz
    | CATCH_EXCEPTION (lab, Some restore_exn) ->
      fprintf oc "CATCH_EXCEPTION(%a, %a = exn)" print_label lab
        print_lvalue restore_exn
    | CATCH_EXCEPTION (lab, None) ->
      fprintf oc "CATCH_EXCEPTION(%a, (void) 0)" print_label lab
    | SAVED_POPTRAP (to_save, frame_sz) ->
      fprintf oc "SAVED_POPTRAP(%a, %d)" print_lvalue to_save frame_sz
    | OFFSETREF (ofs, ref) ->
      fprintf oc "OFFSETREF(%d, %a)" ofs print_expr ref
    | SETVECTITEM (ind, block, src) ->
      fprintf oc "SETVECTITEM(%a, %a, %a)" print_expr ind print_expr block
        print_expr src
    | SETSTRINGCHAR (ind, str, ch) ->
      fprintf oc "SETSTRINGCHAR(%a, %a, %a)" print_expr ind print_expr str
        print_expr ch
    | GETMETHOD (tag, obj, meth) ->
      fprintf oc "GETMETHOD(%a, %a, %a)" print_expr tag print_expr obj
        print_lvalue meth
    | GETPUBMET (tag, obj, meth) ->
      fprintf oc "GETPUBMET(%d, %a, %a)" tag print_expr obj
        print_lvalue meth
    | GETDYNMET (tag, obj, meth) ->
      fprintf oc "GETDYNMET(%a, %a, %a)" print_expr tag print_expr obj
        print_lvalue meth
    | STOP ->
      fprintf oc "VM_STOP()"

and print_trace oc trace =
  match trace with
    | CEnter cfname   -> fprintf oc "printf(\"--> %%s\\n\", %S)" cfname
    | CQuit cfname    -> fprintf oc "printf(\"<-- %%s\\n\", %S)" cfname
    | MLEnter mlfun   -> fprintf oc "printf(\"--> f%%d\\n\", %d)" mlfun
    | MLQuit mlfun    -> fprintf oc "printf(\"<-- f%%d\\n\", %d)" mlfun
    | MLAppterm mlfun -> fprintf oc "printf(\"<-> f%%d\\n\", %d)" mlfun
    | MLRaise         -> fprintf oc "printf(\"<<RAISE>>\\n\")"

and print_args oc args =
  match args with
    | [] -> ()
    | [ last ] -> print_expr oc last
    | arg :: rest -> fprintf oc "%a, %a" print_expr arg print_args rest

and print_env_args oc (env, args, is_static) =
  match !Options.arch with
    | NO_ARCH ->
      fprintf oc "%a, %a" print_env env print_args args
    | GEN_ARCH ->
      if is_static then
        match env with
          | Some expr -> fprintf oc "%a, %a" print_args args print_expr expr
          | None -> print_args oc args
      else
        fprintf oc "%a, %a" print_env env print_args args
    | X86 | X86_64 ->
      match env with
        | Some expr -> fprintf oc "%a, %a" print_args args print_expr expr
        | None -> print_args oc args

and print_env oc env =
  match env with
    | Some expr -> print_expr oc expr
    | None -> fprintf oc "Val_unit"

and print_lvalue_opt oc lvalue_opt =
  match lvalue_opt with
    | Some lvalue -> fprintf oc "%a =" print_lvalue lvalue
    | None -> ()

and print_dst oc lvalue_opt =
  match lvalue_opt with
    | Some lvalue -> fprintf oc "%a =" print_lvalue lvalue
    | None -> fprintf oc "(void)"

and print_expr oc expr =
  match expr with
    | EInt n -> fprintf oc "%d" n
    | EVal_int n -> fprintf oc "Val_int(%d)" n
    | ETag_val e -> fprintf oc "Tag_val(%a)" print_expr e
    | ELong_val e -> fprintf oc "Long_val(%a)" print_expr e
    | EIs_block e -> fprintf oc "Is_block(%a)" print_expr e
    | EGlob n -> fprintf oc "Glob(%d)" n
    | EGlobField (n, p) -> fprintf oc "GlobField(%d, %d)" n p
    | EAtom t -> fprintf oc "Atom(%d)" t 
    | EOffset (e, n) -> fprintf oc "Offset(%a, %d)" print_expr e n
    | EField (e, n) -> fprintf oc "Field(%a, %d)" print_expr e n
    | ECast (t, e) -> fprintf oc "(%a) %a" print_ctype t print_expr e
    | EFunPtr id -> fprintf oc "&f%d" id
    | ELvalue lvalue -> print_lvalue oc lvalue
    | EMakeHeader (size, tag, color) ->
      fprintf oc "Make_header(%d, %a, %a)" size print_tag tag print_color color

and print_lvalue oc lvalue =
  match lvalue with
    | LEnv          -> fprintf oc "env"
    | LTmp          -> fprintf oc "tmp"
    | LVar n        -> fprintf oc "v%d" n
    | LParam n      -> fprintf oc "p%d" n
    | LGlobal s     -> fprintf oc "%s" s
    | LArray (l, i) -> fprintf oc "%a[%d]" print_lvalue l i
    | LAcc (n, ofs) -> fprintf oc "StackAcc(%d, %d)" n ofs

and print_ctype oc ctype =
  match ctype with
    | TVoid -> fprintf oc "void"
    | TValue -> fprintf oc "value"
    | TPValue -> fprintf oc "value *"

and print_tag oc TInfix_tag = fprintf oc "Infix_tag"

and print_color oc CCaml_white = fprintf oc "Caml_white"

and print_mlfun oc funid = fprintf oc "f%d" funid

and print_cfun oc name = fprintf oc "%s" name

and print_label oc lid = fprintf oc "L%05X" lid

and print_switch oc {
  sw_src = src;
  sw_long_labels = long_labels;
  sw_tag_labels = tag_labels;
} =
  let print_cases indent labels =
    let nb_label = Array.length labels in
    let f ind lab =
      if ind = nb_label - 1 then
        fprintf oc "%s  default: goto %a;\n" indent print_label lab
      else
        fprintf oc "%s  case %d: goto %a;\n" indent ind print_label lab
    in
    Array.iteri f labels
  in
  let print_switch_long indent =
    fprintf oc "%sswitch (Long_val(%a)) {\n" indent print_expr src;
    print_cases indent long_labels;
    fprintf oc "%s}\n" indent;
  in
  let print_switch_tag indent =
    fprintf oc "%sswitch (Tag_val(%a)) {\n" indent print_expr src;
    print_cases indent tag_labels;
    fprintf oc "%s}\n" indent;
  in
  if long_labels = [||] then
    print_switch_tag "  "
  else if tag_labels = [||] then
    print_switch_long "  "
  else (
    fprintf oc "  if (Is_block(%a)) {\n" print_expr src;
    print_switch_tag "    ";
    fprintf oc "  } else {\n";
    print_switch_long "    ";
    fprintf oc "  }\n";
  )

and print_var_decl oc {
  vd_type = ty;
  vd_name = name;
} =
  fprintf oc "%a %s;\n" print_ctype ty name

and print_fun_signature oc {
  fs_ret_type = ret_type;
  fs_name     = name;
  fs_params   = params;
  fs_static   = static;
} =
  let print_param oc { vd_type = ty ; vd_name = name } =
    fprintf oc "%a %s" print_ctype ty name;
  in
  let rec print_params oc ps =
    match ps with
      | [] -> ()
      | [ last ] -> print_param oc last
      | p :: rest -> fprintf oc "%a, %a" print_param p print_params rest
  in
  if static then output_string oc "static ";
  fprintf oc "%a %s(" print_ctype ret_type name;
  print_params oc params;
  fprintf oc ")";

and print_location oc location =
  match location with
    | Some loc -> Printf.fprintf oc "/* %a */\n" Printer.print_location loc
    | None -> ()

and print_fun_decl oc {
  fdc_location  = location;
  fdc_signature = signature;
  fdc_noinline  = noinline;
} =
  print_location oc location;
  print_fun_signature oc signature;
  if noinline then fprintf oc " %s" Tools.noinline;
  fprintf oc ";\n";

and print_fun_def oc {
  fdf_location  = location;
  fdf_signature = signature;
  fdf_locals    = locals;
  fdf_body      = body;
  fdf_special   = special;
} =
  let cnt = ref 0 in
  let print_local oc { vd_type = ty ; vd_name = name } =
    if !cnt = 0 then fprintf oc "  value "
    else if !cnt mod 16 = 0 then fprintf oc ",\n        "
    else fprintf oc ", ";
    begin match ty with
      | TVoid -> assert false
      | TValue -> fprintf oc "%s" name
      | TPValue -> fprintf oc "*%s" name
    end;
    incr cnt;
  in
  print_location oc location;
  print_fun_signature oc signature;
  fprintf oc " {\n";
  begin match special with
    | Some fun_id ->
      fprintf oc "  OCAMLCC_SPECIAL_TAIL_CALL_HEADER(%d);\n" fun_id;
    | None -> ()
  end;
  List.iter (print_local oc) locals;
  if !cnt <> 0 then fprintf oc ";\n";
  fprintf oc "  DeclareLocalSp();\n";
  List.iter (print_instr oc) body;
  fprintf oc "}\n\n";

and print_macroc oc macroc =
  Data.export oc macroc.mc_data;
  Printf.fprintf oc "\n";
  Apply.gen_applies oc macroc.mc_max_arity;
  List.iter (print_fun_decl oc) macroc.mc_fun_decls;
  Printf.fprintf oc "\n";
  List.iter (print_fun_def oc) macroc.mc_fun_defs;
;;
