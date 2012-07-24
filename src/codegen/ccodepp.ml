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
open Ccode

let rec export_macro oc macro =
  match macro with
    | MOVE (src, dst) ->
      fprintf oc "MOVE(%a, %a)" export_expr src export_lvalue dst
    | OFFSETINT (n, src, dst) ->
      fprintf oc "OFFSETINT(%d, %a, %a)" n export_expr src export_lvalue
        dst
    | BOOLNOT (src, dst) ->
      fprintf oc "BOOLNOT(%a, %a)" export_expr src export_lvalue dst
    | NEGINT (src, dst) ->
      fprintf oc "NEGINT(%a, %a)" export_expr src export_lvalue dst
    | ISINT (src, dst) ->
      fprintf oc "ISINT(%a, %a)" export_expr src export_lvalue dst
    | VECTLENGTH (src, dst) ->
      fprintf oc "VECTLENGTH(%a, %a)" export_expr src export_lvalue dst
    | ADDINT (op1, op2, dst) ->
      fprintf oc "ADDINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | SUBINT (op1, op2, dst) ->
      fprintf oc "SUBINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | MULINT (op1, op2, dst) ->
      fprintf oc "MULINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | DIVINT (op1, op2, dst, frame_sz) ->
      fprintf oc "DIVINT(%a, %a, %a, %d)" export_expr op1 export_expr op2
        export_lvalue dst frame_sz
    | MODINT (op1, op2, dst, frame_sz) ->
      fprintf oc "MODINT(%a, %a, %a, %d)" export_expr op1 export_expr op2
        export_lvalue dst frame_sz
    | ANDINT (op1, op2, dst) ->
      fprintf oc "ANDINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | ORINT (op1, op2, dst) ->
      fprintf oc "ORINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | XORINT (op1, op2, dst) ->
      fprintf oc "XORINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | LSLINT (op1, op2, dst) ->
      fprintf oc "LSLINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | LSRINT (op1, op2, dst) ->
      fprintf oc "LSRINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | ASRINT (op1, op2, dst) ->
      fprintf oc "ASRINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | EQ (op1, op2, dst) ->
      fprintf oc "EQ(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | NEQ (op1, op2, dst) ->
      fprintf oc "NEQ(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | LTINT (op1, op2, dst) ->
      fprintf oc "LTINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | LEINT (op1, op2, dst) ->
      fprintf oc "LEINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | GTINT (op1, op2, dst) ->
      fprintf oc "GTINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | GEINT (op1, op2, dst) ->
      fprintf oc "GEINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | ULTINT (op1, op2, dst) ->
      fprintf oc "ULTINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | UGEINT (op1, op2, dst) ->
      fprintf oc "UGEINT(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | GETVECTITEM (op1, op2, dst) ->
      fprintf oc "GETVECTITEM(%a, %a, %a)" export_expr op1 export_expr op2
        export_lvalue dst
    | GETSTRINGCHAR (op1, op2, dst) ->
      fprintf oc "GETSTRINGCHAR(%a, %a, %a)" export_expr op1 export_expr
        op2 export_lvalue dst
    | BRANCH lab ->
      fprintf oc "BRANCH(%a)" export_label lab
    | BRANCHIF (src, lab) ->
      fprintf oc "BRANCHIF(%a, %a)" export_expr src export_label lab
    | BRANCHIFNOT (src, lab) ->
      fprintf oc "BRANCHIFNOT(%a, %a)" export_expr src export_label lab
    | BEQ (n, src, lab) ->
      fprintf oc "BEQ(%d, %a, %a)" n export_expr src export_label lab
    | BNEQ (n, src, lab) ->
      fprintf oc "BNEQ(%d, %a, %a)" n export_expr src export_label lab
    | BLTINT (n, src, lab) ->
      fprintf oc "BLTINT(%d, %a, %a)" n export_expr src export_label lab
    | BLEINT (n, src, lab) ->
      fprintf oc "BLEINT(%d, %a, %a)" n export_expr src export_label lab
    | BGTINT (n, src, lab) ->
      fprintf oc "BGTINT(%d, %a, %a)" n export_expr src export_label lab
    | BGEINT (n, src, lab) ->
      fprintf oc "BGEINT(%d, %a, %a)" n export_expr src export_label lab
    | BULTINT (n, src, lab) ->
      fprintf oc "BULTINT(%d, %a, %a)" n export_expr src export_label lab
    | BUGEINT (n, src, lab) ->
      fprintf oc "BUGEINT(%d, %a, %a)" n export_expr src export_label lab
    | DYNAMIC_APPLY (nargs, curr_fsz, next_fsz, dst, args) ->
      fprintf oc "DYNAMIC_APPLY(%d, %d, %d, %a, %a)" nargs curr_fsz
        next_fsz export_lvalue_opt dst export_args args
    | PARTIAL_APPLY (nargs, curr_fsz, next_fsz, dst, args) ->
      fprintf oc "PARTIAL_APPLY(%d, %d, %d, %a, %a)" nargs curr_fsz
        next_fsz export_lvalue_opt dst export_args args
    | STATIC_APPLY (nargs, curr_fsz, next_fsz, dst, f, args) ->
      fprintf oc "STATIC_APPLY(%d, %d, %d, %a, %a, %a)" nargs curr_fsz
        next_fsz export_lvalue_opt dst export_mlfun f export_args args
    | DYNAMIC_APPTERM (nargs, tc_nargs, curr_fsz, args) ->
      fprintf oc "DYNAMIC_APPTERM(%d, %d, %d, %a)" nargs tc_nargs
        curr_fsz export_args args
    | PARTIAL_APPTERM (nargs, tc_nargs, curr_fsz, args) ->
      fprintf oc "PARTIAL_APPTERM(%d, %d, %d, %a)" nargs tc_nargs curr_fsz
        export_args args
    | SPECIAL_APPTERM (nargs, tc_nargs, curr_fsz, args) ->
      fprintf oc "SPECIAL_APPTERM(%d, %d, %d, %a)" nargs tc_nargs curr_fsz
        export_args args
    | STATIC_APPTERM (tc_nargs, f, args) ->
      fprintf oc "STATIC_APPTERM(%d, %a, %a)" tc_nargs export_mlfun f
        export_args args
    | RETURN src ->
      fprintf oc "RETURN(%a)" export_expr src
    | MAKE_YOUNG_BLOCK (size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_YOUNG_BLOCK(%d, %d, %a, %d)" size tag export_lvalue dst
        frame_sz
    | MAKE_SAVED_YOUNG_BLOCK (to_save, size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_YOUNG_BLOCK(%a, %d, %d, %a, %d)" export_lvalue
        to_save size tag export_lvalue dst frame_sz
    | MAKE_BLOCK (size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_BLOCK(%d, %d, %a, %d)" size tag export_lvalue dst
        frame_sz
    | MAKE_SAVED_BLOCK (to_save, size, tag, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_BLOCK(%a, %d, %d, %a, %d)" export_lvalue to_save
        size tag export_lvalue dst frame_sz
    | MAKE_YOUNG_FLOAT_BLOCK (size, dst, frame_sz) ->
      fprintf oc "MAKE_YOUNG_FLOAT_BLOCK(%d, %a, %d)" size export_lvalue dst
        frame_sz
    | MAKE_SAVED_YOUNG_FLOAT_BLOCK (to_save, size, dst, frame_sz) ->
      fprintf oc "MAKE_SAVED_YOUNG_FLOAT_BLOCK(%a, %d, %a, %d)" export_lvalue
        to_save size export_lvalue dst frame_sz
    | MAKE_FLOAT_BLOCK (size, dst, frame_sz) ->
      fprintf oc "MAKE_FLOAT_BLOCK(%d, %a, %d)" size export_lvalue dst
        frame_sz
    | MAKE_SAVED_FLOAT_BLOCK (to_save, size, dst, frame_sz) ->
      fprintf oc "BLOCK(%a, %d, %a, %d)" export_lvalue to_save size
        export_lvalue dst frame_sz
    | SET_YOUNG_FIELD (ind, block, src) ->
      fprintf oc "SET_YOUNG_FIELD(%d, %a, %a)" ind export_expr block
        export_expr src
    | SETFIELD (ind, block, src) ->
      fprintf oc "SETFIELD(%d, %a, %a)" ind export_expr block export_expr src
    | SETFLOATFIELD (ind, block, src) ->
      fprintf oc "SETFLOATFIELD(%d, %a, %a)" ind export_expr block
        export_expr src
    | GETFIELD (ind, block, dst) ->
      fprintf oc "GETFIELD(%d, %a, %a)" ind export_expr block export_lvalue
        dst
    | GETFLOATFIELD (ind, block, dst, frame_sz) ->
      fprintf oc "GETFLOATFIELD(%d, %a, %a, %d)" ind export_expr block
        export_lvalue dst frame_sz
    | SETGLOBAL (ind, src) ->
      fprintf oc "SETGLOBAL(%d, %a)" ind export_expr src
    | CHECK_SIGNALS (frame_sz) ->
      fprintf oc "CHECK_SIGNALS(%d)" frame_sz
    | SAVED_CHECK_SIGNALS (to_save, frame_sz) ->
      fprintf oc "SAVED_CHECK_SIGNALS(%a, %d)" export_lvalue to_save
        frame_sz
    | CCALL (dst, fname, frame_sz, args) ->
      fprintf oc "frame_sz,(%a, %a ,%d, %a)" export_lvalue_opt dst export_cfun
        fname frame_sz export_args args
    | BIG_CCALL (nargs, dst, fname, frame_sz, args) ->
      fprintf oc "BIG_CCALL(%d, %a, %a, %d, %a)" nargs export_lvalue_opt dst
        export_cfun fname frame_sz export_args args
    | PUSHTRAP (Some restore_exn, lab, ukid) ->
      fprintf oc "PUSHTRAP(%a = caml_exn_bucket, %a, %d)" export_lvalue
        restore_exn export_label lab ukid
    | PUSHTRAP (None, lab, ukid) ->
      fprintf oc "PUSHTRAP(, %a, %d)" export_label lab ukid
    | RAISE exn ->
      fprintf oc "RAISE(%a)" export_expr exn
    | POPTRAP frame_sz ->
      fprintf oc "POPTRAP(%d)" frame_sz
    | SAVED_POPTRAP (to_save, frame_sz) ->
      fprintf oc "SAVED_POPTRAP(%a, %d)" export_lvalue to_save frame_sz
    | OFFSETREF (ofs, ref) ->
      fprintf oc "OFFSETREF(%a, %a)" export_expr ofs export_expr ref
    | SETVECTITEM (ind, block, src) ->
      fprintf oc "SETVECTITEM(%d, %a, %a)" ind export_expr block export_expr
        src
    | SETSTRINGCHAR (ind, str, ch) ->
      fprintf oc "SETSTRINGCHAR(%d, %a, %a)" ind export_expr str export_expr
        ch
    | GETMETHOD (tag, obj, meth) ->
      fprintf oc "GETMETHOD(%a, %a, %a)" export_expr tag export_expr obj
        export_expr meth
    | GETPUBMET (tag, obj, meth) ->
      fprintf oc "GETPUBMET(%a, %a, %a)" export_expr tag export_expr obj
        export_expr meth
    | GETDYNMET (tag, obj, meth) ->
      fprintf oc "GETDYNMET(%a, %a, %a)" export_expr tag export_expr obj
        export_expr meth
    | STOP ->
      fprintf oc "STOP"

and export_args oc args =
  match args with
    | [] -> ()
    | [ last ] -> export_expr oc last
    | arg :: rest -> fprintf oc "%a, %a" export_expr arg export_args rest

and export_lvalue_opt oc lvalue_opt =
  match lvalue_opt with
    | Some lvalue -> fprintf oc "%a =" export_lvalue lvalue
    | None -> ()

and export_expr oc expr =
  match expr with
    | Glob n -> fprintf oc "Glob(%d)" n
    | GlobField (n, p) -> fprintf oc "GlobField(%d, %d)" n p
    | Offset (e, n) -> fprintf oc "Offset(%a, %d)" export_expr e n
    | Lvalue lvalue -> export_lvalue oc lvalue

and export_lvalue oc lvalue =
  match lvalue with
    | Var n -> fprintf oc "v%d" n
    | SpAcc n -> fprintf oc "sp[-%d]" n

and export_mlfun oc (MLfun fid) = fprintf oc "f%d" fid

and export_cfun oc (CFun name) = fprintf oc "%s" name

and export_label oc (Label lid) = fprintf oc "L%05X" lid

and export_ctype oc ctype =
  match ctype with
    | Void -> fprintf oc "void"
    | Value -> fprintf oc "value"
    | PValue -> fprintf oc "value *"

and export_switch oc {
  sw_src = src;
  sw_long_labels = long_labels;
  sw_tag_labels = tag_labels;
} =
  let export_cases indent labels =
    let rec f ind labels =
      match labels with
        | [] -> ()
        | [ last ] ->
          fprintf oc "%s  default: goto %a;\n" indent export_label last;
        | lab :: rest ->
          fprintf oc "%s  case %d: goto %a;\n" indent ind export_label lab;
          f (ind + 1) rest
    in
    f 0 labels
  in
  let export_switch_long indent =
    fprintf oc "%sswitch (Long_val(%a)) {\n" indent export_expr src;
    export_cases indent long_labels;
    fprintf oc "%s}\n" indent;
  in
  let export_switch_tag indent =
    fprintf oc "%sswitch (Tag_val(%a)) {\n" indent export_expr src;
    export_cases indent long_labels;
    fprintf oc "%s}\n" indent;
  in
  if long_labels = [] then
    export_switch_tag "  "
  else if tag_labels = [] then
    export_switch_long "  "
  else (
    fprintf oc "  if (Is_block(%a)) {\n" export_expr src;
    export_switch_tag "    ";
    fprintf oc "  } else {\n";
    export_switch_long "    ";
    fprintf oc "  }\n";
  )

and export_instr oc instr =
  match instr with
    | IMacro macro -> fprintf oc "  %a;\n" export_macro macro
    | ILabel label -> fprintf oc " %a:\n" export_label label
    | ISwitch switch -> export_switch oc switch

and export_var_decl oc {
  vd_type = ty;
  vd_name = name;
} =
  fprintf oc "%a %s;\n" export_ctype ty name

and export_fun_signature oc ret_type name params =
  let export_param oc { vd_type = ty ; vd_name = name } =
    fprintf oc "%a %s" export_ctype ty name;
  in
  let rec export_params oc ps =
    match ps with
      | [] -> ()
      | [ last ] -> export_param oc last
      | p :: rest -> fprintf oc "%a, %a" export_param p export_params rest
  in
  fprintf oc "%a %s(" export_ctype ret_type name;
  export_params oc params;
  fprintf oc ")";

and export_fun_decl oc {
  fdc_ret_type = ret_type;
  fdc_name = name;
  fdc_params = params;
  fdc_noinline = noinline;
} =
  export_fun_signature oc ret_type name params;
  if noinline then fprintf oc " __attribute__((noinline))";
  fprintf oc ";\n";

and export_fun_def oc {
  fdf_ret_type = ret_type;
  fdf_name = name;
  fdf_params = params;
  fdf_locals = locals;
  fdf_body = body;
} =
  let export_local oc { vd_type = ty ; vd_name = name } =
    fprintf oc "  %a %s;\n" export_ctype ty name;
  in
  export_fun_signature oc ret_type name params;
  fprintf oc " {\n";
  List.iter (export_local oc) locals;
  List.iter (export_instr oc) body;
;;
