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

type macro =
  | MOVE of expr * lvalue
  | OFFSETINT of int * expr * lvalue
  | BOOLNOT of expr * lvalue
  | NEGINT of expr * lvalue
  | ISINT of expr * lvalue
  | VECTLENGTH of expr * lvalue
  | ADDINT of expr * expr * lvalue
  | SUBINT of expr * expr * lvalue
  | MULINT of expr * expr * lvalue
  | DIVINT of expr * expr * lvalue * int
  | MODINT of expr * expr * lvalue * int
  | ANDINT of expr * expr * lvalue
  | ORINT of expr * expr * lvalue
  | XORINT of expr * expr * lvalue
  | LSLINT of expr * expr * lvalue
  | LSRINT of expr * expr * lvalue
  | ASRINT of expr * expr * lvalue
  | EQ of expr * expr * lvalue
  | NEQ of expr * expr * lvalue
  | LTINT of expr * expr * lvalue
  | LEINT of expr * expr * lvalue
  | GTINT of expr * expr * lvalue
  | GEINT of expr * expr * lvalue
  | ULTINT of expr * expr * lvalue
  | UGEINT of expr * expr * lvalue
  | GETVECTITEM of expr * expr * lvalue
  | GETSTRINGCHAR of expr * expr * lvalue
  | BRANCH of label
  | BRANCHIF of expr * label
  | BRANCHIFNOT of expr * label
  | BEQ of int * expr * label
  | BNEQ of int * expr * label
  | BLTINT of int * expr * label
  | BLEINT of int * expr * label
  | BGTINT of int * expr * label
  | BGEINT of int * expr * label
  | BULTINT of int * expr * label
  | BUGEINT of int * expr * label
  | DYNAMIC_APPLY of int * int * int * lvalue option * expr list
  | PARTIAL_APPLY of int * int * int * lvalue option * expr list
  | STATIC_APPLY of int * int * int * lvalue option * mlfun * expr list
  | DYNAMIC_APPTERM of int * int * int * expr list
  | PARTIAL_APPTERM of int * int * int * expr list
  | SPECIAL_APPTERM of int * int * int * expr list
  | STATIC_APPTERM of int * mlfun * expr list
  | RETURN of expr
  | MAKE_YOUNG_BLOCK of int * int * lvalue * int
  | MAKE_SAVED_YOUNG_BLOCK of lvalue * int * int * lvalue * int
  | MAKE_BLOCK of int * int * lvalue * int
  | MAKE_SAVED_BLOCK of lvalue * int * int * lvalue * int
  | MAKE_YOUNG_FLOAT_BLOCK of int * lvalue * int
  | MAKE_SAVED_YOUNG_FLOAT_BLOCK of lvalue * int * lvalue * int
  | MAKE_FLOAT_BLOCK of int * lvalue * int
  | MAKE_SAVED_FLOAT_BLOCK of lvalue * int * lvalue * int
  | SET_YOUNG_FIELD of int * expr * expr
  | SETFIELD of int * expr * expr
  | SETFLOATFIELD of int * expr * expr
  | GETFIELD of int * expr * lvalue
  | GETFLOATFIELD of int * expr * lvalue * int
  | SETGLOBAL of int * expr
  | CHECK_SIGNALS of int
  | SAVED_CHECK_SIGNALS of lvalue * int
  | CCALL of lvalue option * cfun * int * expr list
  | BIG_CCALL of int * lvalue option * cfun * int * expr list
  | PUSHTRAP of lvalue option * label * Tools.Id.t
  | RAISE of expr
  | POPTRAP of int
  | SAVED_POPTRAP of lvalue * int
  | OFFSETREF of expr * expr
  | SETVECTITEM of int * expr * expr
  | SETSTRINGCHAR of int * expr * expr
  | GETMETHOD of expr * expr * expr
  | GETPUBMET of expr * expr * expr
  | GETDYNMET of expr * expr * expr
  | STOP

and expr =
  | Glob of int
  | GlobField of int * int
  | Offset of expr * int
  | Lvalue of lvalue

and lvalue =
  | Var of int
  | SpAcc of int

and mlfun = MLfun of int

and cfun = CFun of string

and label = Label of int

and ctype =
  | Void
  | Value
  | PValue

and switch = {
  sw_src : expr;
  sw_long_labels : label list;
  sw_tag_labels : label list;
}

and instr =
  | IMacro of macro
  | ILabel of label
  | ISwitch of switch

and var_decl = {
  vd_type : ctype;
  vd_name : string;
}

and fun_decl = {
  fdc_ret_type : ctype;
  fdc_name : string;
  fdc_params : var_decl list;
  fdc_noinline : bool;
}

and fun_def = {
  fdf_ret_type : ctype;
  fdf_name : string;
  fdf_params : var_decl list;
  fdf_locals : var_decl list;
  fdf_body : instr list;
}
