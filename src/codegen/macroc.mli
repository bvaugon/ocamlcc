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

type instr =
  | IAffect of lvalue * expr
  | IMacro of macro
  | ITrace of trace
  | ISwitch of switch
  | ILabel of label

and macro =
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
  | DYNAMIC_APPLY of int * int * int * int * lvalue option * expr * expr list
  | PARTIAL_APPLY of int * int * int * int * lvalue option * expr * expr list
  | STATIC_APPLY of int * int * int * int * lvalue option * mlfun * expr *
      expr list
  | DYNAMIC_STANDARD_APPTERM of int * int * int * expr * expr list
  | DYNAMIC_SPECIAL_APPTERM of int * int * int * expr * expr list
  | PARTIAL_STANDARD_APPTERM of int * int * int * expr * expr list
  | PARTIAL_SPECIAL_APPTERM of int * int * int * expr * expr list
  | STATIC_STANDARD_APPTERM of int * int * mlfun * expr * expr list
  | STATIC_SPECIAL_APPTERM of int * int * mlfun * expr * expr list
  | SPECIAL_SPECIAL_APPTERM of int * int * int * expr * expr list
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
  | INITFIELD of int * expr * expr
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
  | CATCH_EXCEPTION of label * lvalue option
  | SAVED_POPTRAP of lvalue * int
  | OFFSETREF of int * expr
  | SETVECTITEM of expr * expr * expr
  | SETSTRINGCHAR of expr * expr * expr
  | GETMETHOD of expr * expr * lvalue
  | GETPUBMET of int * expr * lvalue
  | GETDYNMET of expr * expr * lvalue
  | STOP

and trace =
  | CEnter of string
  | CQuit of string
  | MLEnter of mlfun
  | MLQuit of mlfun
  | MLAppterm of mlfun
  | MLRaise

and expr =
  | EVal_unit
  | EInt of int
  | EVal_int of int
  | ETag_val of expr
  | ELong_val of expr
  | EIs_block of expr
  | EGlob of int
  | EGlobField of int * int
  | EAtom of int
  | EOffset of expr * int
  | EField of expr * int
  | EParam of int
  | ECast of ctype * expr
  | EFunPtr of mlfun
  | ELvalue of lvalue
  | EMakeHeader of int * tag * color

and lvalue =
  | LTmp
  | LVar of int
  | LSpAcc of int

and ctype =
  | TVoid
  | TValue
  | TPValue

and tag = TInfix_tag

and color = CCaml_white

and mlfun = int

and cfun = string

and label = int

and switch = {
  sw_src : expr;
  sw_long_labels : int array;
  sw_tag_labels : int array;
}

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
