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
  | IAffect of lvalue * expr  (* Affectation                         *)
  | IMacro of macro           (* Runtime macro                       *)
  | ITrace of trace           (* Execution trace (see -trace option) *)
  | ISwitch of switch         (* C switch (for OCaml compiled match) *)
  | ILabel of label           (* C label defintion (ex: L008A6B42:)  *)

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
  | DIVINT of expr * expr * lvalue option * int
  | MODINT of expr * expr * lvalue option * int
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
  | DYNAMIC_APPLY of nargs * cfun_nargs * curr_fsz * next_fsz * dst * env * args
  | PARTIAL_APPLY of nargs * cfun_nargs * curr_fsz * next_fsz * dst * env * args
  | STATIC_APPLY of nargs * cfun_nargs * curr_fsz * next_fsz * dst * mlfun *
      env * args
  | STATIC_NOTC_APPLY of nargs * cfun_nargs * curr_fsz * next_fsz * dst * mlfun *
      env * args
  | DYNAMIC_STANDARD_APPTERM of nargs * cfun_nargs * curr_fsz * env * args
  | DYNAMIC_SPECIAL_APPTERM of nargs * cfun_nargs * curr_fsz * env * args
  | PARTIAL_STANDARD_APPTERM of nargs * cfun_nargs * curr_fsz * env * args
  | PARTIAL_SPECIAL_APPTERM of nargs * cfun_nargs * curr_fsz * env * args
  | STATIC_STANDARD_APPTERM of nargs * cfun_nargs * mlfun * env * args
  | STATIC_SPECIAL_APPTERM of nargs * cfun_nargs * mlfun * env * args
  | SPECIAL_SPECIAL_APPTERM of nargs * cfun_nargs * curr_fsz * env * args
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
  | CEnter of string    (* C function call              *)
  | CQuit of string     (* C function return            *)
  | MLEnter of mlfun    (* OCaml function call          *)
  | MLQuit of mlfun     (* OCaml function return        *)
  | MLAppterm of mlfun  (* OCaml function "tail return" *)
  | MLRaise             (* OCaml exception raise        *)

and expr =
  | EInt of int                       (* C literal integer                  *)
  | EVal_int of int                   (* OCaml literal integer              *)
  | ETag_val of expr                  (* OCaml block tag access             *)
  | ELong_val of expr                 (* OCaml value to C long convertion   *)
  | EIs_block of expr                 (* Is it an OCaml block or a long ?   *)
  | EGlob of int                      (* Global OCaml value access          *)
  | EGlobField of int * int           (* Field of Global OCaml value access *)
  | EAtom of int                      (* Get OCaml atom (0 size block)      *)
  | EOffset of expr * int             (* Recursive closure offset           *)
  | EField of expr * int              (* Field of an OCaml block            *)
  | ECast of ctype * expr             (* C type cast                        *)
  | EFunPtr of mlfun                  (* C function pointer                 *)
  | ELvalue of lvalue                 (* Lvalue used as an expression       *)
  | EMakeHeader of size * tag * color (* Construct OCaml block header       *)

and lvalue =
  | LEnv                   (* Local environment pointer        *)
  | LTmp                   (* Local temporary variable         *)
  | LVar of int            (* Local C variable (v0, v1, ...)   *)
  | LParam of int          (* Function parameter (p0, p1, ...) *)
  | LGlobal of string      (* Global variable                  *)
  | LArray of lvalue * int (* Array cell                       *)
  | LAcc of int * int      (* Local stack access               *)

and ctype =
  | TVoid    (* void   *)
  | TValue   (* value  *)
  | TPValue  (* value* *)

(***)

and mlfun = int    (* OCaml function identifier          *)
and cfun = string  (* C function name (external call)    *)
and label = int    (* C label identifier (ex: L008A6B42) *)

(***)

and nargs = int          (* Argument number                                  *)
and cfun_nargs = int     (* C function argument number (with or without env) *)
and curr_fsz = int       (* Current (before call) frame size                 *)
and next_fsz = int       (* Next (after return) frame size                   *)
and dst = lvalue option  (* Optional returned value destination              *)
and env = expr option    (* Environment (callee closure)                     *)
and args = expr list     (* Function arguments                               *)

(***)

and size = int           (* OCaml block size  *)
and tag = TInfix_tag     (* OCaml block tag   *)
and color = CCaml_white  (* OCaml block color *)

(***)

and switch = {
  sw_src         : expr;
  sw_long_labels : int array;
  sw_tag_labels  : int array;
}

and var_decl = {
  vd_type : ctype;
  vd_name : string;
}

and fun_signature = {
  fs_ret_type : ctype;
  fs_name     : string;
  fs_params   : var_decl list;
  fs_static   : bool;
}

and fun_decl = {
  fdc_location  : Types.location option;
  fdc_signature : fun_signature;
  fdc_noinline  : bool;
}

and fun_def = {
  fdf_location  : Types.location option;
  fdf_signature : fun_signature;
  fdf_locals    : var_decl list;
  fdf_body      : instr list;
  fdf_special   : int option;
}

and macroc = {
  mc_data      : string;
  mc_max_arity : int;
  mc_fun_decls : fun_decl list;
  mc_fun_defs  : fun_def list;
}
