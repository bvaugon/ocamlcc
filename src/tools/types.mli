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

type arch = GEN_ARCH | NO_ARCH | X86 | X86_64

type sigconf = Reactive | Efficient

type except = Setjmp | Trycatch

type section = Code | Dlpt | Dlls | Prim | Data | Symb | Crcs | Dbug

type index = (section * int * int) list

(***)

type env_desc =
  | ENone                   (* The main function haven't closure *)
  | ENonRec of int          (* (env_size)                        *)
  | ERec of int * int * int (* (env_size, env_ofs, base_fun_id)  *)

(***)

type fun_desc = {
  fun_id : int;
  arity : int;
  env_desc : env_desc;
  body : instr array;
  is_special : bool; (* contains PUSHTRAP and APPTERM *)
}

and instr = {
  addr : int;
  mutable index : int;
  mutable bc : bc;
  mutable is_pointed : bool;
}

and ptr = {
  ofs : int;
  mutable pointed : instr;
}

and bc =
  | Acc of int
  | Push
  | Pop of int
  | Assign of int
  | Envacc of int
  | Pushretaddr of ptr
  | DynamicApply of int
  | StaticApply of int * ptr
  | PartialApply of int
  | DynamicAppterm of int * int
  | StaticAppterm of int * int * ptr
  | PartialAppterm of int * int
  | SpecialAppterm of int * int
  | Return of int
  | Restart
  | Grab of int
  | Closure of int * ptr
  | Closurerec of int * int * ptr * ptr array
  | Offsetclosure of int
  | Getglobal of int
  | Getglobalfield of int * int
  | Setglobal of int
  | Atom of int
  | Makeblock of int * int
  | Makefloatblock of int
  | Getfield of int
  | Getfloatfield of int
  | Setfield of int
  | Setfloatfield of int
  | Setvectitem
  | Setstringchar
  | Branch of ptr
  | Switch of int * int * ptr array
  | Pushtrap of ptr
  | Poptrap
  | Raise
  | Checksignals
  | Ccall of int * int
  | Const of int
  | Offsetref of int
  | Unapp of unop
  | Binapp of binop
  | CondBranch of cond_branch
  | Getmethod
  | Getpubmet of int * int
  | Getdynmet
  | Stop

and unop =
  | Boolnot
  | Offsetint of int
  | Negint
  | Isint
  | Vectlength

and binop =
  | Addint
  | Subint
  | Mulint
  | Divint
  | Modint
  | Andint
  | Orint
  | Xorint
  | Lslint
  | Lsrint
  | Asrint
  | Eq
  | Neq
  | Ltint
  | Leint
  | Gtint
  | Geint
  | Ultint
  | Ugeint
  | Getvectitem
  | Getstringchar

and cond_branch =
  | Branchif of ptr
  | Branchifnot of ptr
  | Beq of int * ptr
  | Bneq of int * ptr
  | Bltint of int * ptr
  | Bleint of int * ptr
  | Bgtint of int * ptr
  | Bgeint of int * ptr
  | Bultint of int * ptr
  | Bugeint of int * ptr

type orig =
  | OGlobal of int
  | OGlobalField of int * int
  | OClosure of ptr * orig array
  | OClosurerec of ptr array * orig array * int
  | OUnknown

type glob =
  | GClosure of ptr
  | GClosurerec of ptr array * int
  | GModule of glob array
  | GUnknown

type env =
  | EClosure of ptr * orig array
  | EClosurerec of ptr array * orig array * int
  | EUnknown

type 'a desc = {
  accu : 'a;
  stack : 'a Stk.t;
}

type fake_1_t type fake_2_t type fake_3_t
type fake_4_t type fake_5_t type fake_6_t

type location = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type debug_event = {
  mutable ev_pos: int;   (* Position in bytecode           *)
  ev_module: string;     (* Name of defining module        *)
  ev_loc: location;      (* Location in source file        *)
  ev_kind: fake_1_t;     (* Before/after event             *)
  ev_info: fake_2_t;     (* Extra information              *)
  ev_typenv: fake_3_t;   (* Typing environment             *)
  ev_typsubst: fake_4_t; (* Substitution over types        *)
  ev_compenv: fake_5_t;  (* Compilation environment        *)
  ev_stacksize: int;     (* Size of stack frame            *)
  ev_repr: fake_6_t      (* Position of the representative *)
}

type dbug = location Tools.IMap.t

type alloc = Integer | Unknown | Allocated

type val_desc =
  | VConst of int           (* Known constant                         *)
  | VGlob of int            (* Global variable                        *)
  | VGlobField of int * int (* Global variable field                  *)
  | VAtom of int            (* Atom                                   *)
  | VClsr of int            (* Current (infix) closure with an offset *)
  | VEnv of int             (* Field of the environment               *)
  | VArg of int             (* Function argument                      *)
  | VPtr of ptr             (* Code pointer                           *)
  | VCell                   (* Cells                                  *)

type fun_info = {
  ptr_args : bool array;   (* Function arguments may be pointers         *)
  mutable ptr_res : bool;  (* Function result may be a pointer           *)
  mutable run_gc  : bool;  (* Function call may run the GC               *)
  mutable use_env : bool;  (* Function body may use its environment      *)
  mutable ofs_clo : bool;  (* Usage of the closure (rec call)            *)
  env_usages : bool array; (* Usage of function environment cells        *)
}

type ids_info = {
  fun_desc  : fun_desc;              (* Function description                *)
  states    : int desc option array; (* Stack cells and accu ids (by instr) *)
  idvd_map  : val_desc Tools.IMap.t; (* Ids descriptions                    *)
  ptr_set   : Tools.ISet.t;          (* GC register variable ids            *)
  read_set  : Tools.ISet.t;          (* Read variable ids                   *)
  read_args : Tools.ISet.t;          (* Read argument ids                   *)
}
