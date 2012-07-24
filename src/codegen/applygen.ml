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

open Printf;;
open Types;;
open Tools;;

let attribute = "__attribute__((noinline))";;

module Tail_Call = struct
  let gen_tail_call_param oc n =
    fprintf oc "value ocamlcc_tail_call_p%d;\n" n;
  ;;

  let gen_tail_call_define_1 oc =
    fprintf oc "\
#define ocamlcc_tail_call_1(pf, a1) return ((value (*)(value)) (pf))(a1);\n\n";
  ;;

  module X86 = struct
    let gen_tail_call_fun oc n =
      fprintf oc "\
value ocamlcc_tail_call_fun_%d(value pf) {\n  \
  __asm__(\"ocamlcc_tail_call_body_%d:\");\n  \
  __asm__(\"movl %%ebp, %%eax\");\n  \
  __asm__(\"and $0x1, %%eax\");\n  \
  __asm__(\"jz ocamlcc_tail_call_start_%d\");\n  \
  __asm__(\"movl (%%esp), %%ecx\");\n  \
  __asm__(\"xor $0x1, %%ebp\");\n  \
  __asm__(\"add %%ebp, %%esp\");\n  \
  __asm__(\"jmp ocamlcc_tail_call_restart_%d\");\n  \
  __asm__(\"ocamlcc_tail_call_start_%d:\");\n  \
  __asm__(\"push %%ebp\");\n  \
  __asm__(\"movl 0x4(%%esp), %%ecx\");\n  \
  __asm__(\"movl %%edx, 0x4(%%esp)\");\n  \
  __asm__(\"ocamlcc_tail_call_restart_%d:\");\n  \
  __asm__(\"movl $0x%x, %%ebp\");\n"
        n n n n n n (n * 4 + 1);
      for i = n downto 1 do
        fprintf oc "  \
  __asm__(\"movl %%0, %%%%edx\" : : \"\" (ocamlcc_tail_call_p%d));\n  \
  __asm__(\"push %%edx\");\n" i;
      done;
      fprintf oc "  \
  __asm__(\"push $ocamlcc_tail_call_return_%d\");\n  \
  __asm__(\"jmp *%%ecx\");\n  \
  __asm__(\"ocamlcc_tail_call_return_%d:\");\n  \
  __asm__(\"add $0x%x, %%esp\");\n  \
  __asm__(\"pop %%ebp\");\n  \
  __asm__(\"jmp *(%%esp)\");\n  \
  return Val_unit;\n\
}\n\n" n n (n * 4);
    ;;

    let gen_tail_call_define oc n =
      fprintf oc "\
#define ocamlcc_tail_call_%d(pf" n;
      for i = 1 to n do fprintf oc ", a%d" i done;
      fprintf oc ") { \\\n";
      for i = 1 to n do
        fprintf oc "  \
  ocamlcc_tail_call_p%d = a%d;                                     \\\n" i i
      done;
      fprintf oc "  \
  __asm__(\"movl %%0, 0x8(%%%%ebp)\" : : \"r\" (pf));                   \\\n  \
  __asm__(\"movl 0x4(%%ebp), %%edx\");                               \\\n  \
  __asm__(\"movl $ocamlcc_tail_call_body_%d, %%ecx\");               \\\n  \
  __asm__(\"movl %%ecx, 0x4(%%ebp)\");                               \\\n  \
  return Val_unit;                                               \\\n\
}\n\n" n;
    ;;

    let generate oc max_arity =
      for i = 1 to max_arity + 1 do
        gen_tail_call_param oc i;
      done;
      fprintf oc "\n";
      for i = 2 to max_arity + 1 do
        gen_tail_call_fun oc i;
      done;
      gen_tail_call_define_1 oc;
      for i = 2 to max_arity + 1 do
        gen_tail_call_define oc i;
      done;
    ;;
  end;;

  module X86_64 = struct
    let arg_regs = [| "rdi" ; "rsi" ; "rdx" ; "rcx" ; "r8" ; "r9" |];;
    let arg_reg_nb = Array.length arg_regs;;

    let gen_tail_call_fun oc n =
      fprintf oc "\
value ocamlcc_tail_call_fun_%d(value pf) {\n  \
  __asm__(\"ocamlcc_tail_call_body_%d:\");\n  \
  __asm__(\"mov %%rbp, %%rax\");\n  \
  __asm__(\"and $0x1, %%rax\");\n  \
  __asm__(\"jz ocamlcc_tail_call_start_%d\");\n  \
  __asm__(\"xor $0x1, %%rbp\");\n  \
  __asm__(\"add %%rbp, %%rsp\");\n  \
  __asm__(\"jmp ocamlcc_tail_call_restart_%d\");\n  \
  __asm__(\"ocamlcc_tail_call_start_%d:\");\n  \
  __asm__(\"push %%r11\");\n  \
  __asm__(\"push %%rbp\");\n  \
  __asm__(\"ocamlcc_tail_call_restart_%d:\");\n  \
  __asm__(\"mov $0x%x, %%rbp\");\n"
        n n n n n n (max ((n - arg_reg_nb) * 8 + 1) 1);
      for i = n downto arg_reg_nb + 1 do
        fprintf oc "  \
  __asm__(\"mov %%0, %%%%r11\" : : \"\" (ocamlcc_tail_call_p%d));\n  \
  __asm__(\"push %%r11\");\n" i;
      done;
      fprintf oc "  \
  __asm__(\"push $ocamlcc_tail_call_return_%d\");\n  \
  __asm__(\"jmp *%%r10\");\n  \
  __asm__(\"ocamlcc_tail_call_return_%d:\");\n"
        n n;
      if n > arg_reg_nb then
        fprintf oc "  __asm__(\"add $0x%x, %%rsp\");\n" ((n - arg_reg_nb) * 8);
      fprintf oc "  \
  __asm__(\"pop %%rbp\");\n  \
  __asm__(\"ret\");\n  \
  return Val_unit;\n\
}\n\n";
    ;;

    let gen_tail_call_define oc n =
      fprintf oc "\
#define ocamlcc_tail_call_%d(pf" n;
      for i = 1 to n do fprintf oc ", a%d" i done;
      fprintf oc ") { \\\n";
      for i = arg_reg_nb + 1 to n do
        fprintf oc "  \
  ocamlcc_tail_call_p%d = a%d;                                    \\\n"
          i i;
      done;
      fprintf oc "  __asm__ __volatile__(\"";
      for i = 1 to min n arg_reg_nb do
        fprintf oc " \\\n    mov %%%d, %%%%%s; "
          (i - 1) arg_regs.(i - 1);
      done;
      fprintf oc "  \\\n    mov %%%d, %%%%r10; " (min n arg_reg_nb);
      fprintf oc "\" : \\\n    : ";
      for i = 1 to min n arg_reg_nb do
        if i <> 1 then fprintf oc ", ";
        fprintf oc "\"r\" (a%d)" i;
      done;
      fprintf oc ", \"r\" (pf)";
      fprintf oc " \\\n  : ";
      for i = 1 to min n arg_reg_nb do
        if i <> 1 then fprintf oc ", ";
        fprintf oc "\"%%%s\"" arg_regs.(i - 1);
      done;
      fprintf oc ", \"%%r10\"";
      fprintf oc "); \\\n";
      fprintf oc "  \
  __asm__ __volatile__(\"mov 0x8(%%rbp), %%r11\");                               \\\n  \
  __asm__ __volatile__(\"mov $ocamlcc_tail_call_body_%d, %%rax\");               \\\n  \
  __asm__ __volatile__(\"mov %%rax, 0x8(%%rbp)\");                               \\\n  \
  return Val_unit;                                              \\\n\
}\n\n" (if n <= arg_reg_nb then 0 else n);
    ;;

    let generate oc max_arity =
      for i = arg_reg_nb + 1 to max_arity + 1 do
        gen_tail_call_param oc i;
      done;
      fprintf oc "\n";
      gen_tail_call_fun oc 0;
      for i = arg_reg_nb + 1 to max_arity + 1 do
        gen_tail_call_fun oc i;
      done;
      gen_tail_call_define_1 oc;
      for i = 2 to max_arity + 1 do
        gen_tail_call_define oc i;
      done;
    ;;
  end;;

  let generate oc max_arity =
    match !Options.arch with
      | NO_ARCH -> X86.generate oc max_arity
      | X86     -> X86.generate oc max_arity
      | X86_64  -> X86_64.generate oc max_arity
  ;;
end;;

let gen_fun_type oc n =
  fprintf oc "typedef value (*ocamlcc_fun_%d)(value arg1," n;
  for i = 2 to n do
    fprintf oc " value arg%d," i;
    if i mod 4 = 0 then fprintf oc "\n                              ";
  done;
  fprintf oc " value env);\n";
;;

let gen_fun_apply oc n max_arity =
  fprintf oc "value ocamlcc_apply_%d(value arg1" n;
  for i = 2 to n do fprintf oc ", value arg%d" i done;
  fprintf oc ", value closure) %s;\n" attribute;
  fprintf oc "value ocamlcc_apply_%d(value arg1" n;
  for i = 2 to n do fprintf oc ", value arg%d" i done;
  fprintf oc ", value closure) {\n";
  if max_arity <> 1 then (
    fprintf oc "  value arity_val = Field(closure, 1);\n";
    if n > 1 then
      fprintf oc "  value arity = Long_val(arity_val);\n";
    fprintf oc "\n";

    fprintf oc "  if (arity_val == Val_long(%d)) {\n" n;
  );
  fprintf oc
    "    return ((ocamlcc_fun_%d) Field(closure, 0))(arg1" n;
  for i = 2 to n do fprintf oc ", arg%d" i done;
  fprintf oc ", closure);\n";
  if max_arity <> 1 && n <> max_arity then fprintf oc "\n";

  if max_arity <> 1 then (
    fprintf oc "  } else if (Is_block(arity_val)) {\n";
    if n <> max_arity then (
      fprintf oc "    value sub_closure = arity_val;\n";
      fprintf oc "    long sub_arity = Long_val(Field(sub_closure, 1));\n";
      fprintf oc "    long nb_already_passed = Wosize_val(closure) - 1;\n";
      fprintf oc "    long total_passed = nb_already_passed + %d;\n" n;
      fprintf oc "    if (total_passed == sub_arity) {\n";
      fprintf oc "      switch (sub_arity) {\n";
      for i = n + 1 to max_arity do
        if i < max_arity then fprintf oc "      case %d: " i
        else fprintf oc "      default: ";
        fprintf oc
          "ocamlcc_tail_call_%d(Field(sub_closure, 0), " (i + 1);
        fprintf oc "Field(closure, 0)";
        for j = 2 to i - n do fprintf oc ", Field(closure, %d)" j done;
        for j = 1 to n do fprintf oc ", arg%d" j done;
        fprintf oc ", sub_closure);\n";
      done;
      fprintf oc "      }\n";
      fprintf oc "\n";
      if n > 1 then
        fprintf oc "    } else if (total_passed < sub_arity) {\n"
      else
        fprintf oc "    } else {\n";
      fprintf oc "      long i;\n";
      fprintf oc "      value new_closure;\n";
      fprintf oc
        "      Ocamlcc_alloc_small(new_closure, total_passed + 1, Closure_tag,\n";
      fprintf oc "        *--caml_extern_sp = closure;\n";
      for i = 1 to n do
        fprintf oc "        *--caml_extern_sp = arg%d;\n" i;
      done;
      fprintf oc "        ,\n";
      for i = n downto 1 do
        fprintf oc "        arg%d = *caml_extern_sp++;\n" i;
      done;
      fprintf oc "        closure = *caml_extern_sp++;\n";
      fprintf oc "      );\n";
      fprintf oc "      for(i = 0 ; i <= nb_already_passed ; i ++)\n";
      fprintf oc "        Field(new_closure, i) = Field(closure, i);\n";
      for i = 1 to n do
        fprintf oc
          "      Field(new_closure, nb_already_passed + %d) = arg%d;\n" i i;
      done;
      fprintf oc "      return new_closure;\n";
    );
    if n > 1 then (
      fprintf oc "\n";
      if n <> max_arity then fprintf oc "    } else {\n"
      else fprintf oc "    {\n";
      fprintf oc "      value new_closure;\n";
      fprintf oc "      *--caml_extern_sp = arg%d;\n" n;
      fprintf oc "      new_closure = ocamlcc_apply_%d(arg1" (n - 1);
      for i = 2 to n - 1 do fprintf oc ", arg%d" i done;
      fprintf oc ", closure);\n";
      fprintf oc
        "      return ocamlcc_apply_1(*caml_extern_sp++, new_closure);\n";
    );
    fprintf oc "    }\n";
    fprintf oc "\n";

    if n > 1 then fprintf oc "  } else if (arity > %d) {\n" n
    else fprintf oc "  } else {\n";
    fprintf oc "    value new_closure;\n";
    fprintf oc "    Ocamlcc_alloc_small(new_closure, %d, Closure_tag,\n" (n+1);
    fprintf oc "      *--caml_extern_sp = arg1;\n";
    fprintf oc "      *--caml_extern_sp = closure;\n";
    for i = 2 to n do
      fprintf oc "      *--caml_extern_sp = arg%d;\n" i;
    done;
    fprintf oc "      ,\n";
    for i = n downto 2 do
      fprintf oc "      arg%d = *caml_extern_sp++;\n" i;
    done;
    fprintf oc "      closure = *caml_extern_sp++;\n";
    fprintf oc "      arg1 = *caml_extern_sp++;\n";
    fprintf oc "    );\n";
    fprintf oc "    Field(new_closure, 0) = arg1;\n";
    fprintf oc "    Field(new_closure, 1) = closure;\n";
    for i = 2 to n do
      fprintf oc "    Field(new_closure, %d) = arg%d;\n" i i
    done;
    fprintf oc "    return new_closure;\n";
    if n > 1 then (
      fprintf oc "\n";
      fprintf oc "  } else {\n";
      fprintf oc "    value new_closure;\n";
      fprintf oc "    value *sp = caml_extern_sp;\n";
      fprintf oc "    switch (arity) {\n";
      for i = 1 to n - 1 do
        if i < n - 1 then fprintf oc "      case %d: {\n" i
        else fprintf oc "      default: {\n";
        for j = i + 1 to n do
          fprintf oc "        sp[-%d] = arg%d;\n" (j - i) j;
        done;
        fprintf oc "        caml_extern_sp = sp - %d;\n" (n - i);
        fprintf oc "        new_closure = ";
        fprintf oc "((ocamlcc_fun_%d) Field(closure, 0))(arg1" i;
        for j = 2 to i do fprintf oc ", arg%d" j done;
        fprintf oc ", closure);\n";
        fprintf oc
          "        sp = caml_extern_sp = caml_extern_sp + %d;\n" (n - i);
        fprintf oc "        return ocamlcc_apply_%d(" (n - i);
        for j = i + 1 to n do fprintf oc "sp[-%d], " (j - i) done;
        fprintf oc "new_closure);\n";
        fprintf oc "      }\n";
      done;
      fprintf oc "    }\n";
    );
    if max_arity <> 1 then fprintf oc "  }\n";
  );
  fprintf oc "}\n";
;;

let gen_apply_gen oc max_arity =
  fprintf oc
    "value ocamlcc_apply_gen(value closure, long nargs, value args[]) {\n";
  fprintf oc "  switch (nargs) {\n";
  for i = 1 to max_arity do
    fprintf oc "  case %d: return ocamlcc_apply_%d(args[0]" i i;
    for j = 1 to i - 1 do fprintf oc ", args[%d]" j done;
    fprintf oc ", closure);\n";
  done;
  fprintf oc "  default:\n";
  fprintf oc "    fprintf(stderr, \"Error: invalid callback arity\\n\");\n";
  fprintf oc "    exit(1);\n";
  fprintf oc "  }\n";
  fprintf oc "}\n";
;;

let gen_applies oc max_arity =
  Tail_Call.generate oc max_arity;
  for i = 1 to max_arity do
    gen_fun_type oc i;
  done;
  fprintf oc "\n";
  for i = 1 to max_arity do
    gen_fun_apply oc i max_arity;
    fprintf oc "\n";
  done;
  gen_apply_gen oc max_arity;
  fprintf oc "\n";
;;

let export oc funs =
  let f acc instr =
    match instr.bc with
      | DynamicApply n | StaticApply (n, _) | PartialApply n
      | DynamicAppterm (n, _) | StaticAppterm (n, _, _)
      | PartialAppterm (n, _) | SpecialAppterm (n, _) -> max acc n
      | _ -> acc
  in
  let g _ fun_desc acc =
    Array.fold_left f (max acc fun_desc.arity) fun_desc.body
  in
  let max_arity = IMap.fold g funs 0 in
  gen_applies oc max_arity
;;
