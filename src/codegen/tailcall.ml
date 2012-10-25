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

let gen_tail_call_param oc n =
  fprintf oc "value ocamlcc_tail_call_p%d;\n" n;
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
    for i = 1 to max_arity + 1 do
      gen_tail_call_fun oc i;
    done;
    for i = 1 to max_arity + 1 do
      gen_tail_call_define oc i;
    done;
  ;;
end;;

module X86_64 = struct
  let arg_regs = [| "rdi" ; "rsi" ; "rdx" ; "rcx" ; "r8" ; "r9" |];;
  let arg_reg_nb = Array.length arg_regs;;

  let gen_tail_call_fun oc n =
    let fix_sse_alignment = n > arg_reg_nb && (n - arg_reg_nb) land 1 <> 0 in
    let fix_sse_offset = if fix_sse_alignment then 8 else 0 in
    fprintf oc "\
value ocamlcc_tail_call_fun_%d(value pf) {\n  \
  __asm__ __volatile__(\"ocamlcc_tail_call_body_%d:\");\n  \
  __asm__ __volatile__(\"mov %%rax, %%%s\");\n  \
  __asm__ __volatile__(\"mov %%rbp, %%rax\");\n  \
  __asm__ __volatile__(\"and $0x1, %%rax\");\n  \
  __asm__ __volatile__(\"jz ocamlcc_tail_call_start_%d\");\n  \
  __asm__ __volatile__(\"xor $0x1, %%rbp\");\n  \
  __asm__ __volatile__(\"add %%rbp, %%rsp\");\n  \
  __asm__ __volatile__(\"jmp ocamlcc_tail_call_restart_%d\");\n  \
  __asm__ __volatile__(\"ocamlcc_tail_call_start_%d:\");\n  \
  __asm__ __volatile__(\"push %%0\" : : \"m\" (ocamlcc_tail_call_return_address));\n  \
  __asm__ __volatile__(\"push %%rbp\");\n  \
  __asm__ __volatile__(\"ocamlcc_tail_call_restart_%d:\");\n  \
  __asm__ __volatile__(\"mov $0x%x, %%rbp\");\n"
      n n arg_regs.(0) n n n n
      (max ((n - arg_reg_nb) * 8 + 1 + fix_sse_offset) 1);
    if fix_sse_alignment then
      fprintf oc "  \
  __asm__ __volatile__(\"sub $0x8, %%rsp\");\n";
    for i = n downto arg_reg_nb + 1 do
      fprintf oc "  \
  __asm__ __volatile__(\"push %%0\" : : \"m\" (ocamlcc_tail_call_p%d));\n" i
    done;
    for i = min n arg_reg_nb downto 2 do
      fprintf oc "  \
  __asm__ __volatile__(\"mov %%0, %%%%%s\" : : \"m\" (ocamlcc_tail_call_p%d));\n"
        arg_regs.(i - 1) i
    done;
    fprintf oc "  \
  __asm__ __volatile__(\"push $ocamlcc_tail_call_return_%d\");\n  \
  __asm__ __volatile__(\"jmp *%%0\" : : \"m\" (ocamlcc_tail_call_function_pointer));\n  \
  __asm__ __volatile__(\"ocamlcc_tail_call_return_%d:\");\n"
      n n;
    if n > arg_reg_nb then
      fprintf oc "  \
  __asm__ __volatile__(\"add $0x%x, %%rsp\");\n"
        ((n - arg_reg_nb) * 8 + fix_sse_offset);
    fprintf oc "  \
  __asm__ __volatile__(\"pop %%rbp\");\n  \
  __asm__ __volatile__(\"ret\");\n  \
  return Val_unit;\n\
}\n\n";
  ;;

  let gen_tail_call_define oc n =
    fprintf oc "\
#define ocamlcc_tail_call_%d(pf" n;
    for i = 1 to n do fprintf oc ", a%d" i done;
    fprintf oc ") { \\\n";
    for i = n downto 2 do
      fprintf oc "  \
  ocamlcc_tail_call_p%d = a%d;                                    \\\n"
        i i;
    done;
    fprintf oc "  \
  ocamlcc_tail_call_function_pointer = pf; \\\n";
    fprintf oc "  \
  __asm__ __volatile__(\"movq 0x8(%%%%rbp), %%0\" : \"=r\" (ocamlcc_tail_call_return_address) :); \\\n";
    fprintf oc "  \
  __asm__ __volatile__(\"movq $ocamlcc_tail_call_body_%d, 0x8(%%rbp)\"); \\\n"
      n;
    fprintf oc "  \
  return a1; \\\n\
}\n\n"
  ;;

  let generate oc max_arity =
    fprintf oc "value ocamlcc_tail_call_function_pointer;\n";
    fprintf oc "value ocamlcc_tail_call_return_address;\n";
    for i = 2 to max_arity + 1 do
      gen_tail_call_param oc i;
    done;
    fprintf oc "\n";
    for i = 1 to max_arity + 1 do
      gen_tail_call_fun oc i;
    done;
    for i = 1 to max_arity + 1 do
      gen_tail_call_define oc i;
    done;
  ;;
end;;

let generate oc max_arity =
  match !Options.arch with
    | NO_ARCH -> ()
    | X86     -> X86.generate oc max_arity
    | X86_64  -> X86_64.generate oc max_arity
;;
