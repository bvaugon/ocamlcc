/*************************************************************************/
/*                                                                       */
/*                               OCamlCC                                 */
/*                                                                       */
/*                    Michel Mauny, Benoit Vaugon                        */
/*                          ENSTA ParisTech                              */
/*                                                                       */
/*    This file is distributed under the terms of the CeCILL license.    */
/*    See file ../LICENSE-en.                                            */
/*                                                                       */
/*************************************************************************/

#if defined(OCAMLCC_ARCH_X86) || defined(OCAMLCC_ARCH_X86_64)

/***/

#define ocamlcc_apply_init_stack(curr_fsz, next_fsz) {                    \
  if (next_fsz != 0) {                                                    \
    if ((caml_extern_sp = sp - next_fsz) < caml_stack_threshold) {        \
      caml_extern_sp = sp - curr_fsz;                                     \
      caml_realloc_stack(Stack_threshold / sizeof(value));                \
      sp = caml_extern_sp + curr_fsz;                                     \
      caml_extern_sp = sp - next_fsz;                                     \
    }                                                                     \
  } else {                                                                \
    caml_extern_sp = sp;                                                  \
  }                                                                       \
}

#define ocamlcc_apply_restore_stack(next_fsz) {                           \
  sp = caml_extern_sp + next_fsz;                                         \
}

/***/

#define ocamlcc_dynamic_apply(nargs, cfun_nargs, curr_fsz, next_fsz,    \
                              dst, args...) {                           \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                         \
  dst ocamlcc_apply_##nargs(args);                                      \
  ocamlcc_apply_restore_stack(next_fsz);                                \
}

#define ocamlcc_partial_apply(nargs, cfun_nargs, curr_fsz, next_fsz, \
                              dst, args...)                          \
  ocamlcc_dynamic_apply(nargs, cfun_nargs, curr_fsz, next_fsz, dst, args)

#define ocamlcc_static_apply(nargs, cfun_nargs, curr_fsz, next_fsz,     \
                             dst, f, args...) {                         \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                         \
  dst f(args);                                                          \
  ocamlcc_apply_restore_stack(next_fsz);                                \
}

/***/

#define ocamlcc_dynamic_standard_appterm(nargs, cfun_nargs, curr_fsz,   \
                                         args...) {                     \
  caml_extern_sp = sp;                                                  \
  return ocamlcc_apply_##nargs(args);                                   \
}

#define ocamlcc_partial_standard_appterm(nargs, cfun_nargs, curr_fsz, \
                                         args...)                     \
  ocamlcc_dynamic_standard_appterm(nargs, cfun_nargs, curr_fsz, args)

#define ocamlcc_static_standard_appterm(nargs, cfun_nargs, f, args...) { \
  caml_extern_sp = sp;                                                  \
  return f(args);                                                       \
}

/***/

#define ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz,    \
                                        args...) {                      \
  caml_extern_sp = sp;                                                  \
  ocamlcc_tail_call_##cfun_nargs((value) &ocamlcc_apply_##nargs, args); \
}

#define ocamlcc_partial_special_appterm(nargs, cfun_nargs, curr_fsz, \
                                        args...)                     \
  ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz, args)

#define ocamlcc_static_special_appterm(nargs, cfun_nargs, f, args...) { \
  caml_extern_sp = sp;                                                  \
  ocamlcc_tail_call_##cfun_nargs((value) &f, args);                     \
}

/***/

#define ocamlcc_special_special_appterm(nargs, cfun_nargs, curr_fsz, \
                                        args...)                     \
  ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz, args)

/***/

#define OCAMLCC_SPECIAL_TAIL_CALL_HEADER(id)

#endif /* OCAMLCC_ARCH_X86 || OCAMLCC_ARCH_X86_64 */
