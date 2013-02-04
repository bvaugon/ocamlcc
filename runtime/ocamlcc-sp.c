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

#if defined(OCAMLCC_SP_GLOBAL) || defined(OCAMLCC_SP_REGISTER)

#define sp caml_extern_sp

#define DeclareLocalSp()

#define OffsetSp(ofs) (sp += (ofs))
#define ResetSp(ofs) (sp -= (ofs))
#define StackAcc(ind, ofs) sp[ind + ofs]

#define ocamlcc_apply_init_stack(curr_fsz, next_fsz) {                  \
  if (next_fsz != 0) {                                                  \
    if ((sp = sp - next_fsz) < caml_stack_threshold) {                  \
      sp -= curr_fsz - next_fsz;                                        \
      caml_realloc_stack(Stack_threshold / sizeof(value));              \
      sp += curr_fsz - next_fsz;                                        \
    }                                                                   \
  }                                                                     \
}

#define ocamlcc_apply_restore_stack(next_fsz) {                         \
  sp += next_fsz;                                                       \
}

#else /* OCAMLCC_SP_GLOBAL || OCAMLCC_SP_REGISTER */

#define DeclareLocalSp() value *sp; sp = caml_extern_sp; (void) sp

#define OffsetSp(ofs) (caml_extern_sp = sp + (ofs))
#define ResetSp(ofs)
#define StackAcc(ind, ofs) sp[ind]

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

#endif /* OCAMLCC_SP_GLOBAL || OCAMLCC_SP_REGISTER */

/***/

#define SaveSp(var) ((var) = (char *) caml_stack_high - (char *) sp)
#define RestoreSp(var) (sp = (value *) ((char *) caml_stack_high - (var)))

/***/

#ifdef OCAMLCC_SP_REGISTER
#if defined(__i386__) || defined(__i486__) || defined(__i586__) || defined(__i686__)
CAMLexport register value * caml_extern_sp __asm__("esi");
#elif defined(__x86_64__)
CAMLexport register value * caml_extern_sp __asm__("r15");
#else /* x86 || x86-64 */
#error - Cannot allocate a stack pointer register: unknown architecture
#endif /* x86 || x86-64 */
#else /* OCAMLCC_SP_REGISTER */
CAMLexport value * caml_extern_sp;
#endif /* OCAMLCC_SP_REGISTER */
