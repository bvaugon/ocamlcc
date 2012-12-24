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

#ifdef OCAMLCC_GLOBAL_SP

#define sp caml_extern_sp

#define DeclareLocalSp()

#define OffsetSp(ofs) (sp += (ofs))
#define ResetSp(ofs) (sp -= (ofs))
#define StackAcc(ind, ofs) sp[ind + ofs]

#define ocamlcc_apply_init_stack(curr_fsz, next_fsz) {                  \
  if (next_fsz != 0) {                                                  \
    if ((sp = sp - next_fsz) < caml_stack_threshold) {                  \
      sp -= curr_fsz;                                                   \
      caml_realloc_stack(Stack_threshold / sizeof(value));              \
      sp += curr_fsz - next_fsz;                                        \
    }                                                                   \
  }                                                                     \
}

#define ocamlcc_apply_restore_stack(next_fsz) {                         \
  sp += next_fsz;                                                       \
}

#else /* OCAMLCC_GLOBAL_SP */

#define DeclareLocalSp() value *sp; sp = caml_extern_sp

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

#endif /* OCAMLCC_GLOBAL_SP */

#define SaveSp(var) ((var) = (char *) caml_stack_high - (char *) sp)
#define RestoreSp(var) (sp = (value *) ((char *) caml_stack_high - (var)))
