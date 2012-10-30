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

#ifdef OCAMLCC_ARCH_GEN

#include <stdio.h>
#include <stdlib.h>
#include "ocamlcc-byterun/mlvalues.h"
#include "ocamlcc-byterun/memory.h"
#include "ocamlcc-byterun/stacks.h"

/***/

typedef value (*ocamlcc_fun)();

/***/

static long ocamlcc_global_nargs;
static value ocamlcc_global_closure;
static ocamlcc_fun ocamlcc_global_fun;
static value ocamlcc_global_params[OCAMLCC_MAXIMUM_ARITY + 1];

/***/

static value ocamlcc_exec_closure(long nargs, value closure);
static value ocamlcc_generic_apply(long nargs, value closure);
static value ocamlcc_fix_tailcalls();

/***/

static value ocamlcc_generic_apply(long nargs, value closure) {
  value arity_val = Field(closure, 1);
  long arity = Long_val(arity_val);

  if (arity_val == Val_long(nargs)) {
    return ocamlcc_exec_closure(nargs, closure);

  } else if (Is_block(arity_val)) {
    value sub_closure = arity_val;
    long sub_arity = Long_val(Field(sub_closure, 1));
    long nb_already_passed = Wosize_val(closure) - 1;
    long total_passed = nb_already_passed + nargs;

    if (total_passed == sub_arity) {
      long i;
      for (i = nargs - 1 ; i >= 0 ; i --)
        ocamlcc_global_params[i + nb_already_passed] =
          ocamlcc_global_params[i];
      for (i = nb_already_passed - 1 ; i > 0 ; i --)
        ocamlcc_global_params[i] = Field(closure, i + 1);
      ocamlcc_global_params[0] = Field(closure, 0);
      return ocamlcc_exec_closure(total_passed, sub_closure);

    } else if (total_passed < sub_arity) {
      long i;
      value new_closure;
      Ocamlcc_alloc_small(new_closure, total_passed + 1, Closure_tag,
                          *--caml_extern_sp = closure;
                          for (i = 0 ; i < nargs ; i ++)
                            *--caml_extern_sp = ocamlcc_global_params[i];
                          ,
                          for (i = nargs - 1 ; i >= 0 ; i --)
                            ocamlcc_global_params[i] = *caml_extern_sp++;
                          closure = *caml_extern_sp++;
                          );
      for (i = 0 ; i <= nb_already_passed ; i ++)
        Field(new_closure, i) = Field(closure, i);
      for (i = 0 ; i < nargs ; i ++)
        Field(new_closure, nb_already_passed + i + 1) =
          ocamlcc_global_params[i];
      return new_closure;

    } else {
      value new_closure;
      *--caml_extern_sp = ocamlcc_global_params[nargs - 1];
      new_closure = ocamlcc_generic_apply(nargs - 1, closure);
      if (new_closure == (value) NULL) new_closure = ocamlcc_fix_tailcalls();
      ocamlcc_global_params[0] = *caml_extern_sp++;
      return ocamlcc_generic_apply(1, new_closure);
    }

  } else if (nargs < arity) {
    long i;
    value new_closure;
    Ocamlcc_alloc_small(new_closure, nargs + 1, Closure_tag,
                        *--caml_extern_sp = closure;
                        for (i = 0 ; i < nargs ; i ++)
                          *--caml_extern_sp = ocamlcc_global_params[i];
                        ,
                        for (i = nargs - 1 ; i >= 0 ; i --)
                          ocamlcc_global_params[i] = *caml_extern_sp++;
                        closure = *caml_extern_sp++;
                        );
    Field(new_closure, 0) = ocamlcc_global_params[0];
    Field(new_closure, 1) = closure;
    for (i = 1 ; i < nargs ; i ++)
      Field(new_closure, i + 1) = ocamlcc_global_params[i];
    return new_closure;

  } else {
    long i;
    value new_closure;
    for (i = arity ; i < nargs ; i ++)
      *--caml_extern_sp = ocamlcc_global_params[i];
    new_closure = ocamlcc_exec_closure(arity, closure);
    if (new_closure == (value) NULL) new_closure = ocamlcc_fix_tailcalls();
    for (i = nargs - arity - 1 ; i >= 0 ; i --)
      ocamlcc_global_params[i] = *caml_extern_sp++;
    return ocamlcc_generic_apply(nargs - arity, new_closure);
  }
}

/***/

value ocamlcc_apply_gen(value closure, long nargs, value args[]) {
  long i;
  value res;
  if (nargs > OCAMLCC_MAXIMUM_ARITY) {
    fprintf(stderr, "Error: invalid callback arity\n");
    exit(1);
  }
  for (i = 0 ; i < nargs ; i ++)
    ocamlcc_global_params[i] = args[i];
  res = ocamlcc_generic_apply(nargs, closure);
  if (res == (value) NULL) res = ocamlcc_fix_tailcalls();
  return res;
}

/***/

static value ocamlcc_fix_tailcalls() {
  value res;
  do {
    value closure = ocamlcc_global_closure;
    if (closure != Val_unit)
      res = ocamlcc_generic_apply(ocamlcc_global_nargs, closure);
    else
      res = ocamlcc_exec_closure(ocamlcc_global_nargs,
                                 (value) &ocamlcc_global_fun);
  } while (res == (value) NULL);
  return res;
}

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
                              dst, env, args...) {                      \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                         \
  if (Field(env, 1) == Val_long(nargs)) {                               \
    if ((dst ((ocamlcc_fun) Field(env, 0))(args, env)) == (value) NULL) \
      dst ocamlcc_fix_tailcalls();                                      \
  } else {                                                              \
    ocamlcc_store_args_##nargs(args);                                   \
    if ((dst ocamlcc_generic_apply(nargs, env)) == (value) NULL)        \
      dst ocamlcc_fix_tailcalls();                                      \
  }                                                                     \
  ocamlcc_apply_restore_stack(next_fsz);                                \
}

#define ocamlcc_partial_apply(nargs, cfun_nargs, curr_fsz, next_fsz,    \
                              dst, env, args...)                        \
  ocamlcc_dynamic_apply(nargs, cfun_nargs, curr_fsz, next_fsz, dst, env, args)

#define ocamlcc_static_apply(nargs, cfun_nargs, curr_fsz, next_fsz,     \
                             dst, f, args...) {                         \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                         \
  if ((dst f(args)) == (value) NULL) dst ocamlcc_fix_tailcalls();       \
  ocamlcc_apply_restore_stack(next_fsz);                                \
}

/***/

#define ocamlcc_dynamic_standard_appterm(nargs, cfun_nargs, curr_fsz,   \
                                         env, args...) {                \
  caml_extern_sp = sp;                                                  \
  if (Field(env, 1) == Val_long(nargs)) {                               \
    return ((ocamlcc_fun) Field(env, 0))(args, env);                    \
  } else {                                                              \
    ocamlcc_store_args_##nargs(args);                                   \
    return ocamlcc_generic_apply(nargs, env);                           \
  }                                                                     \
}

#define ocamlcc_partial_standard_appterm(nargs, cfun_nargs, curr_fsz,   \
                                         env, args...)                  \
  ocamlcc_dynamic_standard_appterm(nargs, cfun_nargs, curr_fsz, env, args)

#define ocamlcc_static_standard_appterm(nargs, cfun_nargs, f, args...) { \
  caml_extern_sp = sp;                                                  \
  return f(args);                                                       \
}

/***/

#define ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz,    \
                                        env, args...) {                 \
  caml_extern_sp = sp;                                                  \
  ocamlcc_global_nargs = nargs;                                         \
  ocamlcc_global_closure = env;                                         \
  ocamlcc_global_fun = NULL;                                            \
  ocamlcc_store_args_##nargs(args);                                     \
  return (value) NULL;                                                  \
}

#define ocamlcc_partial_special_appterm(nargs, cfun_nargs, curr_fsz,    \
                                        env, args...)                   \
  ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz, env, args)

#define ocamlcc_static_special_appterm(nargs, cfun_nargs, f, env,       \
                                       args...) {                       \
  caml_extern_sp = sp;                                                  \
  ocamlcc_global_nargs = nargs;                                         \
  ocamlcc_global_closure = env;                                         \
  ocamlcc_global_fun = &f;                                              \
  ocamlcc_store_args_##nargs(args);                                     \
  return (value) NULL;                                                  \
}

/***/

#define ocamlcc_special_special_appterm(nargs, cfun_nargs, curr_fsz,    \
                                        env, args...)                   \
  ocamlcc_dynamic_special_appterm(nargs, cfun_nargs, curr_fsz, env, args)

/***/

#define OCAMLCC_SPECIAL_TAIL_CALL_HEADER(id)

#endif /* OCAMLCC_ARCH_GEN */
