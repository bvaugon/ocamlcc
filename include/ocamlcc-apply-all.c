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

#ifdef OCAMLCC_ARCH_ALL

#include <stdio.h>
#include <stdlib.h>
#include "ocamlcc-byterun/mlvalues.h"
#include "ocamlcc-byterun/memory.h"
#include "ocamlcc-byterun/stacks.h"

/***/

typedef value (*ocamlcc_fun)();

/***/

value ocamlcc_global_params[OCAMLCC_MAXIMUM_ARITY - 1];
value ocamlcc_global_special_arg1;
value ocamlcc_global_arg_nb_val;
value ocamlcc_global_closure;
value ocamlcc_global_env;

/***/

value ocamlcc_generic_apply(value arg1) {
  value arg_nb_val = ocamlcc_global_arg_nb_val;
  value closure = ocamlcc_global_closure;
  value arity_val = Field(closure, 1);
  long arity = Long_val(arity_val);
  long arg_nb = Long_val(arg_nb_val);

  if (arity_val == arg_nb_val) {
    ocamlcc_global_env = closure;
    return ((ocamlcc_fun) Field(closure, 0))(arg1);

  } else if (Is_block(arity_val)) {
    value sub_closure = arity_val;
    long sub_arity = Long_val(Field(sub_closure, 1));
    long nb_already_passed = Wosize_val(closure) - 1;
    long total_passed = nb_already_passed + arg_nb;

    if (total_passed == sub_arity) {
      long i;
      for (i = arg_nb - 2 ; i >= 0 ; i --)
        ocamlcc_global_params[i + nb_already_passed] =
          ocamlcc_global_params[i];
      ocamlcc_global_params[nb_already_passed - 1] = arg1;
      for (i = nb_already_passed - 2 ; i >= 0 ; i --)
        ocamlcc_global_params[i] = Field(closure, i + 2);
      ocamlcc_global_env = sub_closure;
      return
        ((ocamlcc_fun) Field(sub_closure, 0))(Field(closure, 0));

    } else if (total_passed < sub_arity) {
      long i;
      value new_closure;
      Ocamlcc_alloc_small(new_closure, total_passed + 1, Closure_tag,
                          *--caml_extern_sp = closure;
                          *--caml_extern_sp = arg1;
                          for (i = 0 ; i < arg_nb - 1 ; i ++)
                            *--caml_extern_sp = ocamlcc_global_params[i];
                          ,
                          for (i = arg_nb - 2 ; i >= 0 ; i --)
                            ocamlcc_global_params[i] = *caml_extern_sp++;
                          arg1 = *caml_extern_sp++;
                          closure = *caml_extern_sp++;
                          );
      for (i = 0 ; i <= nb_already_passed ; i ++)
        Field(new_closure, i) = Field(closure, i);
      Field(new_closure, nb_already_passed + 1) = arg1;
      for (i = 0 ; i < arg_nb - 1 ; i ++)
        Field(new_closure, nb_already_passed + i + 2) =
          ocamlcc_global_params[i];
      return new_closure;

    } else {
      *--caml_extern_sp = ocamlcc_global_params[arg_nb - 2];
      ocamlcc_global_arg_nb_val = Val_long(arg_nb - 1);
      ocamlcc_global_closure = closure;
      ocamlcc_global_closure = ocamlcc_generic_apply(arg1);
      ocamlcc_global_arg_nb_val = Val_long(1);
      return ocamlcc_generic_apply(*caml_extern_sp++);
    }

  } else if (arg_nb_val < arity_val) {
    long i;
    value new_closure;
    Ocamlcc_alloc_small(new_closure, arg_nb + 1, Closure_tag,
                        *--caml_extern_sp = arg1;
                        *--caml_extern_sp = closure;
                        for (i = 0 ; i < arg_nb - 1 ; i ++)
                          *--caml_extern_sp = ocamlcc_global_params[i];
                        ,
                        for (i = arg_nb - 2 ; i >= 0 ; i --)
                          ocamlcc_global_params[i] = *caml_extern_sp++;
                        closure = *caml_extern_sp++;
                        arg1 = *caml_extern_sp++;
                        );
    Field(new_closure, 0) = arg1;
    Field(new_closure, 1) = closure;
    for (i = 0 ; i < arg_nb - 1 ; i ++)
      Field(new_closure, i + 2) = ocamlcc_global_params[i];
    return new_closure;

  } else {
    long i;
    for (i = arity - 1 ; i < arg_nb - 1 ; i ++)
      *--caml_extern_sp = ocamlcc_global_params[i];
    ocamlcc_global_env = closure;
    ocamlcc_global_closure = ((ocamlcc_fun) Field(closure, 0))(arg1);
    for (i = arg_nb - arity - 2 ; i >= 0 ; i --)
      ocamlcc_global_params[i] = *caml_extern_sp++;
    ocamlcc_global_arg_nb_val = Val_long(arg_nb - arity);
    return ocamlcc_generic_apply(*caml_extern_sp++);
  }
}

/***/

value ocamlcc_apply_gen(value closure, long nargs, value args[]) {
  long i;
  if(nargs > OCAMLCC_MAXIMUM_ARITY) {
    fprintf(stderr, "Error: invalid callback arity\n");
    exit(1);
  }
  for(i = 1 ; i < nargs ; i ++)
    ocamlcc_global_params[i - 1] = args[i];
  ocamlcc_global_arg_nb_val = Val_long(nargs);
  ocamlcc_global_closure = closure;
  return ocamlcc_generic_apply(args[0]);
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

#define ocamlcc_dynamic_apply(nargs, curr_fsz, next_fsz, dst, env,      \
                              arg1, rest...) {                          \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                         \
  if (Field(env, 1) == Val_long(nargs)) {                               \
    dst ((ocamlcc_fun) Field(env, 0))(arg1, rest, env);                 \
  } else {                                                              \
    ocamlcc_store_args_##nargs(rest, env);                              \
    ocamlcc_global_arg_nb_val = Val_long(nargs);                        \
    ocamlcc_global_special_arg1 = arg1;                                 \
    dst ocamlcc_apply(arg1, env);                                       \
  }                                                                     \
  ocamlcc_apply_restore_stack(next_fsz);                                \
}

#define ocamlcc_partial_apply(nargs, curr_fsz, next_fsz, dst, env,      \
                              arg1, rest...)                            \
  ocamlcc_dynamic_apply(nargs, curr_fsz, next_fsz, dst, env, arg1, rest)

#define ocamlcc_static_apply(nargs, curr_fsz, next_fsz, dst, f, args...) { \
  ocamlcc_apply_init_stack(curr_fsz, next_fsz);                           \
  dst f(args);                                                            \
  ocamlcc_apply_restore_stack(next_fsz);                                  \
}

/***/

#define ocamlcc_dynamic_standard_appterm(nargs, curr_fsz, env, arg1,    \
                                         rest...) {                     \
  caml_extern_sp = sp;                                                  \
  if (Field(env, 1) == Val_long(nargs)) {                               \
    return ((ocamlcc_fun) Field(env, 0))(arg1, rest, env);              \
  } else {                                                              \
    ocamlcc_store_args_##nargs(rest, env);                              \
    ocamlcc_global_arg_nb_val = Val_long(nargs);                        \
    ocamlcc_global_special_arg1 = arg1;                                 \
    return ocamlcc_apply(arg1, env);                                    \
  }                                                                     \
}

#define ocamlcc_partial_standard_appterm(nargs, curr_fsz, env, arg1, \
                                         rest...)                    \
  ocamlcc_dynamic_standard_appterm(nargs, curr_fsz, env, arg1, rest)

#define ocamlcc_static_standard_appterm(nargs, f, env, args...) {       \
  caml_extern_sp = sp;                                                  \
  return f(args, env);                                                  \
}

/***/

#define ocamlcc_dynamic_special_appterm(nargs, tc_nargs, curr_fsz, env, \
                                        arg1, args...)                  \
  ocamlcc_dynamic_standard_appterm(nargs, curr_fsz, env, arg1, args)


#define ocamlcc_partial_special_appterm(nargs, tc_nargs, curr_fsz, env, \
                                        arg1, args...)                  \
  ocamlcc_dynamic_special_appterm(nargs, tc_nargs, curr_fsz, env, arg1, args)

#define ocamlcc_static_special_appterm(nargs, tc_nargs, f, env, args...) \
  ocamlcc_static_standard_appterm(nargs, f, env, args)

/***/

#define ocamlcc_special_special_appterm(nargs, tc_nargs, curr_fsz, env, \
                                        arg1, args...)                  \
  ocamlcc_dynamic_special_appterm(nargs, tc_nargs, curr_fsz, env, arg1, args)

/***/

#define OCAMLCC_SPECIAL_TAIL_CALL_HEADER(id)

#endif /* OCAMLCC_ARCH_ALL */
