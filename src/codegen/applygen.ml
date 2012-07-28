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

module X_ALL = struct
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
      fprintf oc
        "    Ocamlcc_alloc_small(new_closure, %d, Closure_tag,\n" (n+1);
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
    Tailcall.generate oc max_arity;
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

  let export oc max_arity =
    gen_applies oc max_arity
  ;;
end;;

module NO_ARCH = struct
  let gen_store_args oc n =
    fprintf oc "\
#define ocamlcc_store_args_%d(" n;
    if n >= 2 then (
      fprintf oc "a2";
      for i = 3 to n do fprintf oc ", a%d" i done;
    );
    fprintf oc ") { \\\n";
    if n >= 3 then (
      fprintf oc "  ocamlcc_store_args_%d(a2" (n - 1);

      for i = 3 to n - 1 do fprintf oc ", a%d" i done;
      fprintf oc "); \\\n"
    );
    if n >= 2 then (
      fprintf oc "  ocamlcc_global_params[%d] = a%d; \\\n" (n - 2) n;
    );
    fprintf oc "}\n\n";
  ;;

  let gen_applies oc max_arity =
    for i = 1 to max_arity do
      gen_store_args oc i;
    done;
  ;;
end;;

let run oc max_arity =
  match !Options.arch with
    | NO_ARCH ->
      NO_ARCH.gen_applies oc max_arity
    | _ ->
      X_ALL.gen_applies oc max_arity
;;
