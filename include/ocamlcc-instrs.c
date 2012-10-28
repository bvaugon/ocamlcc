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

#define Glob(n) Field(caml_global_data, n)
#define GlobField(n, p) Field(Field(caml_global_data, n), p)
#define Offset(clsr, ofs) ((value) (((value *) clsr) + ofs))

/* - */

#define MOVE(src, dst) \
  (dst) = (src);


/* Unary operations */

#define OFFSETINT(n, src, dst) \
  (dst) = (src) + (n << 1);

#define BOOLNOT(src, dst) \
  (dst) = Val_not(src);

#define NEGINT(src, dst) \
  (dst) = (value) (2 - (intnat) (src));

#define ISINT(src, dst) \
  (dst) = Val_long((src) & 1);

#define VECTLENGTH(src, dst) {                  \
  mlsize_t size = Wosize_val(src);              \
  if (Tag_val(src) == Double_array_tag)         \
    size = size / Double_wosize;                \
  (dst) = Val_long(size);                       \
}


/* Binary operations */

#define ADDINT(op1, op2, dst) \
  (dst) = (value) ((intnat) (op1) + (intnat) (op2) - 1);

#define SUBINT(op1, op2, dst) \
  (dst) = (value) ((intnat) (op1) - (intnat) (op2) + 1);

#define MULINT(op1, op2, dst) \
  (dst) = Val_long(Long_val(op1) * Long_val(op2));

#ifdef NONSTANDARD_DIV_MOD

#define DIVINT(op1, op2, dst, frame_sz) {                       \
  intnat divisor = Long_val(op2);                               \
  if (divisor == 0) {                                           \
    caml_extern_sp = sp - frame_sz;                             \
    caml_raise_zero_divide();                                   \
  }                                                             \
  (dst) = Val_long(caml_safe_div(Long_val(op1), divisor));      \
}

#define MODINT(op1, op2, dst, frame_sz) {                       \
  intnat divisor = Long_val(op2);                               \
  if (divisor == 0) {                                           \
    caml_extern_sp = sp - frame_sz;                             \
    caml_raise_zero_divide();                                   \
  }                                                             \
  (dst) = Val_long(caml_safe_mod(Long_val(op1), divisor));      \
}

#else

#define DIVINT(op1, op2, dst, frame_sz) {                       \
  intnat divisor = Long_val(op2);                               \
  if (divisor == 0) {                                           \
    caml_extern_sp = sp - frame_sz;                             \
    caml_raise_zero_divide();                                   \
  }                                                             \
  (dst) = Val_long(Long_val(op1) / divisor);                    \
}

#define MODINT(op1, op2, dst, frame_sz) {                       \
  intnat divisor = Long_val(op2);                               \
  if (divisor == 0) {                                           \
    caml_extern_sp = sp - frame_sz;                             \
    caml_raise_zero_divide();                                   \
  }                                                             \
  (dst) = Val_long(Long_val(op1) % divisor);                    \
}

#endif

#define ANDINT(op1, op2, dst) \
  (dst) = (value) ((intnat) (op1) & (intnat) (op2));

#define ORINT(op1, op2, dst) \
  (dst) = (value) ((intnat) (op1) | (intnat) (op2));

#define XORINT(op1, op2, dst) \
  (dst) = (value) (((intnat) (op1) ^ (intnat) (op2)) | 1);

#define LSLINT(op1, op2, dst) \
  (dst) = (value) ((((intnat) (op1) - 1) << Long_val(op2)) + 1);

#define LSRINT(op1, op2, dst) \
  (dst) = (value) ((((uintnat) (op1) - 1) >> Long_val(op2)) | 1);

#define ASRINT(op1, op2, dst) \
  (dst) = (value) ((((intnat) (op1) - 1) >> Long_val(op2)) | 1);

#define EQ(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) == (intnat) (op2));

#define NEQ(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) != (intnat) (op2));

#define LTINT(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) < (intnat) (op2));

#define LEINT(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) <= (intnat) (op2));

#define GTINT(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) > (intnat) (op2));

#define GEINT(op1, op2, dst) \
  (dst) = Val_int((intnat) (op1) >= (intnat) (op2));

#define ULTINT(op1, op2, dst) \
  (dst) = Val_int((uintnat) (op1) < (uintnat) (op2));

#define UGEINT(op1, op2, dst) \
  (dst) = Val_int((uintnat) (op1) >= (uintnat) (op2));

#define GETVECTITEM(op1, op2, dst) \
  (dst) = Field((op1), Long_val(op2));

#define GETSTRINGCHAR(op1, op2, dst) \
  (dst) = Val_int(Byte_u((op1), Long_val(op2)));


/* Branches and conditional branches */

#define BRANCH(lab) \
  goto lab;

#define BRANCHIF(src, lab) {                            \
  if ((src) != Val_false) goto lab;                     \
}

#define BRANCHIFNOT(src, lab) {                         \
  if ((src) == Val_false) goto lab;                     \
}

#define BEQ(n, src, lab) {                              \
  if ((n) == (intnat) Long_val(src)) goto lab;          \
}

#define BNEQ(n, src, lab) {                             \
  if ((n) != (intnat) Long_val(src)) goto lab;          \
}

#define BLTINT(n, src, lab) {                           \
  if ((n) < (intnat) Long_val(src)) goto lab;           \
}

#define BLEINT(n, src, lab) {                           \
  if ((n) <= (intnat) Long_val(src)) goto lab;          \
}

#define BGTINT(n, src, lab) {                           \
  if ((n) > (intnat) Long_val(src)) goto lab;           \
}

#define BGEINT(n, src, lab) {                           \
  if ((n) >= (intnat) Long_val(src)) goto lab;          \
}

#define BULTINT(n, src, lab) {                          \
  if ((n) < (uintnat) Long_val(src)) goto lab;          \
}

#define BUGEINT(n, src, lab) {                          \
  if ((n) >= (uintnat) Long_val(src)) goto lab;         \
}


/* Function application */

#define DYNAMIC_APPLY(nargs, curr_fsz, next_fsz, dst, args...)  \
  ocamlcc_dynamic_apply(nargs, curr_fsz, next_fsz, dst, args)
  /* See ocamlcc-apply.c */

#define PARTIAL_APPLY(nargs, curr_fsz, next_fsz, dst, args...)  \
  ocamlcc_partial_apply(nargs, curr_fsz, next_fsz, dst, args)
  /* See ocamlcc-apply.c */

#define STATIC_APPLY(nargs, curr_fsz, next_fsz, dst, f, args...)        \
  ocamlcc_static_apply(nargs, curr_fsz, next_fsz, dst, f, args)
  /* See ocamlcc-apply.c */

#define DYNAMIC_STANDARD_APPTERM(nargs, curr_fsz, args...)      \
  ocamlcc_dynamic_standard_appterm(nargs, curr_fsz, args)
  /* See ocamlcc-apply.c */

#define PARTIAL_STANDARD_APPTERM(nargs, curr_fsz, args...)    \
  ocamlcc_partial_standard_appterm(nargs, curr_fsz, args)
  /* See ocamlcc-apply.c */

#define STATIC_STANDARD_APPTERM(f, args...)     \
  ocamlcc_static_standard_appterm(f, args)
  /* See ocamlcc-apply.c */

#define DYNAMIC_SPECIAL_APPTERM(nargs, tc_nargs, curr_fsz, args...)     \
  ocamlcc_dynamic_special_appterm(nargs, tc_nargs, curr_fsz, args)
  /* See ocamlcc-apply.c */

#define PARTIAL_SPECIAL_APPTERM(nargs, tc_nargs, curr_fsz, args...)     \
  ocamlcc_partial_special_appterm(nargs, tc_nargs, curr_fsz, args)
  /* See ocamlcc-apply.c */

#define SPECIAL_SPECIAL_APPTERM(nargs, tc_nargs, curr_fsz, args...)     \
  ocamlcc_special_special_appterm(nargs, tc_nargs, curr_fsz, args)
  /* See ocamlcc-apply.c */

#define STATIC_SPECIAL_APPTERM(nargs, tc_nargs, f, args...)     \
  ocamlcc_static_special_appterm(nargs, tc_nargs, f, args)
  /* See ocamlcc-apply.c */

#define RETURN(src)                             \
  caml_extern_sp = sp;                          \
  return (src);


/* Allocation of blocks */

#define MAKE_YOUNG_BLOCK(size, tag, dst, frame_sz)      \
  Ocamlcc_alloc_small((dst), (size), (tag),             \
                      caml_extern_sp = sp - frame_sz,);

#define MAKE_SAVED_YOUNG_BLOCK(to_save, size, tag, dst, frame_sz)       \
  Ocamlcc_alloc_small((dst), (size), (tag),                             \
                      *(caml_extern_sp = sp - frame_sz - 1) = to_save,  \
                      to_save = *caml_extern_sp);

#define MAKE_BLOCK(size, tag, dst, frame_sz) {          \
  caml_extern_sp = sp - frame_sz;                       \
  (dst) = caml_alloc_shr((size), (tag));                \
}

#define MAKE_SAVED_BLOCK(to_save, size, tag, dst, frame_sz) {   \
  *(caml_extern_sp = sp - frame_sz - 1) = to_save;              \
  (dst) = caml_alloc_shr((size), (tag));                        \
  to_save = *caml_extern_sp;                                    \
}

#define MAKE_YOUNG_FLOAT_BLOCK(size, dst, frame_sz)                     \
  Ocamlcc_alloc_small((dst), (size) * Double_wosize,                    \
                      Double_array_tag,                                 \
                      caml_extern_sp = sp - frame_sz,);

#define MAKE_SAVED_YOUNG_FLOAT_BLOCK(to_save, size, dst, frame_sz)      \
  Ocamlcc_alloc_small((dst), (size) * Double_wosize,                    \
                      Double_array_tag,                                 \
                      *(caml_extern_sp = sp - frame_sz - 1) = to_save,  \
                      to_save = *caml_extern_sp);

#define MAKE_FLOAT_BLOCK(size, dst, frame_sz) {                         \
  caml_extern_sp = sp - frame_sz;                                       \
  (dst) = caml_alloc_shr((size) * Double_wosize, Double_array_tag);     \
}

#define MAKE_SAVED_FLOAT_BLOCK(to_save, size, dst, frame_sz) {          \
  *(caml_extern_sp = sp - frame_sz - 1) = to_save;                      \
  (dst) = caml_alloc_shr((size) * Double_wosize, Double_array_tag);     \
  to_save = *caml_extern_sp;                                            \
}

/* Access to components of blocks */

#define SET_YOUNG_FIELD(ind, block, src)        \
  Field((block), (ind)) = (src);

#define SETFIELD(ind, block, src)               \
  caml_modify(&Field((block), (ind)), (src));

#define INITFIELD(ind, block, src)              \
  caml_initialize(&Field((block), (ind)), (src));

#define SETFLOATFIELD(ind, block, src)                  \
  Store_double_field((block), (ind), Double_val(src));

#define GETFIELD(ind, block, dst)               \
  (dst) = Field((block), (ind));

#define GETFLOATFIELD(ind, block, dst, frame_sz) {               \
  value flt;                                                     \
  double d = Double_field((block), (ind));                       \
  Ocamlcc_alloc_small(flt, Double_wosize, Double_tag,            \
                      caml_extern_sp = sp - frame_sz,);          \
  Store_double_val(flt, d);                                      \
  (dst) = flt;                                                   \
}


/* Access to global variables */

#define SETGLOBAL(ind, src)                             \
  caml_modify(&Field(caml_global_data, (ind)), (src));


/* Signal check */

#define CHECK_SIGNALS(frame_sz) {                       \
  ocamlcc_check_something_to_do(frame_sz);              \
}

#define SAVED_CHECK_SIGNALS(to_save, frame_sz) {                        \
  ocamlcc_saved_check_something_to_do(to_save, frame_sz);               \
}


/* C function call */

#define CCALL(dst, fname, frame_sz, args...) {                         \
  long ocamlcc_save_sp_offset = (char *) caml_stack_high - (char *) sp; \
  caml_extern_sp = sp - frame_sz;                                      \
  dst fname(args);                                                     \
  sp = (value *) ((char *) caml_stack_high - ocamlcc_save_sp_offset);  \
}

#define BIG_CCALL(nargs, dst, fname, frame_sz, args...) {              \
  long ocamlcc_save_sp_offset = (char *) caml_stack_high - (char *) sp; \
  value arg_tbl[nargs] = { args };                                     \
  caml_extern_sp = sp - frame_sz;                                      \
  dst fname(arg_tbl, nargs);                                           \
  sp = (value *) ((char *) caml_stack_high - ocamlcc_save_sp_offset);  \
}


/* Exceptions */

#define RAISE(exn)                              \
  ocamlcc_raise(exn)                       /* See ocamlcc-exceptions.c */

#define PUSHTRAP(restore_exn, lab, ukid)        \
  ocamlcc_pushtrap(restore_exn, lab, ukid) /* See ocamlcc-exceptions.c */

#define POPTRAP(frame_sz)                       \
  ocamlcc_poptrap(frame_sz)                /* See ocamlcc-exceptions.c */

#define SAVED_POPTRAP(to_save, frame_sz)        \
  ocamlcc_saved_poptrap(to_save, frame_sz) /* See ocamlcc-exceptions.c */

#define CATCH_EXCEPTION(lab, restore_exn)       \
  ocamlcc_catch(lab, restore_exn)          /* See ocamlcc-exceptions.c */


/* References */

#define OFFSETREF(ofs, ref) \
  Field((ref), 0) += (ofs) << 1;


/* Array operations */

#define SETVECTITEM(ind, block, src) \
  caml_modify(&Field((block), Long_val(ind)), (src));


/* String operations */

#define SETSTRINGCHAR(ind, str, ch) \
  Byte_u((str), Long_val(ind)) = Int_val(ch);


/* Object-oriented operations */

#define GETMETHOD(tag, obj, meth) \
  (meth) = Field (Field ((obj), 0), Int_val(tag));

#define GETPUBMET(tag, obj, meth) {                     \
  value meths = Field ((obj), 0);                       \
  value vtag = (tag);                                   \
  int li = 3, hi = Field(meths,0), mi;                  \
  while (li < hi) {                                     \
    mi = ((li+hi) >> 1) | 1;                            \
    if (vtag < Field(meths,mi)) hi = mi-2;              \
    else li = mi;                                       \
  }                                                     \
  (meth) = Field (meths, li-1);                         \
}

#define GETDYNMET(tag, obj, meth) \
  GETPUBMET((tag), (obj), (meth))


/* Debugging and machine control */

#define VM_STOP()                               \
  return;
