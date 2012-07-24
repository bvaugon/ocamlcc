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

#ifndef OCAMLCC_ALLOC_H
#define OCAMLCC_ALLOC_H

#define Ocamlcc_alloc_small(result, wosize, tag, Before_gc, After_gc) {     \
  CAMLassert ((wosize) >= 1);                                               \
  CAMLassert ((tag_t) (tag) < 256);                                         \
  CAMLassert ((wosize) <= Max_young_wosize);                                \
  caml_young_ptr -= Bhsize_wosize (wosize);                                 \
  if (caml_young_ptr < caml_young_limit){                                   \
    caml_young_ptr += Bhsize_wosize (wosize);                               \
    Before_gc;                                                              \
    caml_minor_collection ();                                               \
    After_gc;                                                               \
    caml_young_ptr -= Bhsize_wosize (wosize);                               \
  }                                                                         \
  Hd_hp (caml_young_ptr) = Make_header ((wosize), (tag), Caml_black);       \
  (result) = Val_hp (caml_young_ptr);                                       \
  DEBUG_clear ((result), (wosize));                                         \
}

#endif
