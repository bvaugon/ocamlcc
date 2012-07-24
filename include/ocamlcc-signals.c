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

#ifndef OCAMLCC_SIGNALS_H
#define OCAMLCC_SIGNALS_H

#ifdef OCAMLCC_SIGNAL_REACTIVE

#define ocamlcc_check_something_to_do(frame_sz) {                         \
  if (caml_something_to_do) {                                             \
    long ocamlcc_save_sp_offset = (char *) caml_stack_high - (char *) sp; \
    sp[-frame_sz - 1] = sp[-frame_sz - 2] = sp[-frame_sz - 3] =           \
      sp[-frame_sz - 4] = sp[-frame_sz - 5] =                             \
      sp[-frame_sz - 6] = Val_unit;                                       \
    caml_extern_sp = sp - frame_sz - 6;                                   \
    do {                                                                  \
      caml_something_to_do = 0;                                           \
      caml_process_event();                                               \
    } while (caml_something_to_do);                                       \
    sp = (value *) ((char *) caml_stack_high - ocamlcc_save_sp_offset);   \
  }                                                                       \
}

#define ocamlcc_saved_check_something_to_do(to_save, frame_sz) {          \
  if (caml_something_to_do) {                                             \
    long ocamlcc_save_sp_offset = (char *) caml_stack_high - (char *) sp; \
    sp[-frame_sz - 1] = sp[-frame_sz - 2] = sp[-frame_sz - 3] =           \
      sp[-frame_sz - 4] = sp[-frame_sz - 5] = Val_unit;                   \
    sp[-frame_sz - 6] = to_save;                                          \
    caml_extern_sp = sp - frame_sz - 6;                                   \
    do {                                                                  \
      caml_something_to_do = 0;                                           \
      caml_process_event();                                               \
    } while (caml_something_to_do);                                       \
    sp = (value *) ((char *) caml_stack_high - ocamlcc_save_sp_offset);   \
    to_save = sp[-frame_sz - 6];                                          \
  }                                                                       \
}

#else

#define ocamlcc_check_something_to_do(frame_sz)
#define ocamlcc_saved_check_something_to_do(to_save, frame_sz)

#endif
#endif
