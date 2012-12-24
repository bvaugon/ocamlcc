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

#ifdef OCAMLCC_SIGNAL_REACTIVE

#define ocamlcc_check_something_to_do(frame_sz) {                       \
  if (caml_something_to_do) {                                           \
    long ocamlcc_save_sp_offset;                                        \
    SaveSp(ocamlcc_save_sp_offset);                                     \
    sp[-frame_sz - 1] = sp[-frame_sz - 2] = sp[-frame_sz - 3] =         \
      sp[-frame_sz - 4] = sp[-frame_sz - 5] =                           \
      sp[-frame_sz - 6] = Val_unit;                                     \
    OffsetSp(-frame_sz - 6);                                            \
    do {                                                                \
      caml_something_to_do = 0;                                         \
      caml_process_event();                                             \
    } while (caml_something_to_do);                                     \
    ResetSp(-frame_sz - 6);                                             \
    RestoreSp(ocamlcc_save_sp_offset);                                  \
  }                                                                     \
}

#define ocamlcc_saved_check_something_to_do(to_save, frame_sz) {        \
  if (caml_something_to_do) {                                           \
    long ocamlcc_save_sp_offset;                                        \
    SaveSp(ocamlcc_save_sp_offset);                                     \
    sp[-frame_sz - 1] = sp[-frame_sz - 2] = sp[-frame_sz - 3] =         \
      sp[-frame_sz - 4] = sp[-frame_sz - 5] = Val_unit;                 \
    sp[-frame_sz - 6] = to_save;                                        \
    OffsetSp(-frame_sz - 6);                                            \
    do {                                                                \
      caml_something_to_do = 0;                                         \
      caml_process_event();                                             \
    } while (caml_something_to_do);                                     \
    ResetSp(-frame_sz - 6);                                             \
    RestoreSp(ocamlcc_save_sp_offset);                                  \
    to_save = sp[-frame_sz - 6];                                        \
  }                                                                     \
}

#else

#define ocamlcc_check_something_to_do(frame_sz)
#define ocamlcc_saved_check_something_to_do(to_save, frame_sz)

#endif
