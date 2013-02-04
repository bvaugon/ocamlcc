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

#include <stdlib.h>
#include <string.h>

extern const int ocamlcc_fun_nb;
extern const void *ocamlcc_fun_ptrs[];

void caml_init_code_fragments();

void ocamlcc_codeptrs_init(void) {
  int i;
  code_t caml_end_code = caml_start_code = (code_t) ocamlcc_fun_ptrs[0];
  for (i = 1 ; i < ocamlcc_fun_nb ; i ++) {
    if ((code_t) ocamlcc_fun_ptrs[i] < caml_start_code)
      caml_start_code = (code_t) ocamlcc_fun_ptrs[i];
    else if ((code_t) ocamlcc_fun_ptrs[i] > caml_end_code)
      caml_end_code = (code_t) ocamlcc_fun_ptrs[i];
  }
  caml_code_size = caml_end_code - caml_start_code + sizeof(char *);
#ifdef OCAMLCC_RUNTIME_VERSION_4_00
  caml_init_code_fragments();
#endif
}
