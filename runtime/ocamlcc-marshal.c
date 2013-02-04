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
  {
    struct code_fragment * cf;
    /* Register the code in the table of code fragments */
    cf = caml_stat_alloc(sizeof(struct code_fragment));
    cf->code_start = (char *) caml_start_code;
    cf->code_end = (char *) caml_start_code + caml_code_size;
    memcpy(cf->digest, OCAMLCC_MD5, 16);
    cf->digest_computed = 1;
    caml_ext_table_init(&caml_code_fragments_table, 8);
    caml_ext_table_add(&caml_code_fragments_table, cf);
  }
#endif
#ifdef OCAMLCC_RUNTIME_VERSION_3_12
  memcpy(caml_code_md5,  OCAMLCC_MD5, 16);
#endif
}
