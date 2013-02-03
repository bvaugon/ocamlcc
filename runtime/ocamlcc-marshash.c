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

#ifdef OCAMLCC_RUNTIME_VERSION_3_12
#include "ocamlcc-byterun-3.12/intext.h"
#endif

typedef struct {
  void *ocamlcc_marshash_fun_ptr;
  const char *ocamlcc_marshash_hash;
} ocamlcc_marshash_t;

extern int ocamlcc_marshash_fun_nb;
extern ocamlcc_marshash_t ocamlcc_marshash_table[];
extern ocamlcc_marshash_t ocamlcc_marshash_table_inv[];

int ocamlcc_marshash_compare_fun_ptr(ocamlcc_marshash_t *m1,
                                     ocamlcc_marshash_t *m2) {
  if (m1->ocamlcc_marshash_fun_ptr > m2->ocamlcc_marshash_fun_ptr) return 1;
  if (m1->ocamlcc_marshash_fun_ptr < m2->ocamlcc_marshash_fun_ptr) return -1;
  return 0;
}

int ocamlcc_marshash_compare_hash(ocamlcc_marshash_t *m1,
                                  ocamlcc_marshash_t *m2) {
  return memcmp(m1->ocamlcc_marshash_hash, m2->ocamlcc_marshash_hash, 16);
}

void ocamlcc_marshash_init(void) {
  memcpy(ocamlcc_marshash_table_inv, ocamlcc_marshash_table,
         ocamlcc_marshash_fun_nb * sizeof(ocamlcc_marshash_t));
  qsort(ocamlcc_marshash_table, ocamlcc_marshash_fun_nb,
        sizeof(ocamlcc_marshash_t),
        (int (*)(const void*, const void*)) &ocamlcc_marshash_compare_fun_ptr);
  qsort(ocamlcc_marshash_table_inv, ocamlcc_marshash_fun_nb,
        sizeof(ocamlcc_marshash_t),
        (int (*)(const void*, const void*)) &ocamlcc_marshash_compare_hash);
#ifdef OCAMLCC_RUNTIME_VERSION_3_12
  caml_start_code = ocamlcc_marshash_table[0].ocamlcc_marshash_fun_ptr;
  caml_code_size =
    ocamlcc_marshash_table_inv[0].ocamlcc_marshash_fun_ptr -
    ocamlcc_marshash_table[0].ocamlcc_marshash_fun_ptr +
    sizeof(void *);
#endif
}

char *ocamlcc_marshash_digest_of_fun_ptr(void *fun_ptr) {
  ocamlcc_marshash_t key;
  ocamlcc_marshash_t *blk;
  key.ocamlcc_marshash_fun_ptr = fun_ptr;
  blk = bsearch(&key, ocamlcc_marshash_table, ocamlcc_marshash_fun_nb,
                sizeof(ocamlcc_marshash_t),
                (int (*)(const void*, const void*))
                &ocamlcc_marshash_compare_fun_ptr);
  if (blk != NULL) return (char *) blk->ocamlcc_marshash_hash;
  else return NULL;
}

void *ocamlcc_marshash_fun_ptr_of_digest(unsigned char *hash) {
  ocamlcc_marshash_t key;
  ocamlcc_marshash_t *blk;
  key.ocamlcc_marshash_hash = (const char*) hash;
  blk = bsearch(&key, ocamlcc_marshash_table_inv, ocamlcc_marshash_fun_nb,
                sizeof(ocamlcc_marshash_t),
                (int (*)(const void*, const void*))
                &ocamlcc_marshash_compare_hash);
  if (blk != NULL) return blk->ocamlcc_marshash_fun_ptr;
  else return NULL;
}
