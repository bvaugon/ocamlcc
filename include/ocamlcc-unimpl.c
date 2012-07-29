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

#include "ocamlcc-byterun/fail.h"
#include "ocamlcc-byterun/misc.h"

void ocamlcc_dynlink_error(void) Noreturn;
void ocamlcc_dynlink_error(void) {
  caml_failwith("OCamlCC: dynlink not implemented");
}

CAMLprim value caml_reify_bytecode(value prog, value len) {
  ocamlcc_dynlink_error();
}
CAMLprim value caml_get_current_environment(value unit) {
  ocamlcc_dynlink_error();
}
CAMLprim value caml_invoke_traced_function(value codeptr, value env,
                                           value arg) {
  ocamlcc_dynlink_error();
}
CAMLprim value caml_get_global_data(value unit) {
  ocamlcc_dynlink_error();
}
CAMLprim value caml_get_section_table(value unit) {
  ocamlcc_dynlink_error();
}
CAMLprim value caml_realloc_global(value size) {
  ocamlcc_dynlink_error();
}
