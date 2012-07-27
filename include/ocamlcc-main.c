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

#include "ocamlcc-byterun/mlvalues.h"
#include "ocamlcc-byterun/memory.h"
#include "ocamlcc-byterun/stacks.h"
#include "ocamlcc-byterun/callback.h"
#include "ocamlcc-byterun/signals.h"

void ocamlcc_bytecode_main(void);

value ocamlcc_main() {
  CAMLparam0();
  CAMLlocal1(result);
  value *sp = caml_extern_sp;
  ocamlcc_exception_init();
  ocamlcc_pushtrap(result = Make_exception_result(caml_exn_bucket),
                   catch_label, 0_main_0);
  ocamlcc_bytecode_main();
  ocamlcc_poptrap(0);
  caml_extern_sp = sp;
  result = Val_unit;
  goto end;
  ocamlcc_catch(catch_label, result = Make_exception_result(exn));
  caml_extern_sp = sp;
 end:
  CAMLreturn(result);
}
