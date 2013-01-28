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

#ifndef OCAMLCC_H
#define OCAMLCC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <mlvalues.h>
#include "ocamlcc-sp.c"
#include "ocamlcc-signals.c"
#include "ocamlcc-exceptions.c"
#include "ocamlcc-windows.c"
#include "ocamlcc-unimpl.c"
#include "ocamlcc-instrs.c"
#include "ocamlcc-alloc.c"
#include "ocamlcc-apply-gen.c"
#include "ocamlcc-apply-none.c"
#include "ocamlcc-apply-x86x.c"
#include "ocamlcc-main.c"
#include "ocamlcc-byterun.h"
#include "ocamlcc-otherlibs.h"

#ifdef __cplusplus
}
#endif

#endif
