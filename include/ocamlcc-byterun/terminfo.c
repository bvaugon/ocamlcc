/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: terminfo.c 6045 2004-01-01 16:42:43Z doligez $ */

/* Read and output terminal commands */


#include <curses.h>
#include <term.h>
#include "config.h"
#include "alloc.h"
#include "fail.h"
#include "io.h"
#include "mlvalues.h"

#define Uninitialised (Val_int(0))
#define Bad_term (Val_int(1))
#define Good_term_tag 0

#if defined (HAS_TERMCAP) && !defined (NATIVE_CODE)

static struct channel *chan;
static char area [1024];
static char *area_p = area;
static int num_lines;
static char *up = NULL;
static char *down = NULL;
static char *caml_standout = NULL;
static char *caml_standend = NULL;

CAMLprim value caml_terminfo_setup (value vchan)
{
  value result;
  static char buffer[1024];
  char *term;

  chan = Channel (vchan);

  term = getenv ("TERM");
  if (term == NULL) return Bad_term;
  if (tgetent(buffer, term) != 1) return Bad_term;

  num_lines = tgetnum ((char *) "li");
  up = tgetstr ((char *) "up", &area_p);
  down = tgetstr ((char *) "do", &area_p);
  caml_standout = tgetstr ((char *) "us", &area_p);
  caml_standend = tgetstr ((char *) "ue", &area_p);
  if (caml_standout == NULL || caml_standend == NULL){
    caml_standout = tgetstr ((char *) "so", &area_p);
    caml_standend = tgetstr ((char *) "se", &area_p);
  }
  Assert (area_p <= area + 1024);
  if (num_lines == -1 || up == NULL || down == NULL
      || caml_standout == NULL || caml_standend == NULL){
    return Bad_term;
  }
  result = caml_alloc_small (1, Good_term_tag);
  Field (result, 0) = Val_int (num_lines);
  return result;
}

static int terminfo_putc (int c)
{
  caml_putch (chan, c);
  return c;
}

CAMLprim value caml_terminfo_backup (value vlines)
{
  int i;

  for (i = 0; i < Int_val (vlines); i++){
    tputs (up, 1, terminfo_putc);
  }
  return Val_unit;
}

CAMLprim value caml_terminfo_standout (value start)
{
  tputs (Bool_val (start) ? caml_standout : caml_standend, 1, terminfo_putc);
  return Val_unit;
}

CAMLprim value caml_terminfo_resume (value vlines)
{
  int i;

  for (i = 0; i < Int_val (vlines); i++){
    tputs (down, 1, terminfo_putc);
  }
  return Val_unit;
}

#else /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */

CAMLexport value caml_terminfo_setup (value vchan)
{
  return Bad_term;
}

CAMLexport value caml_terminfo_backup (value lines)
{
  caml_invalid_argument("Terminfo.backup");
  return Val_unit;
}

CAMLexport value caml_terminfo_standout (value start)
{
  caml_invalid_argument("Terminfo.standout");
  return Val_unit;
}

CAMLexport value caml_terminfo_resume (value lines)
{
  caml_invalid_argument("Terminfo.resume");
  return Val_unit;
}

#endif /* defined (HAS_TERMCAP) && !defined (NATIVE_CODE) */
