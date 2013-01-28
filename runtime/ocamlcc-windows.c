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

#ifdef __MINGW32__

#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <mswsock.h>
#include <config.h>
#include <flexdll.c>

#define sigjmp_buf jmp_buf
#define sigsetjmp(buf,save) setjmp(buf)
#define siglongjmp(env,val) longjmp(env,val)

#define SIG_BLOCK 0
#define SIG_SETMASK 2
int sigemptyset(sigset_t *set){
  printf("Error: sigemptyset not implemented\n");
  exit(1);
}
int sigaddset(sigset_t *set, int signum){
  printf("Error: sigaddset not implemented\n");
  exit(1);
}
int sigdelset(sigset_t *set, int signum){
  printf("Error: sigdelset not implemented\n");
  exit(1);
}
int sigprocmask(int how, sigset_t *set, sigset_t *oldset){
  printf("Error: sigprocmask not implemented\n");
  exit(1);
}

int tputs(char *str, int affcnt, int (*putc)(int)){
  printf("Error: tputs not implemented\n");
  exit(1);
}
int tgetent(char *bp, char *name){
  printf("Error: tgetent not implemented\n");
  exit(1);
}
int tgetnum (char * id){
  printf("Error: tgetnum not implemented\n");
  exit(1);
}
char *tgetstr(char *id, char **area){
  printf("Error: tgetstr not implemented\n");
  exit(1);
}

#endif
