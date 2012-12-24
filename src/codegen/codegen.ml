(*************************************************************************)
(*                                                                       *)
(*                               OCamlCC                                 *)
(*                                                                       *)
(*                    Michel Mauny, Benoit Vaugon                        *)
(*                          ENSTA ParisTech                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

open Types;;
open Macroc;;

let gen_code cfile macroc =
  Options.verb_start "+ Generating %S..." cfile;
  let oc = open_out cfile in
  if !Options.no_main then Printf.fprintf oc "#define OCAMLCC_NO_MAIN\n";
  if !Options.global_sp then Printf.fprintf oc "#define OCAMLCC_GLOBAL_SP\n";
  Printf.fprintf oc "#define OCAMLCC_GLOBAL_DATA_LENGTH %d\n"
    (String.length macroc.mc_data);
  Printf.fprintf oc "#define OCAMLCC_MAXIMUM_ARITY %d\n" macroc.mc_max_arity;
  Printf.fprintf oc "#define OCAMLCC_ARCH_%a\n" Printer.print_arch
    !Options.arch;
  Printf.fprintf oc "#define OCAMLCC_SIGNAL_%a\n" Printer.print_sigconf
    !Options.sigconf;
  Printf.fprintf oc "#define OCAMLCC_EXCEPTION_%a\n" Printer.print_except
    !Options.except;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "#if !defined(__GNUC__)\n";
  Printf.fprintf oc
    "  #error - Incompatible code: compiler should be GNU C compatible\n";
  Printf.fprintf oc "#endif\n";
  begin match !Options.arch with
    | GEN_ARCH | NO_ARCH -> ()
    | X86 ->
      Printf.fprintf oc
        "#if (!defined(__i386__) && !defined(__i486__)     \\\n     \
              && !defined(__i585__) && !defined(__i686__))\n";
      Printf.fprintf oc
        "  #error - Incompatible code: architecture should be x86\n";
      Printf.fprintf oc "#endif\n\n";
    | X86_64 ->
      Printf.fprintf oc
        "#if !defined(__x86_64__)\n";
      Printf.fprintf oc
        "  #error - Incompatible code: architecture should be x86-64\n";
      Printf.fprintf oc "#endif\n";
  end;
  Printf.fprintf oc "\n";
  Printf.fprintf oc "#include <ocamlcc.h>\n\n";
  Mcprinter.print_macroc oc macroc;
  close_out oc;
  Options.verb_stop ();
;;
