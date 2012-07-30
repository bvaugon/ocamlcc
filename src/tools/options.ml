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

let verbose = ref false;;
let stat = ref false;;
let trace = ref false;;
let no_main = ref false;;
let only_generate_C = ref false;;
let keep_C_file = ref false;;
let output_file : string option ref = ref None;;
let input_file : string option ref = ref None;;
let arch = ref NO_ARCH;;
let sigconf = ref Efficient;;
let except = ref Setjmp;;
let ccomp = ref Config.ccomp;;

let offset_counter = ref 0;;
let (message, ofsmsg) =
  let print msg =
    if !verbose then (
      Printf.eprintf "%s%!" msg;
      try offset_counter := String.length msg - String.rindex msg '\n' - 1;
      with Not_found -> offset_counter := String.length msg + !offset_counter
    );
  in
  let message fmt = Printf.ksprintf print fmt in
  let ofsmsg ofs =
    if !verbose then
      let sz = ofs - !offset_counter in
      if sz > 0 then prerr_string (String.make sz ' ');
  in
  (message, ofsmsg)
;;

let (btime, etime) =
  let start = ref 0.0 in
  let btime () = start := Sys.time() in
  let etime () =
    let t = Sys.time() -. !start in
    start := 0.0;
    Printf.sprintf " [%.2fs]" t
  in
  (btime, etime)
;;

let verb_start fmt =
  let () = btime () in
  message fmt
and verb_stop () =
  ofsmsg 40;
  message "%s\n" (etime());
;;

let arch_of_string s =
  match String.lowercase s with
    | "none" -> NO_ARCH
    | "x86" | "i386" | "i486" | "i586" | "i686" -> X86
    | "x86-64" | "x86_64" -> X86_64
    | _ -> invalid_arg "arch_of_string"
;;

let string_of_arch a =
  match a with
    | NO_ARCH -> "none"
    | X86 -> "x86"
    | X86_64 -> "x86_64"
;;

let sigconf_of_string s =
  match String.lowercase s with
    | "e" | "efficient" -> Efficient
    | "r" | "reactive" -> Reactive
    | _ -> invalid_arg "sigconf_of_string"
;;

let except_of_string s =
  match String.lowercase s with
    | "s" | "setjmp" -> Setjmp
    | "t" | "try-catch" -> Trycatch
    | _ -> invalid_arg "except_of_string"
;;

try arch := arch_of_string Config.default_arch
with Invalid_argument _ -> arch := NO_ARCH;;

let default_arch = string_of_arch !arch;;

let arch_option_doc =
  let all_archs = [| NO_ARCH ; X86 ; X86_64 |] in
  let buf = Buffer.create 16 in
  Printf.bprintf buf "<x> Define target architecture [ ";
  Array.iteri (fun i a ->
    if i <> 0 then Printf.bprintf buf " | ";
    Printf.bprintf buf "%s" (string_of_arch a);
    if a = !arch then Printf.bprintf buf " (default)")
    all_archs;
  Printf.bprintf buf " ]";
  Buffer.contents buf
;;
