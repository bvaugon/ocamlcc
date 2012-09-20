(*************************************************************************)
(*                                                                       *)
(*                               OCamlCC                                 *)
(*                                                                       *)
(*                    Michel Mauny, Benoit Vaugon                        *)
(*                          ENSTA ParisTech                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

Printexc.record_backtrace true;;

let set_arch = ref (fun _ -> ());;
let set_sigconf = ref (fun _ -> ());;
let set_exception = ref (fun _ -> ());;

(***)

let usage = Printf.sprintf "Usage: %s [ OPTIONS ] [ <src.byte> ]" Sys.argv.(0);;

let spec =
  Arg.align [
    ("-c", Arg.Set Options.only_generate_C,
     " Stop after generation of C code");
    ("-k", Arg.Set Options.keep_C_file,
     " Keep C intermediate file");
    ("-o", Arg.String (fun o -> Options.output_file := Some o),
     "<file> Output goes to <file>");
    ("-cc", Arg.Set_string Options.ccomp,
     Printf.sprintf "<x> Define C compiler (default: %s)" Config.ccomp);
    ("-arch", Arg.String (fun a -> !set_arch a),
     Options.arch_option_doc);
    ("-signal", Arg.String (fun s -> !set_sigconf s),
     "<x> Define signal reactivity [ R[eactive] | E[fficient] (default) ]");
    ("-exception", Arg.String (fun s -> !set_exception s),
     "<x> Define exception mechanism [ S[etjmp] (default) | T[ry-catch] ]");
    ("-trace", Arg.Set Options.trace,
     " Generate additional C code to trace execution");
    ("-no-main", Arg.Set Options.no_main,
     " Do not include the main function");
    ("-no-xconst", Arg.Set Options.no_xconst,
     " Do not perform constant extraction");
    ("-include", Arg.Unit (fun () -> print_endline Config.include_dir; exit 0),
     " Print include directory and exit");
    ("-stat", Arg.Set Options.stat,
     " Print statistics");
    ("-verbose", Arg.Set Options.verbose,
     " Verbose mode");
    ("-version", Arg.Unit (fun () -> print_endline Config.version; exit 0),
     " Print version and exit");
    ("-v", Arg.Unit
      (fun () ->
        Printf.printf "OCamlCC version %s\n" Config.version;
        Printf.printf "Default arch: %s\n" Options.default_arch;
        Printf.printf "Based on the %s runtime\n" Config.runtime_version;
        Printf.printf "C compiler: %s\n" Config.ccomp;
        Printf.printf "Include directory: %s\n" Config.include_dir;
        exit 0),
     " Print directories and versions, then exit");
  ]
;;

(***)

let error fmt =
  let k msg =
    Printf.eprintf "Error: %s\n" msg;
    Arg.usage spec usage;
    exit 1;
  in
  Printf.ksprintf k fmt
;;

let unknow arg =
  if !Options.input_file <> None then error "invalid argument: %s" arg;
  Options.input_file := Some arg;
;;

(***)

let () = begin
  set_arch :=
    (fun s ->
      try Options.arch := Options.arch_of_string s
      with Invalid_argument _ ->
        error "invalid target architecture: %S" s);
  set_sigconf :=
    (fun s ->
      try Options.sigconf := Options.sigconf_of_string s
      with Invalid_argument _ ->
        error "invalid signal configuration: %S" s);
  set_exception :=
    (fun s ->
      try Options.except := Options.except_of_string s
      with Invalid_argument _ ->
        error "invalid exception configuration: %S" s);
  Arg.parse spec unknow usage;
end;;

(***)

let input_file =
  match !Options.input_file with
    | None -> error "input file expected"
    | Some file -> file
;;

let inputting_bytecode =
  not(Filename.check_suffix input_file ".c")
;;

let cfile =
  if !Options.only_generate_C then
    let () = if not inputting_bytecode then
        error "-c option has no effect: %S is a C file" input_file in
    match !Options.output_file with
      | None ->
        begin try Filename.chop_extension input_file ^ ".c"
          with Invalid_argument _ -> input_file ^ ".c" end
      | Some file ->
        if not (Filename.check_suffix file ".c") then
          error "file %S should have extension .c" file;
        file
  else
    if inputting_bytecode then
      let (filename, oc) = Filename.open_temp_file "tmp-ocamlcc-" ".c" in
      close_out oc;
      filename
    else
      let () = Options.keep_C_file := true in
      input_file
;;

(***)

let cc_run args =
  let compilation =
    Printf.sprintf
      "%s %s -I %s -O3 -Wall -fno-omit-frame-pointer -lm -ldl -lcurses -Wl,-E"
      !Options.ccomp args Config.include_dir
  in
  Options.message "+ Running %S..." compilation;
  let ret_code = Sys.command compilation in
  Options.message " done\n";
  ret_code
;;

(***)

let b2c bfile cfile stop =
  let (prims, data, code, dbug) = Loader.load bfile in
  let funs = Block.create code in
  (* WARNING: compute_applies must be called before remap_stack *)
  (* WARNING: compute_applies change bytecode in place *)
  Static.compute_applies data funs;
  (* WARNING: remap_stack change bytecode in place *)
  Stkmap.remap_stack funs;
  Rmclsrs.run funs;
  let (dzeta_code, fun_tys) = Xconst.run prims funs in
  let fact_funs = Factfun.factor_functions funs in
  let max_arity = Block.compute_maximum_arity fact_funs in
  Codegen.run cfile prims data dbug funs dzeta_code max_arity;
  if !Options.stat then Stat.run stdout funs dzeta_code fun_tys;
  if stop then exit 0;
;;

try
  (* Generate C *)
  if inputting_bytecode then
    b2c input_file cfile !Options.only_generate_C;
  (* Call the C compiler *)
  let ccargs = match !Options.output_file with
    | None -> cfile
    | Some fname -> Printf.sprintf "%s -o %s" cfile fname in
  let ret_code = cc_run ccargs in
  if not (!Options.keep_C_file) then (
    Options.message "+ Removing %s..." cfile;
    begin try Sys.remove cfile with Sys_error _ -> () end;
    Options.message " done\n";
    exit ret_code
  );
with Failure msg | Sys_error msg ->
  Printf.eprintf " fail\nError: %s\n" msg;
  exit 1;
;;
