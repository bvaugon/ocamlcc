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

open Printf;;
open Types;;

let fake = { addr = -1 ; index = -1 ; bc = Stop ; is_pointed = false };;

let parse read =
  let read_ptr () = {
    ofs = read ();
    pointed = fake;
  } in
  let opcode = read () in
  try
    match opcode with
      | 0 ->   [ Acc 0 ]
      | 1 ->   [ Acc 1 ]
      | 2 ->   [ Acc 2 ]
      | 3 ->   [ Acc 3 ]
      | 4 ->   [ Acc 4 ]
      | 5 ->   [ Acc 5 ]
      | 6 ->   [ Acc 6 ]
      | 7 ->   [ Acc 7 ]
      | 8 ->   [ Acc (read ()) ]
      | 9 ->   [ Push ]
      | 10 ->  [ Push ]
      | 11 ->  [ Push ; Acc 1 ]
      | 12 ->  [ Push ; Acc 2 ]
      | 13 ->  [ Push ; Acc 3 ]
      | 14 ->  [ Push ; Acc 4 ]
      | 15 ->  [ Push ; Acc 5 ]
      | 16 ->  [ Push ; Acc 6 ]
      | 17 ->  [ Push ; Acc 7 ]
      | 18 ->  [ Push ; Acc (read ()) ]
      | 19 ->  [ Pop (read ()) ]
      | 20 ->  [ Assign (read ()) ]
      | 21 ->  [ Envacc 1 ]
      | 22 ->  [ Envacc 2 ]
      | 23 ->  [ Envacc 3 ]
      | 24 ->  [ Envacc 4 ]
      | 25 ->  [ Envacc (read ()) ]
      | 26 ->  [ Push ; Envacc 1 ]
      | 27 ->  [ Push ; Envacc 2 ]
      | 28 ->  [ Push ; Envacc 3 ]
      | 29 ->  [ Push ; Envacc 4 ]
      | 30 ->  [ Push ; Envacc (read ()) ]
      | 31 ->  [ Pushretaddr (read_ptr ()) ]
      | 32 ->  [ DynamicApply (read ()) ]
      | 33 ->  [ DynamicApply 1 ]
      | 34 ->  [ DynamicApply 2 ]
      | 35 ->  [ DynamicApply 3 ]
      | 36 ->  let n = read () in let s = read () in [ DynamicAppterm (n, s) ]
      | 37 ->  [ DynamicAppterm (1, (read ())) ]
      | 38 ->  [ DynamicAppterm (2, (read ())) ]
      | 39 ->  [ DynamicAppterm (3, (read ())) ]
      | 40 ->  [ Return (read ()) ]
      | 41 ->  [ Restart ]
      | 42 ->  [ Grab (read ()) ]
      | 43 ->
        let n = read () in
        let ptr = read_ptr () in
        [ Closure (n, ptr) ]
      | 44 ->
        let f = read () in
        let v = read () in
        let o = read_ptr () in
        let t = if f = 1 then [||] else
            let t = Array.make (f - 1) (read_ptr ()) in
            for i = 1 to f - 2 do t.(i) <- read_ptr () done ; t
        in
        [ Closurerec (f, v, o, t) ]
      | 45 ->  [ Offsetclosure (-2) ]
      | 46 ->  [ Offsetclosure 0 ]
      | 47 ->  [ Offsetclosure 2 ]
      | 48 ->  [ Offsetclosure (read ()) ]
      | 49 ->  [ Push ; Offsetclosure (-2) ]
      | 50 ->  [ Push ; Offsetclosure 0 ]
      | 51 ->  [ Push ; Offsetclosure 2 ]
      | 52 ->  [ Push ; Offsetclosure (read ()) ]
      | 53 ->  [ Getglobal (read ()) ]
      | 54 ->  [ Push ; Getglobal (read ()) ]
      | 55 ->  let n = read () in let p = read () in [ Getglobalfield (n, p) ]
      | 56 ->
        let n = read () in
        let p = read () in
        [ Push ; Getglobalfield (n, p) ]
      | 57 ->  [ Setglobal (read ()) ]
      | 58 ->  [ Atom 0 ]
      | 59 ->  [ Atom (read ()) ]
      | 60 ->  [ Push ; Atom 0 ]
      | 61 ->  [ Push ; Atom (read ()) ]
      | 62 ->  let n = read () in let t = read () in [ Makeblock (n, t) ]
      | 63 ->  [ Makeblock (1, read ()) ]
      | 64 ->  [ Makeblock (2, read ()) ]
      | 65 ->  [ Makeblock (3, read ()) ]
      | 66 ->  [ Makefloatblock (read ()) ]
      | 67 ->  [ Getfield 0 ]
      | 68 ->  [ Getfield 1 ]
      | 69 ->  [ Getfield 2 ]
      | 70 ->  [ Getfield 3 ]
      | 71 ->  [ Getfield (read ()) ]
      | 72 ->  [ Getfloatfield (read ()) ]
      | 73 ->  [ Setfield 0 ]
      | 74 ->  [ Setfield 1 ]
      | 75 ->  [ Setfield 2 ]
      | 76 ->  [ Setfield 3 ]
      | 77 ->  [ Setfield (read ()) ]
      | 78 ->  [ Setfloatfield (read ()) ]
      | 79 ->  [ Unapp Vectlength ]
      | 80 ->  [ Binapp Getvectitem ]
      | 81 ->  [ Setvectitem ]
      | 82 ->  [ Binapp Getstringchar ]
      | 83 ->  [ Setstringchar ]
      | 84 ->  [ Branch (read_ptr ()) ]
      | 85 ->  [ CondBranch (Branchif (read_ptr ())) ]
      | 86 ->  [ CondBranch (Branchifnot (read_ptr ())) ]
      | 87 ->
        let n = read () in
        let size_tag = n lsr 16 in
        let size_long = n land (1 lsl 16 - 1) in
        let size = size_tag + size_long in
        let tab = Array.init size (fun _ -> read_ptr ()) in
        [ Switch (size_long, size_tag, tab) ]
      | 88 ->  [ Unapp Boolnot ]
      | 89 ->  [ Pushtrap (read_ptr ()) ]
      | 90 ->  [ Poptrap ]
      | 91 ->  [ Raise ]
      | 92 ->  [ Checksignals ]
      | 93 ->  [ Ccall (1, read ()) ]
      | 94 ->  [ Ccall (2, read ()) ]
      | 95 ->  [ Ccall (3, read ()) ]
      | 96 ->  [ Ccall (4, read ()) ]
      | 97 ->  [ Ccall (5, read ()) ]
      | 98 ->  let n = read () in let p = read () in [ Ccall (n, p) ]
      | 99 ->  [ Const 0 ]
      | 100 -> [ Const 1 ]
      | 101 -> [ Const 2 ]
      | 102 -> [ Const 3 ]
      | 103 -> [ Const (read ()) ]
      | 104 -> [ Push ; Const 0 ]
      | 105 -> [ Push ; Const 1 ]
      | 106 -> [ Push ; Const 2 ]
      | 107 -> [ Push ; Const 3 ]
      | 108 -> [ Push ; Const (read ()) ]
      | 109 -> [ Unapp Negint ]
      | 110 -> [ Binapp Addint ]
      | 111 -> [ Binapp Subint ]
      | 112 -> [ Binapp Mulint ]
      | 113 -> [ Binapp Divint ]
      | 114 -> [ Binapp Modint ]
      | 115 -> [ Binapp Andint ]
      | 116 -> [ Binapp Orint ]
      | 117 -> [ Binapp Xorint ]
      | 118 -> [ Binapp Lslint ]
      | 119 -> [ Binapp Lsrint ]
      | 120 -> [ Binapp Asrint ]
      | 121 -> [ Binapp Eq ]
      | 122 -> [ Binapp Neq ]
      | 123 -> [ Binapp Ltint ]
      | 124 -> [ Binapp Leint ]
      | 125 -> [ Binapp Gtint ]
      | 126 -> [ Binapp Geint ]
      | 127 -> [ Unapp (Offsetint (read ())) ]
      | 128 -> [ Offsetref (read ()) ]
      | 129 -> [ Unapp Isint ]
      | 130 -> [ Getmethod ]
      | 131 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Beq (v, ptr)) ]
      | 132 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bneq (v, ptr)) ]
      | 133 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bltint (v, ptr)) ]
      | 134 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bleint (v, ptr)) ]
      | 135 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bgtint (v, ptr)) ]
      | 136 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bgeint (v, ptr)) ]
      | 137 -> [ Binapp Ultint ]
      | 138 -> [ Binapp Ugeint ]
      | 139 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bultint (v, ptr)) ]
      | 140 ->
        let v = read () in
        let ptr = read_ptr () in
        [ CondBranch (Bugeint (v, ptr)) ]
      | 141 ->
        let v = read () in
        let ofs = read () in
        [ Getpubmet (v, ofs) ]
      | 142 -> [ Getdynmet ]
      | 143 -> [ Stop ]
      | 144 -> assert false
      | 145 -> assert false
      | _ -> failwith (sprintf "invalid bytecode (unknown opcode: %d)" opcode)
  with End_of_file ->
    failwith "invalid bytecode (unexpected end of CODE section)"
;;

let hcode_of_bc bc =
  match bc with
    | Acc _ -> 0
    | Push -> 1
    | Pop _ -> 2
    | Assign _ -> 3
    | Envacc _ -> 4
    | Pushretaddr _ -> 5
    | DynamicApply _ -> 6
    | StaticApply _ -> 7
    | PartialApply _ -> 8
    | DynamicAppterm _ -> 9
    | StaticAppterm _ -> 10
    | PartialAppterm _ -> 11
    | SpecialAppterm _ -> 12
    | Return _ -> 13
    | Restart -> 14
    | Grab _ -> 15
    | Closure _ -> 16
    | Closurerec _ -> 17
    | Offsetclosure _ -> 18
    | Getglobal _ -> 19
    | Getglobalfield _ -> 20
    | Setglobal _ -> 21
    | Atom _ -> 22
    | Makeblock _ -> 23
    | Makefloatblock _ -> 24
    | Getfield _ -> 25
    | Getfloatfield _ -> 26
    | Setfield _ -> 27
    | Setfloatfield _ -> 28
    | Setvectitem -> 29
    | Setstringchar -> 30
    | Branch _ -> 31
    | Switch _ -> 32
    | Pushtrap _ -> 33
    | Poptrap -> 34
    | Raise -> 35
    | Checksignals -> 36
    | Ccall _ -> 37
    | Const _ -> 38
    | Offsetref _ -> 39
    | Unapp Boolnot -> 40
    | Unapp (Offsetint _) -> 41
    | Unapp Negint -> 42
    | Unapp Isint -> 43
    | Unapp Vectlength -> 44
    | Binapp Addint -> 45
    | Binapp Subint -> 46
    | Binapp Mulint -> 47
    | Binapp Divint -> 48
    | Binapp Modint -> 49
    | Binapp Andint -> 50
    | Binapp Orint -> 51
    | Binapp Xorint -> 52
    | Binapp Lslint -> 53
    | Binapp Lsrint -> 54
    | Binapp Asrint -> 55
    | Binapp Eq -> 56
    | Binapp Neq -> 57
    | Binapp Ltint -> 58
    | Binapp Leint -> 59
    | Binapp Gtint -> 60
    | Binapp Geint -> 61
    | Binapp Ultint -> 62
    | Binapp Ugeint -> 63
    | Binapp Getvectitem -> 64
    | Binapp Getstringchar -> 65
    | CondBranch (Branchif _) -> 66
    | CondBranch (Branchifnot _) -> 67
    | CondBranch (Beq _) -> 68
    | CondBranch (Bneq _) -> 69
    | CondBranch (Bltint _) -> 70
    | CondBranch (Bleint _) -> 71
    | CondBranch (Bgtint _) -> 72
    | CondBranch (Bgeint _) -> 73
    | CondBranch (Bultint _) -> 74
    | CondBranch (Bugeint _) -> 75
    | Getmethod -> 76
    | Getpubmet _ -> 77
    | Getdynmet -> 78
    | Stop -> 79
;;

let ptr_of_cond_branch cond_branch =
  match cond_branch with
    | Branchif ptr | Branchifnot ptr | Beq (_, ptr) | Bneq (_, ptr)
    | Bltint (_, ptr) | Bleint (_, ptr) | Bgtint (_, ptr) | Bgeint (_, ptr)
    | Bultint (_, ptr) | Bugeint (_, ptr) -> ptr
;;

let ptr_map_cond_branch f cond_branch =
  match cond_branch with
    | Branchif ptr -> Branchif (f ptr)
    | Branchifnot ptr -> Branchifnot (f ptr)
    | Beq (n, ptr) -> Beq (n, f ptr)
    | Bneq (n, ptr) -> Bneq (n, f ptr)
    | Bltint (n, ptr) -> Bltint (n, f ptr)
    | Bleint (n, ptr) -> Bleint (n, f ptr)
    | Bgtint (n, ptr) -> Bgtint (n, f ptr)
    | Bgeint (n, ptr) -> Bgeint (n, f ptr)
    | Bultint (n, ptr) -> Bultint (n, f ptr)
    | Bugeint (n, ptr) -> Bugeint (n, f ptr)
;;
