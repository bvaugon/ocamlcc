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

type 'a t = { size : int ; list : 'a list }

let empty = { size = 0 ; list = [] }

let push x { size = size ; list = list } =
  { size = size + 1 ; list = x :: list }

let pop n { size = size ; list = list } =
  if n > size || n < 0 then
    invalid_arg (Printf.sprintf "Stk.pop %d { size = %d }" n size);
  let rec f i list = if i = n then list else f (i + 1) (List.tl list) in
  { size = size - n ; list = f 0 list }

let acc n { size = size ; list = list } =
  if n >= size || n < 0 then invalid_arg "Stk.acc";
  List.nth list n

let assign n x { size = size ; list = list } =
  if n >= size || n < 0 then invalid_arg "Stk.assign";
  let rec f i list =
    if i = 0 then x :: List.tl list else
      match list with
        | [] -> assert false
        | hd :: tl -> hd :: f (i - 1) tl
  in
  { size = size ; list = f n list }

let size stack = stack.size

let push_list l stack =
  let rec f cnt l acc =
    match l with
      | [] -> { size = cnt ; list = acc }
      | hd :: tl -> f (cnt + 1) tl (hd :: acc)
  in
  f stack.size l stack.list

let iter f stack =
  List.iter f stack.list

let iteri f stack =
  let rec loop i l =
    match l with
      | [] -> ()
      | x :: rest -> f i x; loop (i + 1) rest
  in
  loop  0 stack.list

let iter2 f stack stack' =
  if stack.size <> stack'.size then invalid_arg "Stk.iter2";
  List.iter2 f stack.list stack'.list

let map f stack =
  { size = stack.size ; list = List.map f stack.list }

let map2 f stack stack' =
  if stack.size <> stack'.size then invalid_arg "Stk.map2";
  { size = stack.size ; list = List.map2 f stack.list stack'.list }

let fold_left f init stack =
  List.fold_left f init stack.list

let fold_right f stack init =
  List.fold_right f stack.list init

let fold_left2 f init stack stack' =
  if stack.size <> stack'.size then invalid_arg "Stk.fold_left2";
  List.fold_left2 f init stack.list stack'.list

let print pp oc stack =
  let rec f i l =
    match l with
      | [] -> ()
      | hd :: tl -> Printf.fprintf oc "%d: %a\n" i pp hd; f (i + 1) tl
  in
  f 0 stack.list
