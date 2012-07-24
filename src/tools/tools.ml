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

module Id = struct
  type t = int
  let counter = ref 0
  let create () = let n = !counter in counter := n + 1; n
end;;

(***)

module Int = struct
  type t = int
  let compare (x : int) (y : int) = compare x y
end;;

module ISet = Set.Make (Int);;
module IMap = Map.Make (Int);;
module SMap = Map.Make (String);;
