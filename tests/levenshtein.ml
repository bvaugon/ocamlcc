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

let min3 a b c = min a (min b c);;

let levenshtein m n =
  let d1 = Array.init (String.length n) (fun i -> i) in
  let d0 = Array.make (String.length n) 0 in
  for i = 1 to String.length m - 1 do
    d0.(0) <- i;
    let ui = int_of_char m.[i] in
    for j = 1 to String.length n - 1 do
      d0.(j) <- 1 + min3 d1.(j) d0.(j-1)
        (d1.(j-1) + if ui = int_of_char n.[j] then -1 else 0);
    done;
    Array.blit d0 0 d1 0 (String.length n);
  done;
  d0.(String.length n - 1)
;;

let str = String.create 1500 in
for i = 0 to 299 do
  str.[i] <- char_of_int (int_of_char 'a' + Random.int 26);
done;
for i = 1 to 10 do
  print_char '*'; flush stdout;
  ignore (levenshtein str ("foo" ^ str));
done;
print_newline ();
;;
