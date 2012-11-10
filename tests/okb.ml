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

let eq_string a b = a = b;;

(******* Quelques definitions du prelude CAML **************)

(* 0- les indispensables *)

exception Failure of string;;

let failwith s = raise(Failure s)
;;

(* 1- les paires *)

(* let fst (x,y) = x
   and snd (x,y) = y
   ;;
*)

(* 2- Les listes *)

(*
  let prefix
  @ L1 L2 = append_rec L1
  where rec append_rec = function
  []  -> L2
  |  a::L -> a :: append_rec L
  ;;
*)

let do_list f = let  rec do_rec = function
[]  -> ()
  | a::xL -> f a; do_rec xL in
                do_rec
;;

let map f = let  rec map_rec = function
[]  -> []
  | a::xL -> f a :: map_rec xL
            in map_rec
;;

let it_list f = let rec it_rec a = function
[]  -> a
  | b::xL -> it_rec (f a b) xL
                in it_rec
;;

let it_list2 f = let rec it_rec a l1 l2 = match (l1,l2) with
    ([],[])    -> a
  | ((a1::q1),(a2::q2)) -> it_rec (f a (a1,a2)) q1 q2
  |     _     -> failwith "it_list2"
                 in it_rec
;;

let fold f = let rec fold_rec a1 = function
[] -> (a1,[])
  | b1::bl ->
    let (a2,c2) = f a1 b1 in
    let (a,cl) = fold_rec a2 bl in
    (a, c2::cl)
             in fold_rec
;;

let exists p = let rec exists_rec = function
[]  -> false
  | a::xL -> (p a) or (exists_rec xL)
               in exists_rec
;;

let for_all p = let rec for_all_rec = function
[]  -> true
  | a::xL -> (p a) & (for_all_rec xL)
                in for_all_rec
;;

let rec rev_append l1 l2 = match l1 with
    []  -> l2
  | (x::q1)  -> rev_append q1 (x::l2)
;;

let rev l = rev_append l []
;;


let rec length = function
[]  -> 0
  | a::xL -> succ(length xL)
;;

let try_find f = let rec try_find_rec = function
[]  -> failwith "try_find"
  | a::l -> try f a with Failure _ -> try_find_rec l
                 in try_find_rec;;

let partition p = let rec part_rec = function
[]  -> [],[]
  | a::xL -> let (pos,neg) = part_rec xL in
             if p a then  a::pos, neg else pos, a::neg
                  in part_rec
;;

(* 3- Les ensembles et les listes d'association *)


let mem eq a = let  rec mem_rec = function
[]  -> false
  | b::xL -> eq (a,b) or mem_rec xL
               in mem_rec;;

let union eq xL1 xL2 =
  let  rec union_rec = function
  []  -> xL2
    | a::xL -> if mem eq a xL2 then union_rec xL else a :: union_rec xL
  in union_rec xL1
;;


let mem_assoc eq a = let rec mem_rec = function
[]    -> false
  | (b,_)::xL -> eq(a,b) or mem_rec xL
                     in mem_rec
;;


let rec assoc eq a =
  function  [] -> failwith "find"
    | (b,d)::l -> if eq(a,b) then d else assoc eq a l
;;
(*

  let mem a = let rec mem_rec = function
  []  -> false
  | b::xL -> a=b or mem_rec xL
  in mem_rec;;

  let union xL1 xL2 = let rec union_rec = function
  []  -> xL2
  | a::xL -> if mem a xL2 then union_rec xL else a :: union_rec xL
  in union_rec xL1
  ;;


  let mem_assoc a = mem_rec where rec mem_rec = function
  []    -> false
  | (b,_)::L -> a=b or mem_rec L
  ;;

  let assoc a = assoc_rec where rec assoc_rec = function
  []    -> failwith "find"
  | (b,d)::L -> if a=b then d else assoc_rec L
  ;;
*)
(* 4- Les sorties *)

(*
  let print_newline () = print_string "\n"; flush std_out
  ;;
*)
let message s = print_string s; print_newline()
;;

let eq_int(a,b)=a==b;;
let eq2_string (a,b)= eq_string a b (*eq_string a b*);;

(* #open "prelude";;
*)

type term = Var of int
            | Term of string * term list;;

let rec eq_term (xM,xN) =
   (* print_string "--->";pretty_term xN;print_string " =? ";pretty_term xM;print_newline();*)
  (match (xM,xN) with
      (Var i),( Var j) -> eq_int(i,j)
    | (Term(s1,l1)), (Term(s2,l2)) -> eq2_string (s1,s2) & eq_term_list(l1,l2)
    | _ -> false)

and eq_term_list = function
[], [] -> true
  | t1::l1, t2::l2 -> eq_term(t1,t2) & eq_term_list(l1,l2)
  | _ -> false
;;

let rec vars = function
Var n -> [n]
  | Term(_,xL) -> vars_of_list xL
and vars_of_list = function
[] -> []
  | t::r -> union eq_int (vars t) (vars_of_list r)
;;

let substitute subst = let rec subst_rec = function
Term(oper,sons) -> Term(oper, map subst_rec sons)
  | Var(n) as t     -> try assoc eq_int n subst with Failure _ -> t
                       in subst_rec
;;

let change f = let rec change_rec l n = match l with
    (h::t) -> (* print_int n; print_string "\n"; *)
      if n==1 then f h :: t else h :: change_rec t (n-1)
  |        _ -> failwith "change"
               in change_rec
;;

(* Term replacement replace xM u xN => xM[u<-xN] *)
let replace xM u xN = let rec reprec = function
_, [] -> xN
  | Term(oper,sons), (n::u) ->
    Term(oper, change (fun xP -> reprec(xP,u)) sons n)
  | _ -> failwith "replace"
                      in reprec(xM,u)
;;

(* matching = - : (term -> term -> subst) *)
let matching term1 term2 =
  let rec match_rec subst = function
  Var v, xM ->
    if mem_assoc eq_int v subst then
      if eq_term (xM ,assoc eq_int v subst) then subst else failwith "matching"
    else
      (v,xM) :: subst
    | Term(op1,sons1), Term(op2,sons2) ->
      if eq2_string (op1,op2) then it_list2 match_rec subst sons1 sons2
      else failwith "matching"
    | _ -> failwith "matching" in
  match_rec [] (term1,term2)
;;

(* A naive unification algorithm *)

let compsubst subst1 subst2 =
  (map (fun (v,t) -> (v, substitute subst1 t)) subst2) @ subst1
;;

let occurs n = let rec occur_rec = function
Var m -> m=n
  | Term(_,sons) -> exists occur_rec sons
               in occur_rec
;;

let rec unify = function
(Var n1 as term1), term2 ->
  if eq_term(term1,term2) then []
  else if occurs n1 term2 then failwith "unify1"
  else [n1,term2]
  | term1, Var n2 ->
    if occurs n2 term1 then failwith "unify2"
    else [n2,term1]
  | Term(op1,sons1), Term(op2,sons2) ->
    if eq2_string (op1,op2) then
      it_list2 (fun s (t1,t2) -> compsubst (unify(substitute s t1,
                                                  substitute s t2)) s)
        [] sons1 sons2
    else failwith "unify3"
;;

(* We need to print terms with variables independently from input terms
   obtained by parsing. We give arbitrary names v1,v2,... to their variables. *)

let xINFIXES = ["+";"*"];;

let rec pretty_term = function
Var n ->
  print_string "v"; print_int n
  | Term (oper,sons) ->
    if mem eq2_string oper xINFIXES then
      match sons with
          [s1;s2] ->
            pretty_close s1; print_string oper; pretty_close s2
        | _ ->
          failwith "pretty_term : infix arity <> 2"
    else
      (print_string oper;
       match sons with
           []   -> ()
         | t::lt -> print_string "(";
           pretty_term t;
           do_list (fun t -> print_string ","; pretty_term t) lt;
           print_string ")")
and pretty_close = function
Term(oper, _) as m ->
  if mem eq2_string  oper xINFIXES then
    (print_string "("; pretty_term m; print_string ")")
  else pretty_term m
  | m -> pretty_term m
;;

(****************** Equation manipulations *************)

(* #open "prelude";;
   #open "terms";;
*)

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule xM xN =
  let all_vars = union eq_int (vars xM) (vars xN) in
  let (k,subst) =
    it_list (fun (i,sigma) v -> (i+1,(v,Var(i))::sigma)) (1,[]) all_vars in
  (k-1, (substitute subst xM, substitute subst xN))
;;

(* checks that rules are numbered in sequence and returns their number *)
let check_rules =
  it_list (fun n (k,_) -> if k==n+1 then k
    else failwith "Rule numbers not in sequence") 0
;;

let pretty_rule (k,(n,(xM,xN))) =
  print_int k; print_string " : ";
  pretty_term xM; print_string " = "; pretty_term xN;
  print_newline()
;;

let pretty_rules = do_list pretty_rule
;;

(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:L=R be an equation, xM be a term such that L<=xM.
   With sigma = matching L xM, we define the image of xM by eq as sigma(R) *)
let reduce xL xM =
  substitute (matching xL xM)
;;

(* A more efficient version of can (rewrite1 (xL,R)) for R arbitrary *)
let reducible xL = let rec redrec xM =
                     try
                       ignore (matching xL xM); true
                     with Failure _ ->
                       match xM with Term(_,sons) -> exists redrec sons
                         |         _     -> false
                   in redrec
;;

(* mreduce : rules -> term -> term *)
let mreduce rules xM =
  let redex (_,(_,(xL,xR))) = reduce xL xM xR in try_find redex rules
;;

(* One step of rewriting in leftmost-outermost strategy, with multiple rules *)
(* fails if no redex is found *)
(* mrewrite1 : rules -> term -> term *)
let mrewrite1 rules = let rec rewrec xM =
                        try
                          mreduce rules xM
                        with Failure _ ->
                          let rec tryrec = function
                          [] -> failwith "mrewrite1"
                            | son::rest ->
                              try
                                rewrec son :: rest
                              with Failure _ ->
                                son :: tryrec rest in
                          match xM with
                              Term(f, sons) -> Term(f, tryrec sons)
                            | _ -> failwith "mrewrite1"
                      in rewrec
;;

(* Iterating rewrite1. Returns a normal form. xMay loop forever *)
(* mrewrite_all : rules -> term -> term *)
let mrewrite_all rules xM = let  rec rew_loop xM =
                              try
                                rew_loop(mrewrite1 rules xM)
                              with Failure _ ->
                                xM
                            in rew_loop xM
;;

(*
  pretty_term (mrewrite_all Group_rules M where M,_=<<A*(I(B)*B)>>);;
  ==> A*U
*)


(*********************** Recursive Path Ordering ****************************)

(* #open "prelude";;
   #open "terms";;
*)
type ordering = Greater | Equal | NotGE;;

let ge_ord order pair = match order pair with NotGE -> false | _ -> true
and gt_ord order pair = match order pair with Greater -> true | _ -> false
and eq_ord order pair = match order pair with Equal -> true | _ -> false
;;

let rem_eq equiv = let rec remrec x = function
[]  -> failwith "rem_eq"
  | y::l -> if equiv (x,y) then l else y :: remrec x l
                   in remrec
;;

let diff_eq equiv (x,y) =
  let rec diffrec = function
  ([],_) as p -> p
    | (h::t, y)   -> try diffrec (t,rem_eq equiv h y)
      with Failure _ ->
        let (x_prime,y_prime) = diffrec (t,y) in (h::x_prime,y_prime) in
  if length x > length y then diffrec(y,x) else diffrec(x,y)
;;

(* multiset extension of order *)
let mult_ext order = function
Term(_,sons1), Term(_,sons2) ->
  (match diff_eq (eq_ord order) (sons1,sons2) with
      ([],[]) -> Equal
    | (l1,l2) ->
      if for_all (fun xN -> exists (fun xM -> order (xM,xN) = Greater) l1) l2
      then Greater else NotGE)
  | _ -> failwith "mult_ext"
;;

(* lexicographic extension of order *)
let lex_ext order = function
((Term(_,sons1) as xM), (Term(_,sons2) as xN)) ->
  let rec lexrec = function
  ([] , []) -> Equal
    | ([] , _ ) -> NotGE
    | ( _ , []) -> Greater
    | (x1::l1, x2::l2) ->
      match order (x1,x2) with
          Greater -> if for_all (fun xN_prime -> gt_ord order (xM,xN_prime)) l2
            then Greater else NotGE
        | Equal -> lexrec (l1,l2)
        | NotGE -> if exists (fun xM_prime -> ge_ord order (xM_prime,xN)) l1
          then Greater else NotGE in
  lexrec (sons1, sons2)
  | _ -> failwith "lex_ext"
;;

(* recursive path ordering *)
(* rpo : (string -> string -> ordering) -> (string -> extension) -> term_pair -> ordering *)
let rpo op_order ext = let rec rporec (xM,xN) =
                         if eq_term(xM,xN) then Equal else
                           match xM with
                               Var m -> NotGE
                             | Term(op1,sons1) ->
                               match xN with
                                   Var n ->
                                     if occurs n xM then Greater else NotGE
                                 | Term(op2,sons2) ->
                                   match (op_order op1 op2) with
                                       Greater ->
                                         if for_all (fun xN_prime -> gt_ord rporec (xM,xN_prime)) sons2
                                         then Greater else NotGE
                                     | Equal ->
                                       ext rporec (xM,xN)
                                     | NotGE ->
                                       if exists (fun xM_prime -> ge_ord rporec (xM_prime,xN)) sons1
                                       then Greater else NotGE
                       in rporec
;;


(*#open "prelude";;
  #open "terms";;
  #open "equations";;
*)
(****************** Critical pairs *********************)

(* All (u,sig) such that xN/u (&var) unifies with M,
   with principal unifier sig *)
(* super : term -> term -> (num list & subst) list *)
let super xM =
  let rec suprec xN = match xN with
      Term(_,sons) as xN ->
        let collate (pairs,n) son =
          ((pairs @
              (map (function (u,xsig) ->
                (n::u,xsig)) (suprec son))),
           n+1) in
        let insides = fst (it_list collate ([],1) sons) in
        (try
           ([], unify(xM,xN)) :: insides
         with Failure _ -> insides)
    | _ -> []
  in suprec
;;

(* Ex :
   let (xM,_) = <<F(A,B)>>
   and (xN,_) = <<H(F(A,x),F(x,y))>> in super xM xN;;
   ==> [[1],[2,Term ("B",[])];                      x <- B
   [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,sig), u&[], such that N/u unifies with xM *)
(* super_strict : term -> term -> (num list & subst) list *)
let super_strict xM = function
Term(_,sons) ->
  let collate (pairs,n) son =
    (pairs @ map (function (u,xsig) -> (n::u,xsig)) (super xM son), n+1) in
  fst (it_list collate ([],1) sons)
  | _ -> []
;;

(* Critical pairs of L1=R1 with L2=R2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (xL1,xR1) (xL2,xR2) =
  let mk_pair (u,xsig) =
    substitute xsig (replace xL2 u xR1), substitute xsig xR2 in
  map mk_pair (super xL1 xL2);;

(* Strict critical pairs of L1=R1 with L2=R2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (xL1,xR1) (xL2,xR2) =
  let mk_pair (u,xsig) =
    substitute xsig (replace xL2 u xR1), substitute xsig xR2 in
  map mk_pair (super_strict xL1 xL2)
;;

(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 =
  (strict_critical_pairs eq1 eq2) @ (critical_pairs eq2 eq1);;

(* Renaming of variables *)

let rename n (t1,t2) =
  let rec ren_rec = function
  Var k -> Var(k+n)
    | Term(op,sons) -> Term(op, map ren_rec sons) in
  (ren_rec t1, ren_rec t2)
;;

(************************ Completion ******************************)

let deletion_message (k,_) =
  print_string "Rule ";print_int k; message " deleted"
;;

(* Generate failure message *)
let non_orientable (xM,xN) =
  pretty_term xM; print_string " = "; pretty_term xN; print_newline()
;;

(* Improved Knuth-Bendix completion procedure *)
(* kb_completion : (term_pair -> bool) -> num -> rules -> term_pair list ->
   (num & num) -> term_pair list -> rules *)

let kb_completion greater =
  let  rec kbrec n rules =
    let normal_form = mrewrite_all rules
    and get_rule k = assoc eq_int k rules in
    let rec process failures = (
      let rec processf (k,l) = (
        let rec processkl eqs = (
     (**
        print_string "***kb_completion "; print_int n; print_newline();
        pretty_rules rules;
        do_list non_orientable failures;
        print_int k; print_string " "; print_int l; print_newline();
        do_list non_orientable eqs;
     **)
          match eqs with
              [] ->
                if k<l then next_criticals (k+1,l) else
                  if l<n then next_criticals (1,l+1) else
                    (match failures with
                        [] -> rules (* successful completion *)
                      | _  -> message "Non-orientable equations :";
                        do_list non_orientable failures;
                        failwith "kb_completion")
            | (xM,xN)::eqs ->
              let xM_prime = normal_form xM
              and xN_prime = normal_form xN
              and enter_rule(left,right) =
                let new_rule = (n+1, mk_rule left right) in
                pretty_rule new_rule;
                let left_reducible (_,(_,(xL,_))) = reducible left xL in
                let redl,irredl = partition left_reducible rules in
                do_list deletion_message redl;
                let irreds = let right_reduce (m,(_,(xL,xR))) =
                               m,mk_rule xL (mrewrite_all (new_rule::rules) xR) in
                             (map right_reduce irredl)

                and eqs_prime = map (function (_,(_,pair)) -> pair) redl in
                kbrec (n+1) (new_rule::irreds) [] (k,l)
                  (eqs @ eqs_prime @ failures)
              in
              if eq_term(xM_prime,xN_prime) then processkl eqs else
                if greater(xM_prime,xN_prime) then enter_rule(xM_prime,xN_prime) else
                  if greater(xN_prime,xM_prime) then enter_rule(xN_prime,xM_prime) else
                    process ((xM_prime,xN_prime)::failures) (k,l) eqs
        ) and next_criticals (k,l) = (
    (**
       print_string "***next_criticals ";
       print_int k; print_string " "; print_int l ; print_newline();
    **)
          try
            let (v,el) = get_rule l in
            if k=l then
              processf (k,l) (strict_critical_pairs el (rename v el))
            else
              try
                let (_,ek) = get_rule k in
                processf (k,l) (mutual_critical_pairs el (rename v ek))
              with Failure "find" (*rule k deleted*) -> next_criticals (k+1,l)
          with Failure "find" (*rule l deleted*) -> next_criticals (1,l+1)
        ) in  processkl
      )  in processf
    ) in process



  in   kbrec
;;

(* complete_rules is assumed locally confluent, and checked Noetherian with
   ordering greater, rules is any list of rules *)

let kb_complete greater complete_rules rules =
  let n = check_rules complete_rules
  and eqs = map (fun (_,(_,pair)) -> pair) rules in
  let completed_rules =
    kb_completion greater n complete_rules [] (n,n) eqs in
  message "Canonical set found :";
  pretty_rules (rev completed_rules);()
;;


(*#open "prelude";;
  #open "terms";;
  #open "equations";;
  #open "orderings";;
  #open "kb";;
*)
let xGroup_rules = [
  1, (1, (Term("*", [Term("U",[]); Var 1]), Var 1));
  2, (1, (Term("*", [Term("I",[Var 1]); Var 1]), Term("U",[])));
  3, (3, (Term("*", [Term("*", [Var 1; Var 2]); Var 3]),
          Term("*", [Var 1; Term("*", [Var 2; Var 3])])))
];;

let xGroup_precedence op1 op2 =
  if eq_string op1  op2 then Equal else
    if (eq_string op1  "I") or (eq_string op2  "U") then Greater else NotGE
;;


(***
    let xGeom_rules = [
    1,(1,(Term ("*",[(Term ("U",[])); (Var 1)]),(Var 1)));
    2,(1,(Term ("*",[(Term ("I",[(Var 1)])); (Var 1)]),(Term ("U",[]))));
    3,(3,(Term ("*",[(Term ("*",[(Var 1); (Var 2)])); (Var 3)]),
    (Term ("*",[(Var 1); (Term ("*",[(Var 2); (Var 3)]))]))));
    4,(0,(Term ("*",[(Term ("A",[])); (Term ("B",[]))]),
    (Term ("*",[(Term ("B",[])); (Term ("A",[]))]))));
    5,(0,(Term ("*",[(Term ("C",[])); (Term ("C",[]))]),(Term ("U",[]))));
    6,(0,
    (Term
    ("*",
    [(Term ("C",[]));
    (Term ("*",[(Term ("A",[])); (Term ("I",[(Term ("C",[]))]))]))]),
    (Term ("I",[(Term ("A",[]))]))));
    7,(0,
    (Term
    ("*",
    [(Term ("C",[]));
    (Term ("*",[(Term ("B",[])); (Term ("I",[(Term ("C",[]))]))]))]),
    (Term ("B",[]))))
    ];;

    let xGeom_rank = function
    "U" -> 0
    | "*" -> 1
    | "I" -> 2
    | "B" -> 3
    | "C" -> 4
    | "A" -> 5
    ;;

    let xGeom_precedence op1 op2 =
    let r1 = xGeom_rank op1
    and r2 = xGeom_rank op2 in
    if r1 = r2 then Equal else
    if r1 > r2 then Greater else NotGE
    ;;

***)

let xGroup_order = rpo xGroup_precedence lex_ext
;;

let greater pair =
  match xGroup_order pair with Greater -> true | _ -> false
;;


for i = 1 to 30 do
  ignore (kb_complete greater [] xGroup_rules)
done
