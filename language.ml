open Mergesort;;



type automata = char list * int list * int list * int list * (int -> char -> int list);; 
(* sigma, Q, I, F, delta *)

let rec isIn a b =
  match (a,b) with
      _, [] -> false
    | n, hd::tl -> n = hd || isIn n tl;;

let rec isInclude a b =
  match (a,b) with
      [], _ -> true
    | hd::tl, b -> isIn hd b && isInclude tl b;;

(* list must be sorted (grow) *)
let rec union a b =
  match (a,b) with
      [], b -> b
    | a, [] -> a
    (*| hd1::tl1, hd1::tl2 -> hd1::(union tl1 tl2)*)
    | hd1::tl1, hd2::tl2 -> 
        if hd1 = hd2 then hd1::(union tl1 tl2) 
        else if hd1 > hd2 then hd2::(union (hd1::tl1) tl2) else hd1::(union tl1 (hd2::tl2) );;

let check_automata (a : automata) = 
  match a with
      (sigma, q, i, f, delta) -> isInclude i q || isInclude f q;;





