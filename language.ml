open Mergesort;;



type automata = char list * int list * int list * int list * (int -> char -> int list);; 
(* sigma, Q, I, F, delta *)

let delta q a =
  match q,a with
      0, 'a' -> [0; 1]
    | 0, 'b' -> [1]
    | 0, 'c' -> [1]
    | 1, 'a' -> [1]
    | 1, 'b' -> [0]
    | 1, 'c' -> [1];;

(*--------------------------------------------------------------*)


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

(*--------------------------------------------------------------*)


let check_automata (a : automata) = 
  match a with
      (sigma, q, i, f, delta) -> isInclude i q || isInclude f q;;

(*--------------------------------------------------------------*)

let rec cpt_image_q delta listStates a =
  match listStates with
      [] -> []
    | q::t -> union (delta q a) (cpt_image_q (delta) t a);;


let rec cpt_image_alpha delta q alpha =
  match alpha with
      [] -> []
    | a::tl -> union (delta q a) (cpt_image_alpha (delta) q tl);;

let rec cpt_image delta domain alpha =
  match domain with
      [] -> []
    | q::tl -> union (cpt_image_alpha (delta) q alpha) (cpt_image (delta) tl alpha);;

cpt_image_q (delta) [0;1] 'c';;
cpt_image_q (delta) [0;1] 'b';;


(*--------------------------------------------------------------*)

let rec deps_aux_aux llq1 llq2 a delta = 
  match llq2 with
      [] -> llq1
    | lq::tl -> let e = (cpt_image_q delta lq a) in
        if (isIn e llq1) || (isIn e llq2)
        then deps_aux_aux (lq::llq1) tl a delta
        else deps_aux_aux (lq::llq1) (e::tl) a delta;;

let deps_aux = deps_aux_aux [];;

(* MISTAKE HERE *)
(*let rec deps_aux_deprecated llq a delta = 
  match llq with
      [] -> []
    | lq::t -> if isIn (cpt_image_q delta lq a) llq 
      then lq :: (deps_aux t a delta)
      else lq :: ( deps_aux (List.rev_append t [(cpt_image_q delta lq a)] ) a delta);;*)

let rec deps llq alpha delta =
  match alpha with
      [] -> llq
    | a::tl -> deps (deps_aux llq a delta) tl delta;;

deps_aux [[0];[1]] 'a' delta;;
deps [[0];[1]] ['a';'b';'c'] delta;;




