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


let check_automata (a : automata) = 
  match a with
      (sigma, q, i, f, delta) -> isInclude i q || isInclude f q;;
		


