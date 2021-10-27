#use "list_tools.ml";;

(*decompose *)

let decompose n =
  if n <= 1 then failwith "Decompose : N <=1"
  else
    let rec aux n d =
      if d = n then d::[]
      else
        if n mod d = 0 then d::(aux (n/d) d)
        else aux n (d+1)
    in aux n 2 ;;

(*#trace decompose ;;*)

(*decompose 45 ;;
decompose  150 ;;*)


(*shared*)

let rec shared l1 l2 =
  match (l1,l2) with
    | (_,[]) ->[]
    | ([],_) ->[]
    | (e::r1,d::r2) -> if e = d then e::(shared r1 r2) else
if e < d then shared r1 (d::r2) else shared (e::r1) r2 ;;

(*shared  [1; 3; 6; 6; 8; 9] [2; 5; 6; 6; 8; 9] ;;
shared  [2; 3; 5; 5; 5] [3; 3; 5] ;;
shared  [3; 3; 5; 7] [2; 3; 3; 3; 7; 7] ;;*)


(*PGCD*)

let gcd u v =
  let a = decompose u and b = decompose v in
  let c = shared a b
  in product c ;;

(*gcd 45 150 ;;
gcd  100 7 ;;
gcd 1 7 ;;*)
