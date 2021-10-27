(*add_poly*)

let rec add_poly l1 l2 =
match (l1,l2) with
  | ([],_) -> l2
  | (_,[]) -> l1
  | ((a,b)::r1,(y,z)::r2) -> if b = z then
      if a + y <> 0 then (a+y,b)::(add_poly r1 r2) else add_poly r1 r2
 else if b < z then (y,z)::add_poly ((a,b)::r1) r2 else (a,b)::add_poly r1 ((y,z)::r2) ;;

(*let  l1 = [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;
let  l2 = [(2,5) ; (2,4) ; (-1,2) ; (1,1)] ;;
add_poly  l1  l2 ;;*)


(*times*)

let rec times l (x,y) =
match l with
  | [] -> []
  | (a,b)::r -> (a*x,b+y)::times r (x,y) ;;

(*times  [(4 ,5); (1,2);(-3,1);(1,0)] (2, 1);;
times [] (4, 5);;*)


(*product_poly*)

let rec product_poly (l1,l2) =
match (l1,l2) with
  | (l1,[]) -> []
  | ([],l2) -> []
  | l1,(y,z)::[] -> times l1 (y,z)
  | l1,(y,z)::r2 -> add_poly (times l1(y,z)) (product_poly (l1,r2)) ;;

(*let l1 = [(4,5); (1,2);(-3,1);(1,0) ];;
let l2 = [(2,5) ;(2 ,4);(-1,2);(1,1)];;
product_poly (l1,l2);;*)
