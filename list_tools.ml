
(*1.1 Basics*)

(*Length*)

let rec length l = match l with

| [] -> 0

| e::reste -> length reste + 1 ;;

(*length [1;2;3;6;6;0] ;;*)

(*apprend*)

let rec apprend (k,l) = match k with
  | []   -> l
  | e::reste -> e:: apprend (reste,l);;


(*apprend ([1;2;3],[4;5]) ;;*)

(*product*)

let rec product l = match l with
  | [] -> 1
  | e::reste -> e * product reste ;;

(*product [] ;;*)
(*product [1;2;3;4;5] ;;*)

(*nth*)

let nth i l =
let rec aux i l =
if i < 0 then invalid_arg "nth : index must be a natural"
else
match (i,l) with
  |(i,[]) -> failwith "nth : list is too short"
  |(1,e::reste) -> e
  |(_,_::reste) -> aux (i-1) reste
in aux (i+1) l ;;

(*nth 4 [1;3;5;6;7] ;;*)

(*search_pos*)

let search_pos x l =
  let rec aux x l y =
    match l with
      | [] -> 0
      | e::reste -> if e = x then y
        else aux x reste y+1
  in aux x l 0 ;;

(*search_pos 0 [1;2;4;0;5] ;;*)

(*assoc*)

let rec assoc x l = if x < 0 then failwith "assoc : negative degree"
  else
  match l with
    | [] -> 0
    | (a,b)::reste -> if b = x then a
      else assoc x reste ;;
assoc 3 [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;
(*assoc 1 [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;*)
(*assoc (-1) [(4,5) ; (1,2) ; (-3,1) ; (1,0)] ;;*)

(*1.2*)

(*init_list*)

let init_list n x =
  if n < 0 then invalid_arg "init_list : n must be a natural"
  else
    let rec aux n x l = if n = 0 then l
      else
        aux (n-1) x (x::l)
    in aux n x [] ;;

(*init_list 5 0 ;;*)
(*init_list 0 'a' ;;*)
(*init_list  (-5) 1.5 ;;*)

(*put_list*)

let rec put_list v i list =
match (i,list) with
  | (0,e::l) -> v::l
  | (_,[]) -> list
  | (x,e::l) -> e::put_list v (i-1) l ;;

(*put_list 'x' 3 ['-';'-';'-';'-';'-';'-'] ;;*)


(*init_board*)

let rec init_board (l,c) v =
if l < 0 || c < 0 then failwith "init_board = l,c must be > 0"
 else
  let l2 = init_list c v in
  match (l,c) with
  | (0,c) -> []
  | (l,c) -> l2::init_board (l-1,c) v ;;

(*init_board (5,3) 0 ;;*)


(*get_cell*)


let get_cell (x,y) board =
  if x < 0 || y < 0 then failwith "get_cell : x,y should be naturals"
  else
    let z = nth x board
    in nth y z ;;

(*get_cell (2, 1) [[1; 0; 0]; [0; 0; 0]; [0; 2; 0]; [0; 0; 0]; [0; 0; 0]] ;;*)


(*put_cell*)

let put_cell v (x,y) board =
  if x < 0 || y < 0 then failwith "get_cell : x,y should be naturals"
  else
    let z = nth x board in
    let p = put_list v y z
    in put_list p x board ;;

(*put_cell 2 (2, 1) [[1; 0; 0]; [0; 0; 0]; [0; 0; 0]; [0; 0; 0]; [0; 0; 0]] ;;*)

