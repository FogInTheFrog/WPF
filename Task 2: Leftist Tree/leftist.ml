(* Autor Jakub Zacharczuk *)

(* Typ queue przechowuje
   Node który zawiera:
      - priorytet węzła
      - prawą wysokość poddrzewa zaczepionego w tym węźle
      - lewe poddrzewo
      - prawe poddrzewo
   albo
   Leaf - który jest pustym drzewem *)
type 'a queue = Node of 'a * int * 'a queue * 'a queue | Leaf

(* wyjatek zachodzący w przypadku próby usunięcia elementu z pustego drzewa *)
exception Empty

(* Funkcja zwracająca puste drzewo *)
let empty = Leaf

(* Funkcja zwracająca prawą wysokość poddrzewa zaczepionego w węźle a *)
let zwroc_wysokosc a =
  match a with
  | Leaf -> (-1)
  | Node(_, wys, _, _) -> wys

(* Funkcja zwracająca drzewo powstałe w wyniku połączenia dwóch drzew a i b *)
let rec join a b =
  match a, b with
  | x, Leaf -> x
  | Leaf, x -> x
  | Node(a_x, a_odl, a_left, a_right), Node(b_x, b_odl, b_left, b_right) ->
    if a_x <= b_x then
      match join a_right b with
      | Leaf -> Leaf
      | Node(c_x, c_odl, c_left, c_right) ->
        let lewe_pod_odl = zwroc_wysokosc a_left in
        if lewe_pod_odl <= c_odl then 
          Node(a_x, lewe_pod_odl + 1, Node(c_x, c_odl, c_left, c_right), a_left)
        else Node(a_x, c_odl + 1, a_left, Node(c_x, c_odl, c_left, c_right))
    else join b a

(* Funkcja, która wstawia element a do drzewa b *)
let add a b = join (Node(a, 0, Leaf, Leaf)) b

(* Funkcja usuwa minimalny element drzewa a oraz zwraca drzewo bez tego elementu
    lub podnosi wyjątek jeśli drzewo jest puste *)
let delete_min a = 
  match a with
  | Leaf -> raise Empty
  | Node(a_x, a_odl, a_left, a_right) ->
    (a_x, join a_left a_right)

(* Funkcja sprawdza czy drzewo a jest puste *)
let is_empty a = 
    if a = Leaf then true
    else false


(* Testy *)
(*
exception WA;;

let test q l =
    try
      let (b, nq) = List.fold_left (fun a x -> 
        let (e, nq) = delete_min (snd a)
        in 
        if(compare x e != 0) then raise WA 
        else (true, nq)) 
                                   (true, q) l
      in
      b && (is_empty nq)
    with WA -> false
;;
  
  let rec add_lots f a l iter =
  if iter = 0 then a
  else
    add_lots f (List.fold_left (fun q x -> f x q) a l) l (iter - 1)

let size q =
  let rec aux q a =
    if is_empty q then a
    else aux (snd (delete_min q)) (a + 1)
  in
  aux q 0

let app h t = h::t

let q1 = empty |> add 3 |> add 5 |> add 10 |> add 2 |> add 2 |> add 7 |> add 22
let q2 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [3; 5; 10; 2; 2; 7; 22]
let l2 = List.sort compare [1; 2; 3; 4; 5]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

let q1 = empty |> add 2.3 |> add 1.1 |> add 3.2 |> add 0.01 |> add 222.1 |> add 42.42 |> add 1.03
let q2 = empty |> add 1.5 |> add 2.4 |> add 3.3 |> add 4.2 |> add 5.1

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [2.3; 1.1; 3.2; 0.01; 222.1; 42.42; 1.03]
let l2 = List.sort compare [1.5; 2.4; 3.3; 4.2; 5.1]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

*)
