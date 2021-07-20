(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 

*)
(* Autor: Jakub Zacharczuk *)


(* Typ będący pustym drzewem lub krotką składającą się z lewego poddrzewa, 
pary poczatek koniec przedzialu, prawego poddrzewa, 
wysokości, ilosc elementów w obu poddrzewach *)
type t =
    | Empty
    | Node of t * (int * int) * t * int * int

(* Tworzy puste drzewo AVL *)
let empty = Empty

(* Zwraca true gdy drzewo jest puste w.p.p false *)
let is_empty = function
    | Empty -> true
    | _ -> false
  
(* Zwraca liczbę elementów w drzewie *)
let size = function
    | Node (_, _, _, _, s) -> s
    | Empty -> 0

(* Zwraca wysokość drzewa *)
let height = function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

(* Zwraca minimum z ilości elementów w przedziale (a,b) i wartości max_int *)
let cnt_size (a, b) =
    if b - a + 1 <= 0 then max_int else b - a + 1

(* Zwraca minimum z sumy liczb a i b i max_int *)
let sum a b =
    if a + b <= 0 then max_int else a + b

(* Tworzy nowe drzewo AVL łącząc l - lewe poddrzewo, k - przedział, 
r - prawe poddrzewo oraz aktualizuje wysokość i liczbę elementów 
drzewa l i r mogą różnić się wysokością o co najwyżej 1 *)
let make l k r = 
    Node (l, k, r, sum (max (height l) (height r)) 1,
        sum (size l) (sum (size r) (cnt_size k)))

(* Zwraca zbalansowane drzewo AVL dla poddrzew l r i przedzialu k *)
(* l i r to dowolne drzewa *)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        |Node (ll, lk, lr, _, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
                (match lr with
                | Node (lrl, lrk, lrr, _, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        |Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rk, rr, _, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
                (match rl with
                | Node (rll, rlk, rlr, _, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else Node (l, k, r, max hl hr + 1, sum (size l) (sum (size r) (cnt_size k)) )

(* Zwraca najmniejszy przedział w drzewie *)
let rec min_elt = function
    | Node (Empty, k, _, _, _) -> k
    | Node (l, _, _, _, _) -> min_elt l
    | Empty -> raise Not_found

(* Usuwa z drzewa przedział o najmniejszej wartości *)
let rec remove_min_elt = function
    | Node (Empty, _, r, _, _) -> r
    | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
    | Empty -> invalid_arg "PSet.remove_min_elt"
  
(* Zwraca największy predział z drzewa *)
let rec max_elt = function
    | Node (_, k, Empty, _, _) -> k
    | Node (_, _, r, _, _) -> max_elt r
    | Empty -> raise Not_found

(* Usuwa z drzewa przedział o największej wartości *)
let rec remove_max_elt = function
    | Node (l, _, Empty, _, _) -> l
    | Node (l, k, r, _, _) -> bal l k(remove_max_elt r)
    | Empty -> invalid_arg "PSet.remove_max_elt"

(* Zwraca drzewo AVL z dodanym przedziałem przy założeniu że nie ma on "sąsiednich" przedziałów 
x jest dowolnym przedziałem i pobierane drzewo jest drzewem AVL *)
let rec add_one x = function
    | Node (l, k, r, h, s) ->
        if x = k then Node (l, x, r, h, s)
        else if x < k then bal (add_one x l) k r
        else bal l k (add_one x r)
    | Empty -> Node (Empty, x, Empty, 1, cnt_size x)
 
(* Zwraca drzewo AVL powstałe w wyniku połączenia l - lewego poddrzewa, v - przedziału i r - prawego poddrzewa *)
(* Lewe poddrzewo i prawe poddrzewo to drzewa AVL *)
let rec join l v r =
    match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
        make l v r

(* Dla wartości x : int zwraca krotkę [L, pres, R] oznaczająca
  zbiór mniejszych elementów niż x w s, true jeśli x należy do drzewa s, false w.p.p, 
  zbiór większych elementów niż x w s*)
let split x s =
    let rec loop x = function
        | Empty -> (Empty, false, Empty)
        | Node (l, (a, b), r, _, _) ->
            if a <= x && x <= b then(
                let ll = (if a = x then l
                         else add_one (a, x-1) l)
                and rr = (if (b = x) then r
                         else add_one (x+1, b) r) in
                (ll, true, rr))
            else if x < a then
                let (ll, pres, rr) = loop x l in
                (ll, pres, join rr (a, b) r)
            else
                let (ll, pres, rr) = loop x r in
                (join l (a, b) ll, pres, rr) in
    loop x s

(* Zwraca true jeśli wartość x znajduje się w drzewie, false w.p.p.*)
let mem x s =
    let rec loop = function
        | Node (l, (vl, vr), r, _, _) ->
            if vl <= x && x <= vr then true
            else if x < vl then loop l
            else loop r
        | Empty -> false in
    loop s

(*Zwraca drzewo AVL po dodaniu przedziału (x, y) do drzewa AVL s *)
let add (x, y) s = 
    let (ll, presl, _) = split x s in
    let (_, presr, rr) = split y s in
    let (left, nx) = if mem (x - 1) ll = true then
                         (remove_max_elt ll, fst (max_elt ll))
                     else (ll, x) in
    let (right, ny) =  if mem (y + 1) rr = true then
                              (remove_min_elt rr, snd (min_elt rr))
                          else (rr, y) in
    join left (nx, ny) right

(* Zwraca drzewo AVL otrzymane z s bez elementów z przedziału [x,y] *)
let remove (x, y) s =
    let (ll, _, _) = split x s in
    let (_, _, rr) = split y s in
    if ll = Empty then rr
    else if rr = Empty then ll
    else let elt = (max_elt ll) in
    join (remove_max_elt ll) elt rr
  
(* Zwraca liczbę wartości z drzewa które są mniejsze od x *)
let below x s = 
    if x = max_int then size s else
    let (l, _, _) = (split (x + 1) s) in
    size l
    
(* Aplikuje funkcję f na wszystkich przedziałach s *)
let iter f s =
    let rec loop = function
        | Empty -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r in
    loop s

(* Składa funkcję f na przedziałach s *)
let fold f s acc =
    let rec loop acc = function
        | Empty -> acc
        | Node (l, k, r, _, _) ->
            loop (f k (loop acc l)) r in
    loop acc s

(* Zwraca posortowaną listę przedziałów w rosnącej kolejności *)
let elements s = 
    let rec loop acc = function
        | Empty -> acc
        | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
    loop [] s 
  
(*let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end
let a = empty;;
let a = add (2, 5) a;;
let a = add (7, 10) a;;
let a = add (12, 20) a;;
let a = add (0, 0) a;;

test 1 (mem 1 a = false);;
test 2 (mem 2 a = true);;
test 3 (mem 9 a = true);;
test 4 (mem 21 a = false);;

let elem = elements a;;

test 5 (elem = [(0,0);(2,5);(7,10);(12,20)]);;
test 6 (below 6 a == 5);;
test 7 (below 10 a == 9);;
test 8 (below 19 a == 17);;

let (l,_,r) = split 15 a;;

test 9 (elements l = [(0,0);(2,5);(7,10);(12,14)]);;
test 10 (elements r = [(16,20)]);;

let (l,_,r) = split 8 a;;

test 11 (elements l = [(0,0);(2,5);(7,7);]);;
test 12 (elements r = [(9,10);(12,20)]);;


let a = add (6, 6) a;;
let b = add (11, 11) a;;

test 13 (elements a = [(0,0);(2,10);(12,20)]);;
test 14 (elements b = [(0,0);(2,20)]);;*)
