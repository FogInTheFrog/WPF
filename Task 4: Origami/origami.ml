(* Autor: Jakub Zacharczuk *)

(* Typ reprezentujący punkt na płaszczyźnie *)
type point = float * float

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy *)
let prostokat ((x1, y1):point) ((x2, y2):point) =
    fun ((x, y):point) -> 
        if x1 <= x && x <= x2 && y1 <= y && y <= y2 then 1
        else 0

let square x = x *. x

let eps = 1. /. 100000000.

(** [kolko p r] zwraca kartkę, reprezentującą 
kółko domknięte o środku w punkcie [p] i promieniu [r] *)
let kolko ((xr, yr):point) r =
    fun (x, y) ->
        if square (xr -. x) +. square (yr -. y) -. square r <= eps then 1
        else 0

(* det p1 p2 p zwraca iloczyn wektorowy u x v wektorów u = [p1 p2] v = [p1 p]
jeśli punkt p leży na prawo od prostej przechodzącą przez punkty p1 p2 to
wartość det jest dodatnia, jeśli p leży na prostej wtedy wartość jest równa 0
natomiast jeśli punkt p leży na lewo od prostej wtedy wartość det jest ujemna
z założenia p1 <> p2*)
let det ((xa, ya):point) ((xb, yb):point) ((x, y):point) = 
    let x1 = (xb -. xa) in
    let y1 = (yb -. ya) in
    let x2 = (x -. xa) in
    let y2 = (y -. ya) in
    x1 *. y2 -. x2 *. y1;;

(* symetria p1 p2 p zwraca punkt na płaszczyźnie który jest symetrią punktu p
względem prostej przechodzącej przez punkty p1 p2, z założenia p1 <> p2*)
let symetria ((xa, ya):point) ((xb, yb):point) ((x, y):point) =
    if ya = yb then 
    let y_prim = y +. 2. *. (yb -. y) in
    (x, y_prim) else
    if xa = xb then
    let x_prim = x +. 2. *. (xb -. x) in
    (x_prim, y) else
    let wspl_kier = (yb -. ya) /. (xb -. xa) in
    let wyraz_wolny = yb -. wspl_kier *. xb in
    let a_prostopadle = (0. -. 1.) /. wspl_kier in
    let b_prostopadle = y -. a_prostopadle *. x in
    let roznica_b = (b_prostopadle -. wyraz_wolny) in
    let roznica_a = (wspl_kier -. a_prostopadle) in
    let x_przeciecie = roznica_b /. roznica_a in
    let y_przeciecie = a_prostopadle *. x_przeciecie +. b_prostopadle in
    (2. *. x_przeciecie -. x, 2. *. y_przeciecie -. y);;

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia. *)
let zloz pa pb k =
    fun p ->
        let iloczyn = det pa pb p in
        if iloczyn > eps then
            k p + (k (symetria pa pb p) ) else
        if iloczyn >= (-.eps) && iloczyn <= eps then k p else 0

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *) 
let skladaj lista k =
    List.fold_left (fun k_next (p1, p2) -> zloz p1 p2 k_next) k lista

(*
let x = prostokat (-16., -16.) (16., 16.);;

let a = (0., -16.);;
let b = (0., 16.);;
let c = (-16., 0.);;
let d = (16., 0.);;

let x = skladaj [(a,d);(d,b);(b,c);(c,a)] x;;

assert (x (0., 0.) = 5);;
assert (x (6., 0.) = 3);;
assert (x a = 1);;
assert (x (-16., -16.) = 0);;
assert (x (-8., 8.) = 1);;

let a = (-8., -8.);;
let b = (8., 8.);;
let c = (-8., 8.);;
let d = (8., -8.);;

let x = skladaj [(a,d);(d,b);(b,c);(c,a)] x;;

assert (x (0., 0.) = 9);;
assert (x (6., 0.) = 6);;
assert (x a = 1);;
assert (x (-16., -16.) = 0);;
assert (x (0., 8.) = 3);;
*)
