(* Autor: Jakub Zacharczuk *)


(* Typ wartość przechowuje przedział możliwych wartości pomiaru wielkosci fizycznej
float * float to odpowiednio początek i koniec przedziału
int przyjmuje wartosci [0; 1; 2]
0 - wynik jest typu nan
1 - wynik przyjmuje wartości z przedziału [float, float]
2 - wynik przyjmuje wartości od [neg_infinity, float] U [float, infinity] *)

type wartosc = int * float * float;;  

(*moje konstruktory*)

(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
let wartosc_dokladnosc x p = ((1, x -. abs_float (x *. p /. 100.0), x +. abs_float  (x *. p /. 100.0)) : wartosc);;

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
let wartosc_od_do x y = ((1, x, y) : wartosc);;

(* wartosc_dokladna x = [x;x] *)
let wartosc_dokladna x = ((1, x, x) : wartosc);;

(* zwraca czy y należy do x *)
let in_wartosc (x : wartosc) y =
    match x with
    | (0, _, _) -> false
    | (1, pocz1, kon1) ->
          if (pocz1 <= y && y <= kon1) then true
          else false
    | (2, pocz1, kon1) ->
          if (pocz1 < y && y < kon1) then false
          else true
    | (_, _, _) -> false;;

(* zwraca minimalną wartość należąca do x *)
let min_wartosc (x : wartosc) =
    match x with
    | (0, _, _) -> nan
    | (1, pocz, _) -> pocz
    | (2, _, _) -> neg_infinity
    | (_, _, _) -> nan;;

(* zwraca maksymalną wartość należąca do x *)
let max_wartosc (x : wartosc) =
    match x with
    | (0, _, _) -> nan
    | (1, _, kon) -> kon
    | (2, _, _) -> infinity
    | (_, _, _) -> nan;;

(* zwraca średnią z wartości maksymalnej i minimalnej należącej do x *)
let sr_wartosc x =
    (min_wartosc x +. max_wartosc x) /. 2.;;

(* ta wartość jest zwracana, gdy match nie dopasuje elementu do zadnego innego wzorca *)
let nie_zachodzi = ((0, nan, nan) : wartosc);;

(* zwraca możliwe wartości jakie może przyjąć suma a b *)
let plus (a : wartosc) (b : wartosc) = 
    match a with
    | (0, _, _) -> ((0, nan, nan) : wartosc)
    | (1, pocz1, kon1) -> begin
        match b with
        | (0, _, _) -> ((0, nan, nan) : wartosc)
        | (1, pocz2, kon2) -> ((1, pocz1 +. pocz2, kon1 +. kon2) : wartosc)
        | (2, pocz2, kon2) ->
            if (kon1 +. pocz2 >= pocz1 +. kon2) then ((1, neg_infinity, infinity) : wartosc)
            else ((2, kon1 +. pocz2, pocz1 +. kon2) : wartosc)
         | (_, _, _) -> nie_zachodzi
    end
    | (2, pocz1, kon1) -> begin
        match b with
        | (0, _, _) -> ((0, nan, nan) : wartosc)
        | (1, pocz2, kon2) ->
            if (kon2 +. pocz1 >= pocz2 +. kon1) then ((1, neg_infinity, infinity) : wartosc)
            else ((2, pocz1 +. kon2, pocz2 +. kon1) : wartosc)
        | (2, pocz2, kon2) -> ((1, neg_infinity, infinity) : wartosc)
        | (_, _, _) -> nie_zachodzi
    end
    | (_, _, _) -> nie_zachodzi;;

(* zwraca możliwe wartości jakie może przyjąć różnica a b *)
let minus (a : wartosc) (b : wartosc) =
    match a with
        | (0, _, _) -> ((0, nan, nan) : wartosc)
        | (nr1, pocz1, kon1) -> begin
            match b with
            | (0, _, _) -> ((0, nan, nan) : wartosc)
            | (nr2, pocz2, kon2) -> 
                plus ((nr1, pocz1, kon1) : wartosc) ((nr2, (0. -. kon2), (0. -. pocz2)) : wartosc)
        end;;

(* pomocnicza procedura zwracająca najmniejszą wartość iloczynu przedziałów [pocz1, kon1] [pocz2, kon2] *)
let lewa_granica pocz1 kon1 pocz2 kon2 = min (if (pocz1 = 0. || pocz2 = 0.) then 0. else (pocz1 *. pocz2)) 
                                        (min (if (pocz1 = 0. || kon2 = 0.) then 0. else (pocz1 *. kon2)) 
                                        (min (if (kon1 = 0. || pocz2 = 0.) then 0. else (kon1 *. pocz2)) 
                                        (if (kon1 = 0. || kon2 = 0.) then 0. else (kon1 *. kon2))));;

(* pomocnicza procedura zwracająca największą wartość iloczynu przedziałów [pocz1, kon1] [pocz2, kon2] *)
let prawa_granica pocz1 kon1 pocz2 kon2 = max (if (pocz1 = 0. || pocz2 = 0.) then 0. else (pocz1 *. pocz2)) 
                                         (max (if (pocz1 = 0. || kon2 = 0.) then 0. else (pocz1 *. kon2)) 
                                         (max (if (kon1 = 0. || pocz2 = 0.) then 0. else (kon1 *. pocz2)) 
                                         (if (kon1 = 0. || kon2 = 0.) then 0. else (kon1 *. kon2))));;

(* pomocnicza procedura zwracająca zbiór postaci [a, b] będący wynikiem *)
(* mnożenia możliwych wartości przedziałów [pocz1, kon1] [pocz2, kon2]*)
let mnozenie_pojedynczych_przedzialow pocz1 kon1 pocz2 kon2 = 
    ((1, lewa_granica pocz1 kon1 pocz2 kon2, prawa_granica pocz1 kon1 pocz2 kon2) : wartosc);;

(* procedura zwraca czesc wspolna zbiorow *)
let suma_zbiorow x1 x2 z1 z2 =
    if (x1 = 0. && x2 = 0. && z1 = 0. && z2 = 0.) then ((1, 0., 0.) : wartosc)
    else if (x1 <= z1 && z1 <= x2 && x2 <= z2) then ((1, neg_infinity, infinity) : wartosc)
    else if (z1 <= x1 && x1 <= z2 && z2 <= x2) then ((1, neg_infinity, infinity) : wartosc)
    else if x1 = neg_infinity then ((2, x2, z1) : wartosc)
    else ((2, z2, x1) : wartosc);;

(* zwraca możliwe wartości jakie może przyjąć iloczyn a b *)
let rec razy (a : wartosc) (b : wartosc) =
    match a with
        | (0, _, _) -> ((0, nan, nan) : wartosc)
        | (1, pocz1, kon1) -> begin
            match b with
            | (0, _, _) -> ((0, nan, nan) : wartosc)
            | (1, pocz2, kon2) -> mnozenie_pojedynczych_przedzialow pocz1 kon1 pocz2 kon2
            | (2, pocz2, kon2) -> begin
                let pom1 = razy ((1, pocz1, kon1) : wartosc) ((1, neg_infinity, pocz2) : wartosc) in
                let pom2 = razy ((1, pocz1, kon1) : wartosc) ((1, kon2, infinity) : wartosc) in 
                match pom1, pom2 with (_, x1, x2), (_, z1, z2) ->
                    suma_zbiorow x1 x2 z1 z2
            end
            | (_, _, _) -> nie_zachodzi
        end
        | (2, pocz1, kon1) -> begin
            match b with
            | (0, _, _) -> ((0, nan, nan) : wartosc)
            | (1, pocz2, kon2) -> begin
                let pom1 = razy ((1, pocz2, kon2) : wartosc) ((1, neg_infinity, pocz1) : wartosc) in
                let pom2 = razy ((1, pocz2, kon2) : wartosc) ((1, kon1, infinity) : wartosc) in
                match pom1, pom2 with (_, x1, x2), (_, z1, z2) ->
                    suma_zbiorow x1 x2 z1 z2
            end
            | (2, pocz2, kon2) ->
                if (pocz1 > 0. || pocz2 > 0. || kon1 < 0. || kon2 < 0.) then ((1, neg_infinity, infinity) : wartosc)
                else if (pocz1 = 0. || pocz2 = 0.) && (kon1 = 0. || kon2 = 0.) then ((1, neg_infinity, infinity) : wartosc)
                else 
                let prawo = min (pocz1 *. pocz2) (kon1 *. kon2) in
                let lewo = max (kon1 *. pocz2) (kon2 *. pocz1) in
                ((2, lewo, prawo) : wartosc)
            | (_, _, _) -> nie_zachodzi
        end
        | (_, _, _) -> nie_zachodzi;;

(* zwraca możliwe wartości jakie może przyjąć iloraz a b *)
let podzielic (a : wartosc) (b : wartosc) =
    match b with
    | (0, _, _) -> ((0, nan, nan) : wartosc)
    | (1, 0., 0.) -> ((0, nan, nan) : wartosc)
    | (1, pocz1, 0.) -> razy a ((1, neg_infinity, 1. /. pocz1) : wartosc)
    | (1, 0., kon2) -> razy a ((1, 1. /. kon2, infinity) : wartosc)
    | (1, pocz1, kon1) -> begin
        if (pocz1 = neg_infinity && kon1 = infinity) then razy a ((1, pocz1, kon1) : wartosc)
        else if (pocz1 *. kon1 > 0.) then razy a ((1, 1. /. kon1, 1. /. pocz1) : wartosc)
        else razy a ((2, min (1. /. pocz1) (1. /. kon1), max (1. /. pocz1) (1. /. kon1)) : wartosc)
    end
    | (2, 0., kon1) -> razy a ((1, neg_infinity, 1. /. kon1) : wartosc)
    | (2, pocz1, 0.) -> razy a ((1, 1. /. pocz1, infinity) : wartosc)
    | (2, pocz1, kon1) -> begin
        if (pocz1 *. kon1 < 0.  ) then razy a ((1, 1. /. pocz1, 1. /. kon1) : wartosc)
        else razy a ((2, min (1. /. pocz1) (1. /. kon1), max (1. /. pocz1) (1. /. kon1)) : wartosc)
    end
    | (_, _, _) -> nie_zachodzi;;
    

(*

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = min_wartosc ( podzielic ( wartosc_dokladnosc (9.000000) (7.800000) ) ( wartosc_dokladnosc (-1.800000) (4.400000) ) ) ;;
assert (a =. -5.63807531380753169);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = max_wartosc ( razy ( wartosc_dokladna (-7.000000) ) ( wartosc_dokladnosc (-6.000000) (6.000000) ) ) ;;
assert (a =. 44.52);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = in_wartosc ( razy ( razy ( wartosc_od_do (-2.000000) (0.000000) ) ( wartosc_od_do (0.000000) (0.000000) ) ) ( plus ( wartosc_dokladnosc (-9.000000) (3.000000) ) ( wartosc_od_do (0.000000) (4.000000) ) ) ) (0.000000);;
assert (a = true);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = max_wartosc ( podzielic ( wartosc_od_do (-9.000000) (-7.000000) ) ( wartosc_od_do (-10.000000) (2.000000) ) ) ;;
assert (a = infinity);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = min_wartosc ( razy ( wartosc_od_do (-2.000000) (3.000000) ) ( wartosc_dokladnosc (-7.000000) (6.000000) ) ) ;;
assert (a =. -22.259999999999998);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = sr_wartosc ( plus ( wartosc_od_do (-6.000000) (0.000000) ) ( minus ( podzielic ( wartosc_dokladna (8.000000) ) ( wartosc_dokladna (4.000000) ) ) ( plus ( wartosc_dokladna (-6.000000) ) ( wartosc_dokladnosc (2.000000) (4.000000) ) ) ) ) ;;
assert (a =. 3.);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = max_wartosc ( razy ( wartosc_dokladnosc (8.000000) (0.000000) ) ( podzielic ( wartosc_od_do (-6.000000) (0.000000) ) ( wartosc_od_do (3.000000) (7.000000) ) ) ) ;;
assert (a =. 0.);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = min_wartosc ( podzielic ( wartosc_dokladna (5.400000) ) ( wartosc_dokladna (-3.400000) ) ) ;;
assert (a =. -1.58823529411764719);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = min_wartosc ( razy ( wartosc_dokladna (2.400000) ) ( wartosc_dokladna (-2.400000) ) ) ;;
assert (a =. -5.76);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = min_wartosc ( minus ( wartosc_od_do (-10.000000) (-6.000000) ) ( wartosc_dokladnosc (0.000000) (2.000000) ) ) ;;
assert (a =. -10.);;

let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = max_wartosc ( plus ( wartosc_od_do (-8.000000) (0.000000) ) ( wartosc_dokladnosc (1.000000) (2.000000) ) ) ;;
assert (a =. 1.02);;

*)
