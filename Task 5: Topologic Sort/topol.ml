(* Autor: Jakub Zacharczuk *)
 
open PMap
 
exception Cykliczne
 
(* Zwraca zaktualizowaną mapę stopni wchodzących wierzchołka *)
let oblicz_stopien x aktualne_stopnie lista_krawedzi =
    let rec zaktualizuj_stopnie acc sasiedzi =
        match sasiedzi with
        | [] -> acc
        | (h::t) -> let wartosc = if mem h acc then ((find h acc) + 1) else 1 in
            zaktualizuj_stopnie (add h wartosc acc) t in
    let poczatkowy = if mem x aktualne_stopnie then (find x aktualne_stopnie) else 0 in
    (zaktualizuj_stopnie (add x poczatkowy aktualne_stopnie) lista_krawedzi)
 
(** Dla danego wierzchołka x i mapy m, jeśli x pojawił się wcześniej jako pierwszy element pary na 
wejściowej liście, wtedy funkcja zwraca mapę m z zaktualizowaną listą wierzchołków do których
istnieje bezpośrednia krawędź z x. W przeciwnym przypadku dodaje do mapy x i listę wierzchołków *)
let stworz_graf x sasiedzi m =
    let polacz_listy = if mem x m then find x m else [] in
    add x (sasiedzi@polacz_listy) m
 
(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lista =
    let graf = List.fold_left (fun m (a, sasiedzi) -> 
        stworz_graf a sasiedzi m) empty lista in
    let stopien_wierzcholka =
        ref (foldi (fun x lista_pom akt_stopnie -> 
                (oblicz_stopien x akt_stopnie lista_pom)) graf empty) in
    let wynik = ref [] in
    let kolejka = Queue.create () in
    let usun_wierzcholek x = begin
        (wynik := (x :: !wynik));
        stopien_wierzcholka := remove x !stopien_wierzcholka;
        if mem x graf then let sasiedzi = find x graf in
        List.iter (fun x -> 
            let st_x = (find x !stopien_wierzcholka) - 1 in
            stopien_wierzcholka := add x st_x !stopien_wierzcholka;
            if st_x = 0 then Queue.add x kolejka) sasiedzi;
    end in
    (iter (fun x stopien -> if stopien = 0 then Queue.add x kolejka) !stopien_wierzcholka);
    while Queue.is_empty kolejka = false do
    let x = Queue.take kolejka in
    let stopien = find x !stopien_wierzcholka in
    if stopien = 0 then usun_wierzcholek x
    done;
    if is_empty !stopien_wierzcholka then List.rev !wynik
    else raise Cykliczne
 
(*
exception WA;;
 
let debug = false;;
 
let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false
  
let test_cyclic g =
  try let _ = topol g in false
  with Cykliczne -> true;;
 
let g = [
  ("1", ["2"; "3"]);
  ("3", ["2"]);
  ("4", ["3"; "2"])
];;
 
assert(test g (topol g));;
 
let g = [
  ("first", ["second"; "fourth"; "eighth"]);
  ("second", ["fourth"; "eighth"]);
  ("third", ["fourth"; "fifth"; "sixth"]);
  ("fourth", ["eighth"]);
  ("fifth", ["sixth"; "seventh"]);
  ("sixth", ["eighth"; "first"]);
  ("seventh", ["eighth"]);
];;
 
assert(test g (topol g));;
 
let g = [
  (1, [2; 3]);
  (2, [4]);
  (3, [4]);
  (4, [5; 6]);
  (5, [7]);
  (6, [7]);
];;
 
assert(test g (topol g));;
 
let g = [
  (1, [7; 2]);
  (3, [4; 2; 1; 7; 5]);
  (4, [2; 7; 1]);
  (5, [7; 4; 1; 2]);
  (6, [1; 3; 2; 5; 4; 7]);
  (7, [2])
];;
 
assert(test g (topol g));;
 
let g = [
  (1, [2; 4; 8]);
  (2, [16; 32]);
  (4, [64; 128]);
  (8, [256; 512]);
  (16, [1024]);
  (32, [2048]);
  (64, [4096]);
  (128, [8192]);
  (256, [16384]);
  (512, [32768]);
];;
 
assert(test g (topol g));;
let g = [
  ("pole", ["pole"; "lyse"; "pole"])
];;
 
assert(test_cyclic g);;
 
let g = [
  ("tu", ["tudu"]);
  ("tudu", ["tudu"; "tudu"; "tudu"])
];;
 
assert(test_cyclic g);;
 
let g = [
  ("Pa", ["pa"]);
  ("pa", ["Pa"])
];;
assert(test_cyclic g);;
 
let g = [
  ("isfjsjefi", ["pa"]);
  ("pa", ["isfjsjefi"])
];;
assert(test_cyclic g);;
 
let g = [
  ("p", ["q"]);
  ("q", ["p"])
];;
assert(test_cyclic g);;
 
let g = [
  (0, [1]);
  (1, [1])
];;
assert(test_cyclic g);;
 
*)
