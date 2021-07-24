(* Autor: Jakub Zacharczuk *)

(* Typ wynikowy *)
type wyn = Nic | NieDaSie | Int of int

(* Zwraca największy wspólny dzielnik a i b *)
let rec nwd a b =
    if b = 0 then a else nwd b (a mod b)

(* Procedura pomocnicza procedury czy_mozliwe *)
let czy_mozliwe_pom t_i nwd_t =
    nwd_t := nwd !nwd_t t_i

(* Sprawdza czy jest możliwe osiągnięcie oczekiwanego poziomu wody w szklankach
Jeśli tak to dodaje początkowy stan do kolejki wpp ustawia wynik na NieDaSie *)
let czy_mozliwe x y dlugosc wynik puste kolejka =
    let nwd_x = ref 0 in
    let nwd_y = ref 0 in
    let pusta_pelna = ref false in
    for i = 0 to dlugosc - 1 do
        czy_mozliwe_pom x.(i) nwd_x;
        czy_mozliwe_pom y.(i) nwd_y;
        if (y.(i) = 0) <> (y.(i) = x.(i)) then pusta_pelna := true
    done;
    if !nwd_y = 0 || (nwd !nwd_x !nwd_y = !nwd_x && !pusta_pelna = true) then
        Queue.add (puste, 0) kolejka
    else wynik := NieDaSie

(* Sprawdza czy poziom wody w szklankach z tablicy stan jest oczekiwanym
stanem końcowym. Jeśli tak to aktualizuje wynik *)
let sprawdz stan y nr wynik =
    if stan = y then begin
        wynik := Int nr;
    end

(* Sprawdza czy stan z tablicy dodaj był już wcześniej rozpatrywany.
Jeśli nie to zostaje dodany do kolejki i ustawiony jako rozpatrzony *)
let dodaj_stan dodaj stany nr kolejka =
    if Hashtbl.mem stany dodaj = false then begin
        Hashtbl.add stany dodaj (nr + 1);
        Queue.add (dodaj, nr + 1) kolejka
    end

(* Symuluje przelanie wody ze szklanki i-tej do j-tej gdy i <> j. W przeciwnym
  przypadku szklanka może zostać wypełniona wodą albo może stać się pusta *)
let przelej i j akt x nr stany kolejka =
    let dodaj = Array.copy akt in
    if i = j && x.(i) = akt.(i) && x.(i) <> 0 then
            dodaj.(i) <- 0
    else if i = j && x.(i) > akt.(i) then
            dodaj.(i) <- x.(i)
    else begin
        let woda = min akt.(i) (x.(j) - akt.(j)) in
        dodaj.(i) <- akt.(i) - woda;
        dodaj.(j) <- akt.(j) + woda;
    end;
    dodaj_stan dodaj stany nr kolejka;
    dodaj

(* Procedura zwraca minimalną ilość ruchów potrzebnych do napełnienia
szklanek do poziomów opisanych w tablicy tablica jeśli jest to możliwe.
W przeciwnym przypadku zwraca -1. Wszystkie otrzymane w danej chwili
stany są trzymane w tablicy stany *)
let przelewanka tablica =
    let dlugosc = Array.length tablica in
    let x = Array.init dlugosc (fun x -> fst tablica.(x)) in
    let y = Array.init dlugosc (fun y -> snd tablica.(y)) in
    let puste = Array.make dlugosc 0 in
    let wynik = ref Nic in
    let stany = Hashtbl.create 997 in
    let kolejka = Queue.create () in
    czy_mozliwe x y dlugosc wynik puste kolejka;
    sprawdz puste y 0 wynik;
    
    (* Kolejka zawiera stany które nie są stanami końcowymi. Dopóki 
    stan końcowy nie będzie osiągalny pętla symuluje dla każdej pary
    szklanek przelanie wody z jednej do drugiej i aktualizuje następne
    w kolejności stany do rozpatrzenia *)
    while !wynik = Nic do
        let (akt, nr) = 
            try Queue.take kolejka 
            with Queue.Empty -> failwith "kolejka pusta" in
        let i = ref 0 in
        while !i < dlugosc && !wynik = Nic do
            let j = ref 0 in
            while !j < dlugosc && !wynik = Nic do
                let dodany = przelej !i !j akt x nr stany kolejka in
                sprawdz dodany y (nr + 1) wynik;
                j := !j + 1;
            done;
            i := !i + 1;
        done;
    done;
    match !wynik with
    | Int liczba -> liczba
    | NieDaSie -> -1
    | Nic -> failwith "cos poszlo nie tak"
    
(*
assert (przelewanka [|(5, 2); (1, 1)|] = 5);;
assert (przelewanka [|(5, 2); (1, 0)|] = 4);;
assert (przelewanka [|(1, 0); (1, 0)|] = 0);;
assert (przelewanka [|(9, 6); (12, 9); (12, 3); (999, 411)|] = -1);;
assert (przelewanka [|(10000, 5000); (1, 0)|] = 10000);;
assert (przelewanka [|(50000, 450); (3, 1); (3, 0)|] = 33635);;
assert (przelewanka [|(3, 3); (4, 0); (1, 1); (6, 6)|] = 3);;
assert (przelewanka [|(14, 3); (5, 1)|] = -1);;
assert (przelewanka [|(10, 5); (4, 3); (3, 2); (2, 0)|] = 5);;
assert (przelewanka [|(5, 2); (0, 0)|] = -1);; *)
