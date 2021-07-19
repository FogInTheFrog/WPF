# Zadanie 1: Arytmetyka
### Arytmetyka przybliżonych wartości
Tam gdzie dokonujemy pomiarów wielkości fizycznych, wyniki są obarczone pewnym błędem, np. 5m ± 10%. Każdą taką przybliżoną wartość traktujemy jak zbiór możliwych wartości. Zaimplementuj pakiet operacji arytmetycznych na takich przybliżonych wartościach zawierający:

### Konstruktory:
wartosc_dokladnosc x p = x ± p% (dla p > 0),
wartosc_od_do x y = (x+y)/2 ± (y-x)/2 (dla x < y),
wartosc_dokladna x = x ± 0
### Selektory:
in_wartosc x y ⇔ wartość x może być równa y,
min_wartosc x = kres dolny możliwych wartości x (lub -∞ jeśli możliwe wartości x nie są ograniczone od dołu),
max_wartosc x = kres górny możliwych wartości x (lub ∞ jeśli możliwe wartości x nie są ograniczone od góry),
sr_wartosc x = średnia (arytmetyczna) wartości min_wartosc x i max_wartosc x (lub nan jeśli min_wartosc x i max_wartosc x nie są skończone),
### Modyfikatory:
plus a b = { x + y : in_wartosc a x ∧ in_wartosc b y },
minus a b = { x - y : in_wartosc a x ∧ in_wartosc b y },
razy a b = { x · y : in_wartosc a x ∧ in_wartosc b y },
podzielic a b = {x / y:  in_wartosc a x ∧ in_wartosc b y }.

Zakładamy przy tym implicite, że wszystkie argumenty typu float są liczbami rzeczywistymi (tzn. są różne od infinity, neg_infinity i nan.
Natomiast w przypadku, gdy wynik nie jest liczbą rzeczywistą, powinien być odpowiednią z wartości: infinity, neg_infinity lub nan.
