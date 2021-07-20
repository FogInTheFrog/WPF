# Zadanie 2: Drzewa Lewicowe
Drzewa lewicowe to ciekawa implementacja złączalnych kolejek priorytetowych.

Kolejki priorytetowe to struktury przechowujące dane obdarzone priorytetami, umożliwiające łatwy dostęp do elementu o najwyższym priorytecie. (Tradycyjnie, im mniejsza liczba reprezentująca priorytet, tym wyższy priorytet. ;-) Struktury te dostarczają następujących operacji:

* utwórz pustą strukturę, <br />
* wstaw nowy element, <br />
* usuń element o najwyższym priorytecie. 

Oczywiście po usunięciu elementu o najwyższym priorytecie, drugi w kolejności staje się tym najwyższym itd. Kolejki złączalne umożliwiają dodatkowo łączenie dwóch kolejek w jedną.

Kolejki priorytetowe implementuje się zwykle za pomocą tzw. kopców, czyli struktur drzewiastych, które spełniają tzw. warunek kopca, mówiący, że priorytet elementu zawartego w korzeniu każdego poddrzewa jest mniejszy lub równy niż każdego innego elementu w tym poddrzewie.

Drzewa lewicowe to kopce binarne (czyli każdy węzeł może mieć 0, 1 lub dwóch potomków) spełniające, oprócz warunku kopca, tzw. warunek lewicowości. Warunek lewicowości mówi, że dla każdego węzła skrajnie prawa ścieżka zaczynająca się w danym węźle jest najkrótszą ścieżką od tego węzła do liścia.

Dzięki temu w każdym drzewie lewicowym, tzw. prawa wysokość, czyli długość skrajnej prawej ścieżki od korzenia do liścia, jest co najwyżej logarytmicznej wielkości, w porównaniu z liczbą elementów drzewa. Dodatkowo, aby umożliwić efektywne wykonywanie operacji na drzewie, w każdym węźle przechowywana jest prawa wysokość poddrzewa zaczepionego w tym węźle.

Najważniejszą operacją na drzewach lewicowych jest ich łączenie. Pozostałe operacje wykonuje się bardzo prosto:

* wstawianie elementu do istniejącego drzewa polega na utworzeniu jednoelementowego drzewa i połączeniu go z danym drzewem,
* usuwanie najmniejszego to usunięcie korzenia drzewa i połączenie poddrzew.

Łączenie drzew lewicowych też nie jest trudne. Aby połączyć dwa niepuste drzewa lewicowe, ustawiamy jako pierwsze (d1) to, które ma mniejszy element w korzeniu, a jako drugie (d2) to, które ma większy. W korzeniu wynikowego drzewa na pewno będzie więc korzeń d1. Teraz rekurencyjnie łączymy prawe poddrzewo d1 oraz całe drzewo d2, w wyniku dostając drzewo d3. Jako wynik całej operacji łączenia d1 i d2 zwracamy drzewo d4, w którego korzeniu jest korzeń d1, natomiast poddrzewami są lewe poddrzewo d1 oraz drzewo d3, przy czym prawym poddrzewem d4 zostaje to z nich, które ma mniejszą prawą wysokość. Dzięki temu d4 pozostaje drzewem lewicowym. Oczywiście przy konstrukcji drzewa d4 należy pamiętać o odpowiednim ustawieniu prawej wysokości.

Rysunkową wersję łączenia drzew lewicowych można zobaczyć np. tu:
https://courses.cs.washington.edu/courses/cse326/00wi/handouts/lecture7/sld001.htm

W naszym zadaniu dla uproszczenia zakładamy, że dane składają się z samych priorytetów.

Zadanie
Używając drzew lewicowych zaimplementuj złączalną kolejkę priorytetową z następującymi operacjami:

type 'a queue (** Typ złączalnej kolejki priorytetowej *) <br /> 
val empty : 'a queue (** Pusta kolejka priorytetowa *) <br />
val add : 'a -> 'a queue -> 'a queue (** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *) 
exception Empty (** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *) <br /> 
val delete_min : 'a queue -> 'a * 'a queue (** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] 
gdzie [e] jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
Jeśli [q] jest puste podnosi wyjątek [Empty]. *) <br />
val join : 'a queue -> 'a queue -> 'a queue (** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *) <br />
val is_empty : 'a queue -> bool (** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *) 
