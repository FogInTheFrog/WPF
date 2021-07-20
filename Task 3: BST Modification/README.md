# Zadanie 3: Modyfikacja drzew

Zadanie polega na zmodyfikowaniu biblioteki zbiorów pojedynczych elementów zaimplementowanych jako pewien wariant drzew AVL (drzewa BST z wyważaniem). Dzięki wyważaniu wysokość drzewa jest zawsze rzędu logarytmu z liczby wierzchołków i dlatego wszystkie operacje wykonywane są w czasie logarytmicznym (nawet operacja split, ale to jest trochę mniej oczywiste: wynika z tego, że koszt join jest w istocie proporcjonalny do różnicy wysokości drzew, które łączymy. A ponieważ na split składa się ciąg operacji join na coraz wyższych drzewach, ich koszty sumują się do wysokości drzewa razy pewna stała).

Wynikiem modyfikacji ma być biblioteka zbiorów liczb całkowitych oparta o przedziały. Czyli elementami występującymi w drzewie muszą być przedziały, a nie pojedyncze liczby. Przedziały muszą być rozłączne i w dodatku, aby uniknąć niejednoznaczności reprezentacji, przedziały w drzewie nie mogą być "sąsiednie", czyli np. dwa przedziały [1..3] i [4..6] powinny być zastąpione przez jeden przedział [1..6]. W naszej bibliotece dopuszczamy przedziały jednoelementowe, np. [3..3].

Wszystkie operacje (poza fold, iter, elements oraz is_empty) mają wykonywać się w czasie O(log n), gdzie n jest liczbą wierzchołków w drzewie.

Do zadania dołączona jest oryginalna specyfikacja i implementacja zbiorów (obie dostępne na licencji GNU Lesser General Public License) oraz specyfikacja zbiorów przedziałów, której implementację należy przesłać poprzez system moodle jako plik o nazwie iSet.ml.
