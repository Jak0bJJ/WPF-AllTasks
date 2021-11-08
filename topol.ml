(** Autor: Jakub Jakubowski *)
(** Code review: Ewa Majdaniuk*)

open PMap;;

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne;;

(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il 
    Złożoność: O((n+m) log (n+m)), gdzie n oznacza ilość wierzchołków, a
    m ilość krawędzi. *)
let topol lst = 
    (** Dla pustej listy wejściowej algorytm zwraca pustą listę jako wynik. *)
    match lst with
    | [] -> []
    | _ ->
    let pom_deg m (x,tab) = (** Funkcja pomocnicza do fold_left *)
        let map = ref m in
        let t = ref tab in
            while !t <> [] do
                map := PMap.add (List.hd !t) 0 !map;
                t := List.tl !t;
            done;
            PMap.add x 0 !map in
    (** Mapa, która dla danego wierzchołka zawiera informacje o ilości
        krawędzi wchodzących, jaką dany wierzchołek posiada.
        Początkowo wypełniona jest zerami, a prawidłowe wartości zostają
        jej przypisane w trakcie generowania mapy g (linijka 38). 
        Złożoność wygenerowania: O((n+m) log (n+m)) *)
    let deg = ref (List.fold_left pom_deg PMap.empty lst) in
    (** Lista zawierająca wszystkie wierzchołki znajdujące się w grafie.
        Złożoność wygenerowania: O(n) *)
    let points = PMap.foldi (fun b v a -> b::a) !deg [] in
    let pom_g m (x,tab) = (** Funkcja pomocnicza do fold_left *)
        let t = ref tab in
        let map = ref m in
        while !t <> []
            do
                deg := PMap.add (List.hd !t) ((PMap.find (List.hd !t) !deg ) + 1) !deg;
                if PMap.mem x !map then  map := PMap.add x ((List.hd !t)::(PMap.find x !map)) !map
                else map := PMap.add x [List.hd !t] !map;
                t := List.tl !t;
            done;
        !map; in
    (** Mapa, która służy do reprezentowania grafu - do danego wierzchołka
        przypisana jest lista wszystkich wierzchołków, do których wychodzi
        z niego krawędź
        Złożoność wygenerowania: O((n+m) log (n+m)) *)
    let g = List.fold_left pom_g PMap.empty lst in
    let pom_q q x = (** Funkcja pomocnicza do fold_left *)
        if PMap.find x !deg = 0 then Queue.add x q; q in
    (** Kolejka, na która wrzucane bedą kolejne rozpatrywane wierzchołki.
        Bazowo zawiera wszystkie wierzchołki, które nie posiadają żadnej
        krawędzi wchodzącej.
        Złożoność wygenerowania: O(n) *)
    let q = (List.fold_left pom_q (Queue.create()) points) in
    (** Lista wynikowa *)
    let w = ref [] in
    (** Lista pomocnicza *)
    let t = ref [] in
    (** Jeżeli graf nie posiada żadnego wierzchołka, który nie posiada
        krawędzi wchodzących to na pewno zawiera on cykl *)
    if Queue.is_empty q then raise Cykliczne;
    (** Główna pętla, która pobiera wierzchołek z kolejki, a następnie
        "usuwa" wszystkie krawędzi z niego wychodzące z grafu poprzez obniżenie
        wartości deg wierzchołków do których wychodzi z niego krawędź o 1, a 
        następnie dorzuca rozpatrywany wierzchołek na początek listy wynikowej.
        W przypadku wyzerowania wartości deg jakiegoś wierzchołka jest on
        dorzucany do kolejki.
        Złożność: O((n+m) log (n+m)) *)
    while not (Queue.is_empty q) do
        let akt = Queue.take q in
        if PMap.mem akt g then
            t := PMap.find akt g
        else 
            t := [];
        while !t <> []
            do
                deg := PMap.add (List.hd !t) ((PMap.find (List.hd !t) !deg ) - 1) !deg;
                if PMap.find (List.hd !t) !deg = 0 then Queue.add (List.hd !t) q;
                t := List.tl !t;
            done;
        w := akt::(!w);
    done;
    (** Jeśli wynikowa lista nie zawiera wszystkich wierzchołków, to znaczy,
        że algorytm napotkał cykl, a więc podnoszony jest exception. *)
    if List.length !w <> List.length points then raise Cykliczne
    (** Wynikiem działania algorytmu jest odwrócona lista w. *)
    else List.rev !w;; 