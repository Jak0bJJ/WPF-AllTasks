(** Autor: Jakub Jakubowski *)
(** Code review: Maciej Szerenos *)

(** Funkcja, która bierze array wejściowy i zwraca array, który
    na i-tym indeksie zawiera informacje o pojemności i-tej szklanki 
    Złożoność: O(n) dla n - rozmiar array'a *)
let get_array t =
    let n = Array.length t in
    let w = Array.make n 0 in
    for i = 0 to n-1 do
        w.(i)<- fst t.(i);
    done;
    w;;

(** Funkcja, która bierze array wejściowy i zwraca array, który na i-tym 
    indeksie zawiera informacje o oczekiwanej ilości wody w i-tej szklanki 
    Złożoność: O(n) dla n - rozmiar array'a *)
let get_wyn t =
    let n = Array.length t in
    let w = Array.make n 0 in
    for i = 0 to n-1 do
        w.(i)<- snd t.(i);
    done;
    w;;

(** Funkcja sprawdzający warunek konieczny - w wynikowym rozłożeniu
    wody w szklankach co najmniej jedna musi być pusta lub pełna 
    Złożoność: O(n) dla n - rozmiar array'a *)
let is_pos w t =
    let n = Array.length t in
    let x = ref false in
    for i = 0 to n-1 do
        if ( w.(i) = 0 || w.(i) = t.(i) ) then x := true;
    done;
    !x;;

(** Alogrytm Euklidesa wyznaczjący NWD dwóch liczb 
    Złożność: O(log a+b) dla a,b - liczby na wejściu *)
let rec nwd a b =
    if a = 0 then b
    else nwd (b mod a) a

(** Funkcja sprawdzający warunek konieczny - podzielność
    Złożoność: O(n log (m)) dla n - rozmiar array'a, 
    m - największy element w wynikowym array'u *)
let check_nwd w t =
    let n = Array.length t in
    let div = Array.fold_left nwd 0 t in
    let x = ref true in 
    if div <> 0 then 
        for i = 0 to n-1 do
            if w.(i) mod div <> 0 then x:= false;
        done;
    !x;;

let przelewanka tab = 
    match tab with
    | [||] -> 0 (** Przypadek, gdy na wejściu otrzymujemy pusty array *)
    | _ ->
    let n = Array.length tab in
    let t = get_array tab in
    let akt = ref (Array.make n 0) in
    let w = get_wyn tab in
    let ans = ref (-1) in
    let que = Queue.create () in (** Kolejka zawierająca kolejne rozpatrywane stany *)
    let map = Hashtbl.create 1 in (** Mapy sprawdzająca czy dany stan był już rozpatrywany *)
    if is_pos w t = false then (-1) (** Sprawdzenie warunku koniecznego *)
    else if check_nwd w t = false then (-1) (** Sprawdzenie warunku koniecznego *)
    else if (!akt = w) then 0 (** Sprawdzenie czy stan początkowy jest zarazem stanem oczekiwanym *)
    else
    begin
        (** Dodanie stanu początkowego do kolejki i mapy oraz oflagowanie stanu wynikowego na mapie *)
        Hashtbl.add map w 2;
        Hashtbl.add map !akt 1;
        Queue.add (!akt,0) que;
        while ((!ans = (-1)) && ((Queue.is_empty que) = false )) do
            let (top, time) = Queue.take que in
            if Hashtbl.mem map top && Hashtbl.find map top = 2 then ans := time;
            (** Rozpatrzenie zmiany na każdym indeksie *)
            for i=0 to n-1 do
                akt := Array.copy top;
                (** Wylanie wody ze szklanki *)
                !akt.(i) <- 0;
                (** Sprawdzanie, czy otrzymaliśmy wynik oraz dodanie stanu do mapy oraz kolejki
                    jeśli pojawił się on po raz pierwszy *)
                if Hashtbl.mem map !akt && Hashtbl.find map !akt = 2 then ans := time+1;
                if Hashtbl.mem map !akt = false then 
                begin
                    Hashtbl.add map !akt 1;
                    Queue.add (!akt,time+1) que;
                end;
                akt := Array.copy top;
                (** Nalanie wody do pełna *)
                !akt.(i) <- t.(i);
                if Hashtbl.mem map !akt && Hashtbl.find map !akt = 2 then ans := time+1;
                if Hashtbl.mem map !akt = false then 
                begin
                    Hashtbl.add map !akt 1;
                    Queue.add (!akt,time+1) que;
                end;
                (** Przelenie wody z i-tej szklanki do j-tej szklanki *)
                for j=0 to n-1 do
                    if i <> j && top.(j) <> t.(j) then
                    begin
                        akt := Array.copy top;
                        let dif = t.(j) - !akt.(j) in 
                        !akt.(j) <- min (!akt.(j) + !akt.(i)) t.(j);
                        if !akt.(j) = t.(j) then (** Przypadek, gdy j-ta szklanka została wypełniona *)
                            !akt.(i) <- !akt.(i) - dif
                        else (** Przypadek, gdy i-ta szklanka została opróżniona *)
                            !akt.(i) <- 0;
                        if Hashtbl.mem map !akt && Hashtbl.find map !akt = 2 then ans := time+1;
                        if Hashtbl.mem map !akt = false then 
                        begin
                            Hashtbl.add map !akt 1;
                            Queue.add (!akt,time+1) que;
                        end;
                    end;
                done;
            done;
        done;
        !ans
    end;;
    