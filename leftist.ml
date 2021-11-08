(** Autor: Jakub Jakubowski *)
(** Code reviewer: Konrad Krawiec*)

(** Typ złączalnej kolejki priorytetowej: *)
(** Null -> gdy kolejka jest pusta *)
(** Gdy kolejka jest niepusta - Node z lewym poddrzewem, wartością z najwyższym priorytetem, prawym poddrzewem, prawa wysokość poddrzewa zaczepionego w tym węźlea*)
type 'a queue = 
        Node of ('a queue * 'a * 'a queue * int) | 
        Null;;

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty;;

(** Funkcja, która z danej kolejki priorytetowej zwraca prawą wysokość poddrzewa, lub -1 gdy jest ono puste*)
let high q = 
    match q with
        | Null -> (-1)
        | Node (_,_,_,h) -> h;;

(** Pusta kolejka priorytetowa *)
let empty=
    Null;;

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    if (q=Null) then true
    else false;;

let rec join a b =
    match a,b with 
        | Null, _ -> b  (** Jeśli którakolwiek z kolejek, które łączymy jest pusta, to wynikiem join jest ta druga *)
        | _, Null -> a 
        | Node (_, p1, _, _), Node (_, p2, _, _) when (p1 > p2) -> join b a     (** Jeśli wartość a jest większa od wartości b, to zamieniamy a i b *)
        | Node (left, value, right, h) , _ ->
            let right_tree = join right b in (** Kolejka otrzymana z połączenia lewego poddrzewa a oraz całego b*)
                if (high right_tree > high left ) then
                    Node (right_tree, value, left , (high left) +1)               (** Jeśli wysokość lewego poddrzewa jest mniejsza od wysokości prawego poddrzewa zamieniamy*)
                else                                                              (**  je miejscami (w przeciwnym razie lewe pozostaje lewe, a prawe prawe) *) 
                    Node (left, value, right_tree, (high right_tree) +1);;        (** Dla pustego poddrzewa funkcja high zwróci -1, więc dla kolejki wynikowej wysokość poddrzewa pustego będzie równa 0 *)

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *)
let add x q = 
    join q ( Node (Null , x , Null, 0) );;

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q =
    match q with
        | Null -> raise Empty
        | Node (left, value, right, _) ->
            (value, join left right);; (** Dla niepustej kolejki zwraca wartość w korzeniu (z priorytetem), 
                                        oraz połączone ze sobą poddrzewa kolejki wejściowej - stanowiące
                                        drzewo q bez jego korzenia *)