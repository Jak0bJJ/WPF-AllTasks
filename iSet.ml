(*
    ISet - Interval sets
    Project basen on work of Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
    provided by University of Warsaw
    Copyright (C) 1996-2019
    
    Author: Jakub Jakubowski
    Reviewer: Weronika Tkaczyk 
*)
(* Struktura węzła w drzewie zawierająca pusty węzeł lub odpowiednio
    lewe poddrzewo - (początek , koniec) przedziału na węźle - prawego syna - odleglosc do najdalszego liscia - suma ilości liczb całkowitych w węźle oraz obu poddrzewach
 Struktura spełnia załozenia:
    i) Drzew AVL - Różnica w wysokosci prawej i lewego podrzewa wynosi maksymalnie 2 
    ii) Drzew BST - Wartosci w prawym podrzewie są większe niż na węźle, a wartosi na lewym są mniejsze. 
  Będę stosował oznaczeń: h - wysokość drzewa, n - ilość węzłów w drzewie*)
type t =
  | Empty
  | Node of  t * (int * int) * t * int * int ;;

(* Funkcja zwracająca wysokosc drzewa *)
let height = function
  | Node (_, _, _, h,_) -> h
  | Empty -> 0;;

(* Funkcja zwracająca sumę ilości liczb całkowitych w węźle oraz obu poddrzewach *)
let bel =  function
  | Node (_, _, _, _, b) -> b
  | Empty -> 0;;

(* Funkcja zwracająca sumę a+b, z uwzględnieniem wychodzenia sumy poza zakres, zwracając wtedy odpowiednio max_int lub min_int*)
let is_max a b =
  if (a>0 && b>0) && (a > max_int - b || b > max_int - a)  then max_int
  else if (a<0 && b<0) && (a < min_int - b || b < min_int - a) then min_int
  else a+b

(* Funkcja zwracjaąca ilość liczb całkowitych na danym przedziale *)
let przedzial (a, b) =
  if a = min_int then
    if b = min_int then 1
    else is_max (is_max b max_int) 2
  else is_max (is_max b (-a) ) 1 ;;

(* Tworzenie węzła zawierającego przedział k, lewego syna l i prawego syna r *)
let make l k r = Node (l, k, r, max (height l) (height r) + 1,  is_max (is_max (bel l) (bel r)) (przedzial k) );;

(* Funkcja balansująca drzewo tak, by h - jego wysokość była rzędu log n *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _,_) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _,_) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _,_) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1, is_max (is_max (bel l) (bel r)) (przedzial k) ) ;;

(* Funkcja zwracjająca najmniejszy (pod względem wartości początkowej i końcowej) przedział na drzewie*)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego 
      węzła, a kończy się maksymalnie na liściu. Taka droga od korzenia do liścia jest rzędu h - czyli log n *)
let rec min_elt = function
  | Node (Empty, k, _, _,_) -> k
  | Node (l, _, _, _,_) -> min_elt l
  | Empty -> (min_int,min_int) ;;

(* Funkcja usuwająca z drzewa najmniejszy (pod względem wartości początkowej i końcowej) przedział znajdujący się na tym drzewie *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego 
      węzła, a kończy się maksymalnie na liściu. Taka droga od korzenia do liścia jest rzędu h - czyli log n *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _,_) -> r
  | Node (l, k, r, _,_) -> bal (remove_min_elt l) k r 
  | Empty -> invalid_arg "iSet.remove_min_elt" ;;

(* Funkcja łącząca dwa drzewa w jedno *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2) ;;

(* Pusty węzeł (niezawierający synów i przedziału) *)
let empty = Empty;;

(* Funkcja sprawdzająca czy węzeł jest pusty *)
let is_empty set =
  match set with
  Empty -> true
  | Node (_, _, _, _,_) -> false ;;

(* Funkcja zwracjąca true, gdy przedział jest niepoprawny - jego pierwszy element oznaczający 
początek jest większy od drugiego oznaczającego koniec lub false w przeciwnym przypadku *)
let not_ok x =
  if (fst x > snd x) then true
  else false ;;

(* Funkcja porównująca ze sobą dwa przedziały na potrzeby funkcji add_one *)
let cmp_add (a, b) (c,d) =
    if a < c && d < b then 0
    else if c > b then 1
    else -1;;

(* Funkcja dodająca przedział do drzewa. Dodawany przedział nie może mieć wspólnego elementu 
z żadnym znajdującym się aktualnie na drzewie przedziałem oraz nie może się "łączyć" z nimi *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego 
  węzła, a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let rec add_one x = function
  | Node (l, (low,high), r, h, q) ->
    if (not_ok x) then Node (l, (low,high), r, h, q)
    else 
        let c = cmp_add (low,high) x  in
         if c < 0 then
          let nl = add_one x l in
          bal nl (low,high) r
        else
          let nr = add_one x r in
          bal l (low,high) nr
  | Empty ->
    if (not_ok x) then Empty
    else Node (Empty, x , Empty, 1, przedzial x);;

(* Funkcja łącząca dwa drzewa (l,r) oraz przedział v w jedno drzewo *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego 
  węzła, a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let rec join  l v r =
  match (l, r) with
    (Empty, _) -> add_one  v r
  | (_, Empty) -> add_one  v l
  | (Node(ll, lv, lr, lh, lq), Node(rl, rv, rr, rh, rq)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r ;;

(* Funkcja porównująca ze sobą przedział oraz liczbę x
zwracająca informacje o połozeniu liczby x względem przedziału *)
let cmp (low, high) x= 
    if low <= x && x <= high then 0
    else if x > high then 1
    else -1 ;;  

(* Funkcja przyjmuje liczbę x oraz drzewo, 
    zwraca trójke (l, stan, r) taką że:
  [l]     - drzewo przedziałow mniejszych niż x,
  [r]     - drzewo przedziałow większych niż x,
  [stan]  - przyjmuje wartość [true], jeśli wartosć x  
            występuje w drzewie s, lub [false] jeśli nie. *)
(* Funkcja łącząca dwa drzewa (l,r) oraz przedział v w jedno drzewo *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego węzła, 
  a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let split x set =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, q) ->
        let c = cmp v x in
        if c = 0 then (add_one (fst v, x-1) l , true, add_one (x+1, snd v) r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join  rl v r)
        else
          let (lr, pres, rr) = loop x r in (join  l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  setl, pres, setr ;;

(* Funkcja usuwa liczby z przedziału v z drzewa *)
let remove v set =
  let (l,_,_) = 
  if (fst v) = min_int then (Empty,false,Empty) 
  else split (fst v) set 
  and (_,_,r) = 
  if (snd v) = max_int then (Empty,false,Empty)
  else split (snd v) set in
    merge l r ;;

(* Funkcja zwracjająca najmniejszy (pod względem wartości początkowej i końcowej) przedział na drzewie, 
  w którym zawiera się x, badź x jest o 1 większe od wartości końcowej tego przedziału *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego węzła, 
  a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let rec low x set =
  match set with
    Empty -> x |
    Node (l, v, r, _, _) -> 
    if (x=min_int) then min_int
    else if snd v >= x-1 && fst v <= x then fst v
    else if snd v  < x - 1 then low x r
    else low x l ;;

(* Funkcja zwracjająca największy (pod względem wartości początkowej i końcowej) przedział na drzewie, 
  w którym zawiera się x, badź x jest o 1 mniejsze od wartości początkowej tego przedziału *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego węzła, 
  a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let rec high x set =
  match set with
    Empty -> x |
    Node (l, v, r, _, _) -> 
    if (x=max_int) then max_int
    else if fst v <= x+1 && snd v >= x then snd v
    else if fst v  > x + 1 then high x l
    else high x r ;;

(* Dodaje przedział (x, y) do drzewa s *)
let add v set =
  let new_v = (low (fst v) set, high (snd v) set) in 
  add_one new_v (remove new_v set) ;;

(* Funkcja wykonująca funkcje f na kazdym przedziale w drzewie w porzadku rosnacych*)
(* Funkcja wywołuje się na danym węźle, a następnie w dla obu jego synów. 
  Wywołuje się ona więc dla każdego węzła w drzewie, a więc jej złożoność jest rzędu O(n) *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r;  in
  loop set

(* Funkcja oblicza wartosc [(f xN ... (f x2 (f x1 a))...)] gdzie x1, ... xN to kolejne przedziały na drzewie s w porządku rosnących *)
(* Funkcja wywołuje się na danym węźle, a następnie w dla obu jego synów. 
  Wywołuje się ona więc dla każdego węzła w drzewie, a więc jej złożoność jest rzędu O(n) *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(* Zwraca lista wszystkich przedzialow na drzewie w porzadku rosnącym *)
(* Funkcja wywołuje się na danym węźle, a następnie w dla obu jego synów. 
  Wywołuje się ona więc dla każdego węzła w drzewie, a więc jej złożoność jest rzędu O(n) *)
let elements set = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(* Sprawdza czy wartosc x znajduje sie w drzewie *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego węzła, 
  a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let mem x set  =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = cmp k x in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop set

(* Funkcja zwraca ilosc elementów niewiększych niż x w drzewie
  lub max_int, jeśli iczba takich elementów przekracza max_int *)
(* Funkcja ta posiada złożność O(h)=O(log n), gdyż w każdym kroku rekurencyjnym wywołuje się ona dla jednego syna aktualnego węzła, 
  a kończy się maksymalnie na synu liścia (pustym drzewie). Taka droga od korzenia do syna liścia jest rzędu h - czyli log n *)
let rec below x = function
    | Empty -> 0
    | Node (l, v, r, _, q) ->
        let c = cmp v x in
        if c = 0 then  is_max (bel l) ( przedzial (fst v,x) )
        else if c < 0 then
          below x l 
        else
         is_max ( is_max (bel l) (przedzial v) ) (below x r) ;;