(* Autor: Jakub Jakubowski *)
(* Code Review: Jakub Korzeniewski *)
type wartosc = float * float * int;; (* typ wartość składa się z dwóch floatów (nazwijmy je a,b) oraz liczby int, która określa jeden z dwóch możliwych wartości
0) dla liczby int=0 wartość przyjmuje postać zakresu <a,b>
1) dla liczby int=1 wartość przyjmuje postać zakresu (-inf,a> U <b,inf), przedział (a,b) jest więc dopełnieniem zakresu wartości
*)

(* 3 funkcje odwołujące się do poszczególnych elementów typu wartość *)
let low (a, _, _) = a;; (* 'a * 'b * 'c -> 'a *)
let high (_, b, _) = b;;  (* 'a * 'b * 'c -> 'b *)
let bul (_,_,c) =c;; (* 'a * 'b * 'c -> 'c *)

(* Funkcja sprawdzająca czy dany float jest równy nan*)
let is_nan n =  (* float -> bool  *)
  compare n nan = 0;;

(* Funkcja zwracająca znak danego float *)
let znak x = (*float -> int *)
    if (x<0.) then (-1)
    else if (x>0.) then 1
    else 0;;

(* Funkcja zwracająca minimum z dwóch floatów a,b z uwzględnieniem nan *)    
let min a b = (* float -> float -> float *)
    if (is_nan a || a>b) then b 
    else a;;
    
(* Funkcja zwracająca maximum z dwóch floatów a,b z uwzględnieniem nan *)
let max a b = (* float -> float -> float *)
    if (is_nan a || a<b) then b
    else a ;;
    
(* Funkcja zwracająca minimum z czterech floatów a,b,c,d *)
let max_4 a b c d = (* float -> float -> float -> float -> float *)
    max (max a b) (max c d)

(* Funkcja zwracająca maximum z czterech floatów a,b,c,d *)    
let min_4 a b c d = (* float -> float -> float -> float -> float *)
    min (min a b) (min c d)

(* wartosc_dokladnosc x p = x +/- p% dla war.pocz.: p > 0   *)
let wartosc_dokladnosc x p = (* float -> float -> wartosc  *)
    if (x>=0.) then (x -. x*.(p/.100.0), x+.x*.(p/.100.0), 0) 
    else (x +. x*.(p/.100.0), x-.x*.(p/.100.0), 0) ;;

(* wartosc_od_do x y = <x;y> dla x <= y  *)
let wartosc_od_do x y = (* 'a -> 'b -> 'a * 'b * int *)
(x,y,0);;

(* wartosc_dokladna x = <x;x>   *)
let wartosc_dokladna x = (* 'a -> 'a * 'a * int *)
(x,x,0);;

(* in_wartosc w x = x \in w z uwzględnieniem nan*)
let in_wartosc w x = (*  wartosc -> float -> bool *)
    if (is_nan (low w)) then false
    else if (bul w = 0) then ( (x>= (low w)) && (x<= (high w) ) )
    else ( ( x<=(low w) ) || (x>= (high w) ) );;

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
let min_wartosc w = (* wartosc -> float *)
    if (is_nan (low w)) then nan
    else if (bul w = 0) then low w
    else neg_infinity;;

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
let max_wartosc w = (* wartosc -> float *)
   if (is_nan (low w)) then nan
    else if (bul w = 0) then high w
    else infinity;;

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli sródek przedziału nie jest określony.*)
let sr_wartosc w = (* wartosc -> float *)
    if (is_nan (low w)) then nan
    else if (bul w = 0) then
        if ( (low w) = neg_infinity && (high w) = infinity ) then nan
        else if ( (low w) = neg_infinity ) then neg_infinity
        else if ( (high w) = infinity)  then infinity
        else ( (low w)+. (high w) ) /. 2.0
    else nan;;

(* Funkcja zwracająca sumę dwóch wartości *)
let plus x y = (* wartosc -> wartosc -> wartosc *)
    if (is_nan (low x) || is_nan (low y)) then (nan, nan, 0) (* Uwzględnienie nan*)
    (* Rozbicie dodawanie na 4 przypadki w zależności od paramteru bul wartości x,y *)
    else if (bul x = 0 && bul y = 0) then ((low x) +. (low y), (high x) +. (high y) , 0)  
    else if (bul x = 1 && bul y = 1) then (neg_infinity,infinity,0)
    else if (bul x = 1 && bul y = 0 ) then 
        let n = ( (low x) +. (high y), (high x) +. (low y), 1) in (* Funkcja zwracająca pomocniczą sumę x,y *)
            if ( (low n) >= (high n) ) then (neg_infinity, infinity, 0)
            else n
    else 
        let n = ( (low y) +. (high x), (high y) +. (low x) , 1 ) in (* Funkcja zwracająca pomocniczą sumę x,y*)
            if ( (low n) >= (high n) ) then (neg_infinity, infinity, 0)
            else n;;

(* Funkcja zwracająca różnice dwóch wartości *)
let minus x y = (* wartosc -> wartosc -> wartosc *)
    let przeciw n = (* Funkcja zmieniająca wartość, na wartość przeciwną do niej *)
        ( (-1.) *. (high n), (-1.) *. (low n) , bul n) in
    plus x (przeciw y);; (* zmiana x-y na x+(-y) *)

(* Funkcja zwracająca iloczyn dwóch wartości dla dowolnego x, oraz y postaci (a,b,0) dla a>=0 i b>=0*)
let pom_mnoz x y = (* wartosc -> wartosc -> wartosc *)
    if (bul x = 0) then ( (min ( (low x)*.(low y) ) ( (low x)*.(high y) )  ), (max ( (high x)*.(low y) ) ( (high x)*. (high y )) ), 0  ) (* Rozbicie na 2 przypadki w zależności od bul x,y*)
    else 
        let n = ( (max ( (low x)*.(low y) ) ( (low x)*.(high y) )  ), (min ( (high x)*.(low y) ) ( (high x)*. (high y )) ), 1 ) in (* Funkcja zwracająca pomocniczy iloczyn x,y*)
        if (znak (low x) = znak (high x) ) then
            if ( (low n) >= (high n) ) then (neg_infinity, infinity, 0)
            else n
        else n

(* Funkcja pośrednia pomiędzy funkcją razy, a pom_mnoz dla x postaci (a,b,1) oraz y postaci (a,b,0)*)
let pom_mnoz_wide x y = (* wartosc -> wartosc -> wartosc *)
    if (low y >=0. && high y >=0.) then pom_mnoz x y
    else if (low y <=0. && high y <=0.) then pom_mnoz ( ((high x) *. (-1.) ), ( (low x) *. (-1.) ) , 1 ) ( ((high y) *. (-1.) ), ( (low y) *. (-1.) ) , 0 )
    else (neg_infinity, infinity, 0);;

(* Funkcja zwracająca iloczyn dwóch wartości *)
let razy x y = (* wartosc -> wartosc -> wartosc *)
    if (is_nan (low x) || is_nan (low y) ) then (nan,nan,0) (* Uwzględnienie nan*)
    else if (y = (0.,0.,0) || x = (0.,0.,0)  ) then (0.,0.,0) (* Uwzględnienie mnożenie przez <0,0>*)
    else if (bul x = 0 && bul y = 0) then (* Rozbicie mnożenia na 4 przypadki w zależności od paramteru bul wartości x,y *)
    (* Rozbicie mnożenia wartości x,y z parametrem bul rónym 0 w zależności od znaków low y, high y *)
        if (low y >=0. && high y >=0.) then pom_mnoz x y 
        else if (low y <0. && high y <0.) then pom_mnoz ( (high x *. (-1.) ), ( (low x) *. (-1.) ) , 0 ) ( (high y *. (-1.) ), ( (low y) *. (-1.) ) , 0 ) (* Przemnożenie obu wartości przez (-1) aby parametry a,b od y były dodatnie*)
        else (min_4 ((low x) *. (low y)) ((high x) *. (low y)) ((low x) *. (high y)) ((high x) *. (high y)) , max_4 ((low x) *. (low y)) ((high x) *. (low y)) ((low x) *. (high y)) ((high x) *. (high y))  ,0  ) 
    else if (bul x = 1 && bul y = 0) then pom_mnoz_wide x y
    else if (bul x = 0 && bul y = 1) then pom_mnoz_wide y x   (* Zmiana x*y na y*x, tak aby można było je przekazać do funkcji pom_mnoz_wide *)
    else (* Rozbicie mnożenia wartości x,y z parametrem bul rónym 1 w zależności od znaków low y, high y, low x, high x   *)
        if ( (znak (low x) = znak (high x) ) || (znak (low y) = znak (high y) ) ) then (neg_infinity, infinity, 0) 
        else if ( (low x = 0.) || (low y = 0.) || (high x = 0.) || (high y = 0.) ) then (neg_infinity, infinity, 0) 
        else ( max ((low x) *. (high y)) ((high x) *. (low y)) , min ((low x) *. (low y)) ((high x) *. (high y)) ,1);;

(* Funkcja zwracająca odwrotność liczby x*)
let odwr x = (* wartosc -> wartosc *)
    if ((low x) = 0. && (high x) = 0.) then (0.,0.,bul x)
    else if ((low x) = 0.) then (0., (1. /. (high x)), bul x)
    else if  ((high x) = 0.) then ( (1. /. (low x)), 0. , bul x)
    else if (znak (low x) != znak (high x)) then (1. /. (low x), 1. /. (high x), 0 )
    else (min (1. /. low x)  (1. /. high x), max (1. /. low x)  (1. /. high x), bul x);;

(* Funkcja pomocnicza dla dzielenia, mnożąca x y, gdzie odpowiednia z nich została już odwrócona *)
let pom_dziel x y = (* wartosc -> wartosc -> wartosc *)
    if (bul x = 0) then 
        if (low y >=0. && high y >=0.) then pom_mnoz x y
        else if (low y <0. && high y <0.) then pom_mnoz ( (high x *. (-1.) ), ( (low x) *. (-1.) ) , 0 ) ( (high y *. (-1.) ), ( (low y) *. (-1.) ) , 0 )
        else (min_4 ((low x) *. (low y)) ((high x) *. (low y)) ((low x) *. (high y)) ((high x) *. (high y)) , max_4 ((low x) *. (low y)) ((high x) *. (low y)) ((low x) *. (high y)) ((high x) *. (high y))  ,0  ) 
    else if (low y=0.) then 
        if (znak (low x) = znak (high x) ) then (neg_infinity, infinity, 0) 
        else ( (low x) *. (high y) , (high x) *. (high y), 1)
     else if (high y=0.) then
        if (znak (low x) = znak (high x) ) then (neg_infinity, infinity, 0) 
        else ( (high x) *. (low y) , (low x) *. (low y), 1)
    else if (low y >0. && high y >0.) then pom_mnoz_wide x y
    else if (low y <0. && high y <0.) then pom_mnoz_wide ( ( (high x) *. (-1.) ), ( (low x) *. (-1.) ) , 1 ) ( ( (high y) *. (-1.) ), ( (low y) *. (-1.) ) , 0 )
    else 
        if (znak (low x) = znak (high x) ) then (neg_infinity, infinity, 0) 
        else ( (max ((low x) *. (high y)) ((high x) *. (low y))), (min ((low x) *. (low y)) ((high x) *. (high y))) , 1 )

(* Funkcja zwracająca iloraz dwóch wartości *)
let podzielic x y = (* wartosc -> wartosc -> wartosc *)
    if (is_nan (low x) || is_nan (low y) ) then (nan,nan,0)   (* Uwzględnienie nan*)
    else if (y = (0.,0.,0) ) then (nan,nan,0) (* Uwzględnienie dzielenia przez <0,0> *)
    else if (x= (0.,0.,0)) then x   (* Uwzględnienie dzielenia <0,0> przez cokolwiek *)
    else if (bul x = 0 && bul y = 0) then (* Rozbicie dzielenia wartości x,y z parametrem bul rónym 0 w zależności od znaków low y, high y *)
        if (low y=0.) then (* Rozpatrzenie dzielenia przez <0,b>  *)
            if ( low x < 0. && high x > 0.) then (neg_infinity,infinity,0)
            else if (low x >= 0.) then ( ( (low x) /. (high y) ), infinity, 0 ) 
            else (neg_infinity, ( (high x) /. (high y) ), 0)
        else if (high y=0.) then (* Rozpatrzenie dzielenia przez <a,0>  *)
            if ( low x < 0. && high x > 0.) then    (neg_infinity,infinity,0)
            else if (high x <= 0.) then ( ( (high x) /. (low y) ), infinity, 0 ) 
            else (neg_infinity, ( (low x) /. (low y) ), 0)
        else if (low y >0. && high y >0.) then pom_mnoz x (odwr y)  (* zmiana dzielenia na mnozenie x:y = x*1/y *)
        else if (low y <0. && high y <0.) then pom_mnoz ( (high x *. (-1.) ), ( (low x) *. (-1.) ) , 0 ) ( 1. /. (high y *. (-1.) ), ( 1. /. (low y) *. (-1.) ) , 0 ) (* zmiana dzielenia na mnozenie x:y = x*1/y oraz pomnozeniu obu wartości przez (-1) aby parametry a,b od y były dodatnie*)
        else (* Rozbicie dzielenia wartości x przez wartość y, dla y którego low y, high y są różnych znaków*)
            if (low x > 0. && high x > 0.) then ( ((low x) /. (low y))  , ((low x) /. (high y)), 1 )
            else if (low x < 0. && high x < 0.) then ( ((high x) /. (high y))  , ((high x) /. (low y)), 1 )
            else  (neg_infinity, infinity,0) 
    else if (bul x = 1 && bul y = 0) then pom_dziel x (odwr y) (* zmiana dzielenia na mnozenie x:y = x*1/y *)
    else if (bul x = 0 && bul y = 1) then pom_dziel (odwr y) x (* zmiana dzielenia na mnozenie x:y = x*1/y = 1/y*x *)
    else (neg_infinity, infinity, 0) ;;
