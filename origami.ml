(** Autor: Jakub Jakubowski *)
(** Code review: Barbara Rosiak *)

(** Punkt na płaszczyźnie *)
type point = float * float ;;

(** Poskładana kartka: ile razy kartkę przebije 
    szpilka wbita w danym punkcie *)
type kartka = point -> int ;;

(* Stała niedokładności *)
let eps = 1e-9 ;;

(** Funkcja przyjmująca floata x, zwracająca 
    x podniesionego do kwadratu *)
let pot x = 
    x*.x ;;

(** Funkcja przyjmująca dwa punkty, obliczająca 
    kwadrat odległości między nimi *)
let dist (x1,y1) (x2,y2) = 
     pot (x2-.x1) +. pot (y2-.y1) ;; 

(** Funkcją przyjmująca dwa punkty, tworząca z nich prostą 
    typu y = ax + b. Funkcja zwraca krotkę (a,b) *)
let get_line (x1,y1) (x2,y2) = 
    if x1 = x2 then (max_float, x1)
    else
        let a = (y2  -. y1 ) /. (x2 -.x1  ) in
        let b = y1 -. (a *. x1) in
            (a, b) ;;

(** Funkcja przyjmująca trzy punkty, zwracająca odbicie pierwszego z 
    nich względem prostej utoworzonej z drugiego i trzeciego punktu *)
let mirror (x,y) (x1,y1) (x2,y2) =
    let (a,b) = get_line (x1,y1) (x2,y2) in
        if a = max_float then 
            (2.*.b -. x, y)
        else
            ( ( (1.-.(pot a))*.x  +.  2.*.a*.y  -.  2.*.a*.b ) /. (pot a +. 1.) ,
            ( 2.*.a*.x  +.  ((pot a)-.1.)*.y  +.  2.*.b ) /. (pot a +. 1.) )  ;;

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
prostokąt o bokach równoległych do osi układu współrzędnych i lewym
dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat ((x1,y1) : point) ((x2,y2) : point) : kartka = 
    fun (x,y) -> 
        if x1 <= x && x <= x2 && y1 <= y && y <= y2 then 1
        else 0 ;;

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
    w punkcie [p] i promieniu [r] *)
let kolko ((x1,y1) : point) r : kartka =
    fun (x,y) -> 
        if (dist (x1,y1) (x,y)) -. pot r  <= eps then 1
        else 0 ;;

(** Funkcją przyjmująca trzy punkty, ustalająca po której stronie prostej 
    utworzonej z drugiego oraz trzeciego punktu prostej znajduje się
    pierwszy punkt. Zwraca 1, gdy znajduje się on po lewej, -1 gdy
    po prawej lub 0 gdy znajduje się na tej prostej *)
let orient (x1, y1) (x2, y2) (x3, y3) =
    let v = (y2 -. y1) *. (x3 -. x2) -. (y3 -. y2) *. (x2 -. x1) in
        if v = 0. then 0 (** Na prostej *)
        else if  v > 0. then -1 (** Prawa *) 
        else 1 ;; (** Lewa *)

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej
przez punkty [p1] i [p2] (muszą to być różne punkty). Papier jest
składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
od [p1] do [p2]) jest przekładany na lewą. Wynikiem funkcji jest
złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
się na punkt przebicia. *)
let zloz (a : point) (b : point) (kart : kartka) : kartka =
    fun p -> 
        let poz = orient a b p in
            if poz = -1 then 0 (** Punkt znajduje się po prawej *)
            else if poz = 0 then kart p (** Punkt znajduje się na zgięciu *)
            else kart p + kart (mirror p a b) ;; (** Punkt znajduje się po lewej *)

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych 
z listy *)
let skladaj lst kart =
    let pom kart (a,b) =
        zloz a b kart
    in List.fold_left pom kart lst ;; 