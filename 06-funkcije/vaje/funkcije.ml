(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec bad_reverse = function
  | [] -> []
  | x :: xs -> bad_reverse xs @ [x] (*limanje dveh seznamov skupej je časovno zahtevno *)

let rec reverse list =
  let rec reverse_aux acc list =
    match list with
    | [] -> acc
    | x :: xs -> reverse_aux (x::acc) xs
  in
  reverse_aux [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n = 
  if n <= 0 
    then [] 
  else 
    x :: repeat x (n-1)

let rec repeat_tlrec x n =
  let rec aux x n acc = (*načeloma tukej ne bi blo treba dat x, ma damo k nismo geniji*)
    if n <= 0 then
      acc
    else
      aux x (n-1) (x::acc)
  in
  aux x n []


(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let rec range k = 
  let rec aux k acc=
    if k < 0 then
      acc
    else
      aux (k-1) (k::acc)
  in
  aux k []

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs


(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f list =
  let rec map_aux acc = function 
    | [] -> acc
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] list |> reverse

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)


let rec mapi f list =
  (*f je funckija dveh spremenljivk naredili bomo dva akumulatorja i in acc *)
  let rec mapi_aux i acc = function
  | [] -> reverse acc
  | x::xs -> mapi_aux (i+1) (f x i ::acc) xs
  in
  mapi_aux 0 [] list 

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

 let rec zip' list1 list2 =
  match list1 with
  | x :: xs ->(
      match list2 with
      | y :: ys -> (x,y) :: zip' xs ys
      | [] -> failwith "Different lengths of input lists!")
  | [] -> (
        match list2 with
        |y::ys -> failwith "Different lengths of input lists!"
        | [] -> [] )

 let rec zip'' list1 list2 =
  match list1, list2 with 
  | x::xs, y::ys -> (x,y) :: zip'' xs ys
  | x:: xs, [] -> failwith "Different lengths of input lists!"
  | [], y::ys -> failwith "Different lengths of input lists!"
  | [],[] -> []

let rec zip list1 list2 =
  match list1, list2 with 
  | x::xs, y::ys -> (x,y) :: zip xs ys
  | _ :: _, [] | [], _::_ -> failwith "Different lengths of input lists!"
  | [],[] -> []

let rec zip''' list1 list2 =
  match list1, list2 with 
  | x::xs, y::ys -> (x,y) :: zip''' xs ys
  | [],[] -> []
  | _ -> failwith "Different lengths of input lists!"


(*repna rekurzija *)
let rec zip list1 list2 =
  let rec zip_tlrec acc list1 list2=
    match list1, list2 with
    |x :: xs, y::ys -> zip_tlrec ((x,y)::acc) xs ys
    | [], [] -> reverse acc
    |_ ->failwith "Different lengths of input lists!"
  in
  zip_tlrec [] list1 list2


(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  |[] -> ([],[])
  |xy :: xys ->
    let (x,y) = xy in 
    let (xs, ys) = unzip xys in 
    (x::xs, y::ys)

(*unzip: ('a -> 'b) list -> (a'list) * ('b list)*)


let rec unzip = function
  |[] -> ([],[])
  |(x,y) :: xys -> let (xs, ys) = unzip xys in (x::xs, y::ys)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec list = 
  let rec unzip_aux xacc yacc = function (*damo lahko v par acc če ima funckija poljubno mnogo elementov*)
    | (x,y) :: xys -> unzip_aux (x::xacc) (y::yacc) xys
    | [] -> (reverse xacc, reverse yacc)
  in
  unzip_aux [] [] list

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12

 # fun x -> x < 10 je ekvivalentno loop ((>)  10) ((+) 4) 4 
[*----------------------------------------------------------------------------*)

let rec loop pogoj f x =
  if pogoj x then 
    let x' = f x in 
    loop pogoj f x'
  else
    x

(* let rec loop' pogoj f x =
  if pogoj x then 
    loop' pogoj f (f x) *)



(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f = function 
  | [] | _::[]-> failwith "List too short"
  | x::y::[] -> f x y
  | x::y:: xs -> fold_left_no_acc f ((f x y):: xs)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x n =
  let rec apply_aux f x n acc =
    if n < 0 then reverse acc 
    else apply_aux f(f x) (n-1) (x::acc)
  in 
  apply_aux f x n []


(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f list = 
  let rec filter_aux acc list = 
    match list with 
    | x::xs -> if f x = true then filter_aux (x::acc) xs else filter_aux acc xs
      | _ -> acc
  in 
  filter_aux [] list |> reverse


(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f list =
  let rec exists_aux acc f list = 
    match list with 
    | x::xs -> if f x = true then exists_aux true f xs else exists_aux acc f xs
    | _ -> acc
  in
  exists_aux false f list

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default list = 
  let vrednosti = filter f list in 
  match vrednosti with
  | [] -> default
  | x::xs -> x 


(*repno rekurzivna iz rešitev*)
let rec first_resitve f default = function
  | [] -> default
  | x :: xs -> if f x then x else first_resitve f default xs
