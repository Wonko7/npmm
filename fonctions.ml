
#use"constantes.ml";;


open Complex;;

(*-------controle de frequence--------*)
let fup d f = let f = f+.1000.*.d in if f > 22000. then 22000. else f;;
let fdown d f = let f = f-.2000.*.d in if f < 11000. then 11000. else f;;

(* coordonnees cartesiennes vers coordonnees polaires *)
let cplx x y = let cx = polar x 0. and cy = polar y (pi/.2.) in add cx cy;;
let cplx2 x y = cplx (float_of_int x) (float_of_int y);;
(* addition complexe *)
let (++) = function x -> function y -> add x y;;
let (--) = function x -> function y -> sub x y;;
(* multiplication scalaire*cplx *)
let ( ** ) = function x -> function y -> mul (polar x 0.) y;;
let (//)  = function x -> function y -> div x (polar y 0.);;
(* fait pivoter un vecteur *)
let pivot c alpha = polar (norm c) (arg c +. alpha);;

(* equivalent de abs pour les float *)
let fabs x = if x < 0. then x *. (-.1.) else x;;

let neg_dir x =(45*x*x*x-600*x*x+2345*x-2150)/100;;

let if_under arg max = if (fabs arg) < max then arg else if arg < 0. then -.max else max;;

let rec div x y = if x < y then 0 else 1 + div (x-y) y;; 


(* calcul la distance entre 2 objets *)

let distance obj1 obj2 = norm (obj2#get_coord -- obj1#get_coord);;



let rec isin n = function
    [] -> false
  |e::l -> (e=n)||(isin n l);;


(*determination de l'orientation du vehicule*)
let dir_voit voit  =match (arg voit#get_vitesse) with
    y when (y>= pi/.4. )&&(y<=3.*.pi/.4.)  ->  2
|   y when (y>=0.-.pi/.4.)&&(y<=0.-.3.*.pi/.4.) ->  5
|   y when (y>=0.-.pi/.4.)&&(y<=pi/.4.) ->3  
|     _                                  -> 7;;

   
