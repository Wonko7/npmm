#load "unix.cma";;
Random.self_init ();;
#load "graphics.cma";;
open Graphics;;


(* Classe des vehicules *)
let pi = acos (-.1.);;  (* Definition de pi *)
let rayon_voiture = 15.;;
let envergure_voiture = pi /. 6.;;
let inertie = 90.;;      (* taux inertiel en % *)
let aero = -.0.4;;         (* taux de frottements dus a l'air *)
let bitume = 90.;;       (* taux de frottements dus au bitume *)
let no_neg x = if x < 0. then 0. else x;;

let rayon_mob = 5.;;
let envergure_mob = pi /. 15.;;

let vitesse_limite = 10.;;

let distance_min = 70.;;
let distance_max = 300.;;
let taille_case = 50.;;

let rayon_immeuble = taille_case/.(sqrt 2.);;

let cote = 50;;

let compt_cars = ref 0;; (* compteur du nombre de voitures dans la liste *)

let num_ia = ref (-1);;
