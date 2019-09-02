

(* Fonctions de calcul des collisions *)

(* verifie si il y a collision spherique *)

let sphere obj1 obj2 = 
  (distance obj1 obj2) < (obj1#get_rayon +. obj2#get_rayon);;

(* prend un objet et renvoie un quadruplet de doublet contenant les coordonnees des points des coins du vehicule *)

let corner obj =
  let x = obj#get_x
  and y = obj#get_y
  and r = obj#get_rayon
  and angle = obj#get_car_angle
  and envergure = obj#get_envergure in
    ((x+.r*.cos (angle-.envergure),y+.r*.sin (angle-.envergure)),(x+.r*.cos (angle+.envergure),y+.r*.sin (angle+.envergure)),(x+.r*.cos (angle-.envergure+.pi),y+.r*.sin (angle-.envergure+.pi)),(x+.r*.cos (angle+.envergure+.pi),y+.r*.sin (angle+.envergure+.pi)));;

(* cette fonction sera appliquee partiellement pour obtenir les droites des cotes des objets *)
(* format de sortie (fun,min,max,sens) *)

let affine a b x = a*.x +. b;;

let functionize (x1,y1) (x2,y2) = let (x1,x2) = if x1=x2 then (x1-.0.0000000001,x2) else (x1,x2) in let a = (y2-.y1)/.(x2-.x1) in
 (affine a (y1-.x1*.a),min x1 x2,max x1 x2);;

(* determine si 2 droites se croisent *)


let cross (f1,x1,x2) (f2,x3,x4) =
      ((x2 >= x3)&&(x4 >= x1))&&(((f1 (max x1 x3) -. f2 (max x1 x3))*.(f1 (min x2 x4) -. f2 (min x2 x4))) <= 0.);;

(* cree ds fonctions a partir des coins d un objet *)


let make_functions ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) =
  [|functionize (x1,y1) (x2,y2);functionize (x2,y2) (x3,y3);functionize (x3,y3) (x4,y4);functionize (x4,y4) (x1,y1);|];;

(* determine le point de croisement de 2 fonctions *)

let shot (f1,x1,x2) (f2,x3,x4) =
  if (f1 0. -. f2 0. = 0.)&&(f1 x1 -. f2 x1 = 0.) then let t = (max x1 x3 +. min x2 x4)/.2. in (t, f1 t) 
  else
    let diff = (min x2 x4) -. (max x1 x3)
    and t1 = f1 (max x1 x3) and t2 = f1 (min x2 x4) and t3 = f2 (max x1 x3) and t4 = f2 (max x2 x4) in
    let x= (max x1 x3) +. ( diff *. ((fabs (t1 -. t3))/.(fabs (t1 -. t3) +. fabs (t4 -. t2)))) in
    (x,f1 x);;





(* calcul le barycentre des points ponderes de la collision *)



let barycentre l =
  let rec center = function
     ([e],_) -> e
    |((x1,y1)::(x2,y2)::l,n) -> center ((((x1*.n +. x2)/.(n+.1.)),((y1*.n +. y2)/.(n+.1.)))::l,n+.1.) in
  center (l,1.);;


(* prend 2 objets, verifie les collisions par bounding box, mettre a jour *)

let impact obj1 obj2 mob =
  let c1 = !obj1#get_corners and c2 = !obj2#get_corners and l = ref [] in
  let l1 = make_functions c1 and l2 = make_functions c2 in
  for i=0 to 3 do
    for j=0 to 3 do
      if (cross l1.(i) l2.(j)) then
	l := (shot l1.(i) l2.(j))::!l;
    done;
  done;
  if !l <> [] then begin
    let (x,y) = barycentre !l in
    let targ1 = arg (cplx x y -- !obj1#get_coord)
    and targ2 = arg (cplx x y -- !obj2#get_coord)
    and mas1 = !obj1#get_masse
    and mas2 = !obj2#get_masse in
    let vect2 = mas2 ** (polar (no_neg (pivot !obj2#get_vitesse (-.targ2)).re) targ2)
    and vect1 = mas1 ** (polar (no_neg (pivot !obj1#get_vitesse (-.targ1)).re) targ1) in

    let ratio1 = mas2 /. (mas1+.mas2) +. 1.
    and ratio2 = mas1 /. (mas1+.mas2) +. 1. in

      (* traitement objet 1 *)
    !obj1#collide(ratio2**vect2 -- ratio1**vect1);
      (* traitement objet 2 *)
    !obj2#collide(ratio1**vect1 -- ratio2**vect2);
    

    let dis = norm (cplx x y -- !mob#get_coord) in
    
    if dis < 150. then (); !l
  end
  else [];;




(* Collision immeuble et creation voiture *)


let perimetre = 5;;
let p_div_2 = 2;;
(* etat de la mob a l appel de la fonction *)

type round_state = Init
  |Left
  |Right
  |Up
  |Down;;



(* traitement simplifie des liste *)

let plussoie (a,b) = function
    2 -> (a,b+1)
  |3 -> (a-1,b)
  |5 -> (a,b-1)
  |7 -> (a+1,b);;

let is_div a b = (a <> 0)&&(a = ((div a b)*b));;

let rec isin3 (i,j) = function
    [] -> false
  |e::l -> (let (a,b) = e#get_case in ((a=i)&&(b=j)))||(isin3 (i,j) l);;

let ajoute_bat (xa,ya) (xb,yb) liste =
  for i = xa to xb do
    for j = ya to yb do
      if  (case (i,j) = 0)&&(not (isin3 (i,j) !liste)) then liste := (new immeuble(i,j))::!liste;
    done;
  done;;


let rec isin2 (i,j,dir) = function
    [] -> false
  |(a,b,d)::l -> ((a=i)&&(b=j))||(isin (i,j,dir) l);;

let ajoute_gen (xa,ya) (xb,yb) dir liste =
  for i = xa to xb do
    for j = ya to yb do
      if (case (plussoie (i,j) dir) <> 0)&&(not (isin2 (i,j,dir) !liste)) then
        if (is_div (case (i,j)) dir) then
          liste := (i,j,dir)::!liste
	else if (is_div (case (i,j)) (neg_dir dir)) then
          liste := (i,j,neg_dir dir)::!liste 
    done;
  done;;

let is_between x min max = (x >= min)&&(x <= max);;



let rec rec_retire_bat (xa,ya) (xb,yb) = function
    [] -> []
  |e::l -> let (x,y) = e#get_case in if (is_between x xa xb)&&(is_between y ya yb) then (rec_retire_bat (xa,ya) (xb,yb) l) else e::(rec_retire_bat (xa,ya) (xb,yb) l);; 
   
let retire_bat (xa,ya) (xb,yb) liste =
  liste := (rec_retire_bat (xa,ya) (xb,yb) !liste);;








					
let rec rec_retire_gen (xa,ya) (xb,yb) = function
    [] -> []
  |(x,y,d)::l -> if (is_between x xa xb)&&(is_between y ya yb) then (rec_retire_gen (xa,ya) (xb,yb) l) else (x,y,d)::(rec_retire_gen (xa,ya) (xb,yb) l);; 
   
let retire_gen (xa,ya) (xb,yb) liste =
  liste := (rec_retire_gen (xa,ya) (xb,yb) !liste);;



let rec vire_de_la_liste x =  function
    [] -> []
  |(a,b,d)::l -> if ((a,b) = x)||(plussoie (a,b) d = x) then l else (a,b,d)::(vire_de_la_liste x l);; 


(* procedure a appliquer en cas de changement de case *)

let ronde (x,y) etat bat gen =
  match etat with
  |Init -> 
      ajoute_gen (x-p_div_2+1,y+p_div_2) (x+p_div_2-1,y+p_div_2) 2 gen;
      ajoute_gen (x-p_div_2+1,y-p_div_2) (x+p_div_2-1,y-p_div_2) 5 gen;
      ajoute_gen (x-p_div_2,y-p_div_2+1) (x-p_div_2,y+p_div_2-1) 3 gen;
      ajoute_gen (x+p_div_2,y-p_div_2+1) (x+p_div_2,y+p_div_2-1) 7 gen;
      ajoute_bat (x-p_div_2,y-p_div_2) (x+p_div_2,y+p_div_2) bat;
  |Left -> 
      ajoute_bat (x-p_div_2,y-p_div_2) (x-p_div_2,y+p_div_2) bat;
      retire_bat (x+p_div_2+1,y-p_div_2) (x+p_div_2+1,y+p_div_2) bat;
      ajoute_gen (x+p_div_2,y-p_div_2+1) (x+p_div_2,y+p_div_2-1) 7 gen;
      retire_gen (x+p_div_2+1,y-p_div_2+1) (x+p_div_2+1,y+p_div_2-1) gen;
      ajoute_gen (x-p_div_2,y-p_div_2+1) (x-p_div_2,y+p_div_2-1) 3 gen;
      retire_gen (x-p_div_2+1,y-p_div_2+1) (x-p_div_2+1,y+p_div_2-1) gen;
      retire_gen (x+p_div_2,y-p_div_2) (x+p_div_2+1,y-p_div_2) gen;
      retire_gen (x+p_div_2,y+p_div_2) (x+p_div_2+4,y+p_div_2) gen;
      ajoute_gen (x-p_div_2+1,y-p_div_2) (x-p_div_2+1,y-p_div_2) 5 gen;
      ajoute_gen (x-p_div_2+1,y+p_div_2) (x-p_div_2+1,y+p_div_2) 2 gen;

  |Right ->
      ajoute_bat (x+p_div_2,y-p_div_2) (x+p_div_2,y+p_div_2) bat;
      retire_bat (x-p_div_2-1,y-p_div_2) (x-p_div_2-1,y+p_div_2) bat;
      ajoute_gen (x-p_div_2,y-p_div_2+1) (x-p_div_2,y+p_div_2-1) 3 gen;
      retire_gen (x-p_div_2-1,y-p_div_2+1) (x-p_div_2-1,y+p_div_2-1) gen;
      ajoute_gen (x+p_div_2,y-p_div_2+1) (x+p_div_2,y+p_div_2-1) 7 gen;
      retire_gen (x+p_div_2-1,y-p_div_2+1) (x+p_div_2-1,y+p_div_2-1) gen;
      retire_gen (x-p_div_2-1,y-p_div_2) (x-p_div_2,y-p_div_2) gen;
      retire_gen (x-p_div_2-1,y+p_div_2) (x-p_div_2,y+p_div_2) gen;
      ajoute_gen (x+p_div_2-1,y-p_div_2) (x+p_div_2-1,y-p_div_2) 5 gen;
      ajoute_gen (x+p_div_2-1,y+p_div_2) (x+p_div_2-1,y+p_div_2) 2 gen;
  |Up -> 
      ajoute_bat (x-p_div_2,y-p_div_2) (x+p_div_2,y-p_div_2) bat;
      retire_bat (x-p_div_2,y+p_div_2+1) (x+p_div_2,y+p_div_2+1) bat;
      ajoute_gen (x-p_div_2+1,y-p_div_2) (x+p_div_2-1,y-p_div_2) 5 gen;
      retire_gen (x-p_div_2+1,y-p_div_2+1) (x+p_div_2-1,y-p_div_2+1) gen;
      ajoute_gen (x-p_div_2+1,y+p_div_2) (x+p_div_2-1,y+p_div_2) 2 gen;
      retire_gen (x-p_div_2+1,y+p_div_2+1) (x+p_div_2-1,y+p_div_2+1) gen;
      retire_gen (x-p_div_2,y+p_div_2) (x-p_div_2,y+p_div_2+1) gen;
      retire_gen (x+p_div_2,y+p_div_2) (x+p_div_2,y+p_div_2+1) gen;
      ajoute_gen (x-p_div_2,y-p_div_2+1) (x-p_div_2,y-p_div_2+1) 3 gen;
      ajoute_gen (x+p_div_2,y-p_div_2+1) (x+p_div_2,y-p_div_2+1) 7 gen;
  |Down ->
      ajoute_bat (x-p_div_2,y+p_div_2) (x+p_div_2,y+p_div_2) bat;
      retire_bat (x-p_div_2,y-p_div_2-1) (x+p_div_2,y-p_div_2-1) bat;
      ajoute_gen (x-p_div_2+1,y+p_div_2) (x+p_div_2-1,y+p_div_2) 2 gen;
      retire_gen (x-p_div_2+1,y+p_div_2-1) (x+p_div_2-1,y+p_div_2-1) gen;
      ajoute_gen (x-p_div_2+1,y-p_div_2) (x+p_div_2-1,y-p_div_2) 5 gen;
      retire_gen (x-p_div_2+1,y-p_div_2-1) (x+p_div_2-1,y-p_div_2-1) gen;
      retire_gen (x-p_div_2,y-p_div_2-1) (x-p_div_2,y-p_div_2) gen;
      retire_gen (x+p_div_2,y-p_div_2-1) (x+p_div_2,y-p_div_2) gen;
      ajoute_gen (x-p_div_2,y+p_div_2-1) (x-p_div_2,y+p_div_2-1) 3 gen;
      ajoute_gen (x+p_div_2,y+p_div_2-1) (x+p_div_2,y+p_div_2-1) 7 gen;;
(* dessine pour tester *)














(* tests *)

(* draw_map mappy;;
ronde (6,2) Init immeubles generatrices;;
draw_liste (rgb 255 0 0) !immeubles;;
draw_liste (rgb 0 0 255) !generatrices;;
ronde (6,3) Down immeubles generatrices;;
draw_map mappy;;
draw_liste (rgb 255 0 0) !immeubles;;
draw_liste (rgb 0 0 255) !generatrices;;
ronde (7,3) Right immeubles generatrices;;
draw_map mappy;;
draw_liste (rgb 255 0 0) !immeubles;;
draw_liste (rgb 0 0 255) !generatrices;; *)







let rec crash_dummies car liste mob =
  match !liste with
    [] -> ()
  |e::l -> 
      if (sphere !car e) 
      then ignore (impact car (ref e) mob );
      crash_dummies car (ref l)  mob;;

