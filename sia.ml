#use"class.ml";;
#use"gestion_map.ml";;
#use"pizzeria.ml";;
#use"collisions.ml";;
let compt_cars = ref 0;;

(*---------------------------------------DIRECTION-----------------------------------------*)


let rec got_it n = function
   2 -> if n mod 2 = 0 then 2::(got_it n 3) else got_it n 3
  |3 -> if n mod 3 = 0 then 3::(got_it n 5) else got_it n 5
  |5 -> if n mod 5 = 0 then 5::(got_it n 7) else got_it n 7
  |7 -> if n mod 7 = 0 then 7::(got_it n 9) else got_it n 9
  |_ -> [];;
  



let rec pursue (x,y) d = if ((mappy.(y).(x)#get_kind = 7) || (mappy.(y).(x)#get_kind < 6)) then mappy.(y).(x)#get_kind
else let (xx,yy) = match d with
       2 -> (x,y-1)
     |3 -> (x+1,y)
     |5 -> (x,y+1)
     |7 -> (x-1,y) 
     | _ -> failwith "erreur sur pursue" 
     in pursue (xx,yy) d ;;

let rec build_list d (x,y)= function
    [] -> ([],0)                                                              (*reglage de la frequence des demi-tours*)
   |e::l -> if (pursue (x,y) e mod (neg_dir e) = 0) then (build_list d (x,y) l)  
     else let (a,b) = (build_list d (x,y) l) in (e::a,b+1);;

let rec look_for l n = let e::r=l in if n = 0 then e else look_for r (n-1);;

let rec path d l carr (x,y)=
  if (carr < 2)&&(isin d l) then (d,carr+1)
    else let (a,b) = (build_list d (x,y) l) in let c = look_for a (Random.int b) in
      if c = d then (c,carr+1) else (c,carr-2);;




let signed x valu = if x < 0. then (0.-.valu) else valu;;

let turn mob (x,y) (d,carr) =let _ = ( match d with
  2 -> !mob#set_ia(cplx2 (50 * x + (div 50 2)) (500 - (y * 50)))
  |3 -> !mob#set_ia(cplx2 ((x + 1) * 50) (500 - (y * 50) - (div 50 2)))
  |5 -> !mob#set_ia(cplx2 (50 * x + (div 50 2)) (500 - ((y+1) * 50)))
  |7 -> !mob#set_ia(cplx2 (x * 50) (500 - (y * 50) - (div 50 2)));) in

!mob#set_dir(d);
!mob#set_carr(carr);;



(*----------------------------------------------------------------------------------------------------------*)

(*---------------------------------creation de vehicules--------------------------------------------------*)

(*create_car prend en parametres les coordonnées de la case, cree un vehicule(ou pas) et l'ajoute dans la liste*)

(* plus besoin


let create_car x y map list =  
   if map.(ordonnee-y-1).(x)#get_kind <> 0 then
(         
    let dir = map.(ordonnee-y-1).(x)#get_kind and coord_x = taille_case*.(float_of_int(x)+.0.5) and coord_y = taille_case*.(float_of_int(y)+.2.5) in
    match dir with
       2         -> list := ref(new voiture (cplx coord_x coord_y,(pi/.2.),cplx coord_x (coord_y -. taille_case/.2.),2,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
    |  3         -> list := ref(new voiture (cplx coord_x coord_y,0.,cplx (coord_x +.taille_case/.2.) coord_y,3,Sys.time()))::!list;
compt_cars := !compt_cars + 1;                 
    |  5         -> list := ref(new voiture (cplx coord_x coord_y,3.*.(pi/.2.),cplx coord_x (coord_y +. taille_case/.2.),5,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
    |  7         -> list := ref(new voiture (cplx coord_x coord_y,pi,cplx (coord_x -. taille_case/.2.) coord_y,7,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
    |  x         -> (
                       match x with
                       y when (y mod 2=0) -> list:= ref(new voiture (cplx coord_x coord_y,pi/.2.,cplx coord_x (coord_y -. taille_case/.2.),2,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
      |y when (y mod 3 = 0) -> list := ref(new voiture (cplx coord_x coord_y,0.,cplx (coord_x +.taille_case/.2.) coord_y,3,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
      |y when (y mod 5 = 0) -> list := ref(new voiture (cplx coord_x coord_y,3.*.(pi/.2.),cplx coord_x (coord_y +. taille_case/.2.),5,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
      |y when (y mod 7 = 0) -> list := ref(new voiture (cplx coord_x coord_y,pi,cplx (coord_x -. taille_case/.2.) coord_y,7,Sys.time()))::!list;
compt_cars := !compt_cars + 1;
      | _                   -> ()
));;

*)

let create_car_light (x,y,dir) list=
  let coord_x = taille_case*.(float_of_int(x)+.0.5) and coord_y = (475. -. float_of_int(y)*.taille_case) in
match dir with
       2         -> list := ref(new voiture (cplx coord_x (coord_y-.taille_case/.2.+.1.),(pi/.2.),cplx coord_x (coord_y +. taille_case/.2.),2,Sys.time()))::!list;
    |  3         -> list := ref(new voiture (cplx (coord_x-.taille_case/.2.+.1.) coord_y,0.,cplx (coord_x +.taille_case/.2.) coord_y,3,Sys.time()))::!list;
    |  5         -> list := ref(new voiture (cplx coord_x (coord_y+.taille_case/.2.-.1.),3.*.(pi/.2.),cplx coord_x (coord_y -. taille_case/.2.),5,Sys.time()))::!list;
    |  7         -> list := ref(new voiture (cplx (coord_x+.taille_case/.2.-.1.) coord_y,pi,cplx (coord_x -. taille_case/.2.) coord_y,7,Sys.time()))::!list;;



let rec new_cars liste = function
    [] -> ()
  |e::l -> create_car_light e liste;new_cars liste l;;




(*----------------------------------------REACTIONS---------------------------------------------------------*)


let ralentit vehic mob  = 
      let  orient = vehic#get_dir and mob_x = mob#get_x and mob_y =mob#get_y 
             and vehic_x = vehic#get_x and vehic_y = vehic#get_y in
          match orient with
       
      2    ->   ((mob_x<=vehic_x +. taille_case/.2.7)&& (mob_x>=vehic_x -. taille_case/.2.7)
                 && (mob_y>=vehic_y +. taille_case/.2.5)&& (mob_y<=vehic_y +.taille_case+. distance_min)&& (mob#get_dir <> 5))
||((mob_x >=vehic_x +. taille_case/.2.9)&&(mob_x <=vehic_x +. taille_case*.(3./.2.1))&&(mob_y>=vehic_y +.taille_case/.2.5)&&(mob_y<=vehic_y +.taille_case*.(3./.2.))&&(mob#get_dir=7));


                                           
|     3    ->  ((mob_x>=vehic_x +. taille_case/.2.5)&& (mob_x<=vehic_x +.taille_case +. distance_min)
                &&(mob_y>=vehic_y -. taille_case/.2.7)&& (mob_y<=vehic_y +. taille_case/.2.7)&& (mob#get_dir <>7)) 
 ||((mob_x >=vehic_x +. taille_case/.2.5)&&(mob_x <=vehic_x +. taille_case*.(3./.2.))&&(mob_y<=vehic_y -.taille_case/.2.9)&&(mob_y>=vehic_y -.taille_case*.(3./.2.1))&&(mob#get_dir=2));




|    5    ->  ((mob_x>=vehic_x -. taille_case/.2.7)&& (mob_x<=vehic_x +. taille_case/.2.7)
              &&(mob_y>=vehic_y -.taille_case -. distance_min)&& (mob_y<=vehic_y -. taille_case/.2.5)&&(mob#get_dir <> 2))
 ||((mob_x <=vehic_x -. taille_case/.2.9)&&(mob_x >=vehic_x -. taille_case*.(3./.2.1))&&(mob_y<=vehic_y -.taille_case/.2.5)&&(mob_y>=vehic_y -.taille_case*.(3./.2.))&&(mob#get_dir=3));                   


|     7    -> ((mob_x>=vehic_x -.taille_case -. distance_min)&&(mob_x<=vehic_x -. taille_case/.2.5)
          &&(mob_y>=vehic_y -. taille_case/.2.7)&&(mob_y<=vehic_y +. taille_case/.2.7)&&(mob#get_dir <> 3))
||((mob_x <=vehic_x -. taille_case/.2.5)&&(mob_x >=vehic_x -. taille_case*.(3./.2.))&&(mob_y>=vehic_y +.taille_case/.2.9)&&(mob_y<=vehic_y +.taille_case*.(3./.2.1))&&(mob#get_dir=5));


|     _    ->                 false;;









let rec react_ia car list  mob oc_out= match !list with
  [] -> ()
|autre::l ->
    let dis = distance !car !autre in
    if (sphere !car !autre) then ignore (impact car autre mob oc_out);
    if dis <= distance_min then
      begin
	if ralentit !car !autre then 
            !car#set_push (0.,1.9);
	
	if ralentit !autre !car then
	    !autre#set_push (0.,1.9);
      end;
    react_ia car (ref l) mob oc_out;;



(* vieux test qui marche pas

let rec react_ia car list = match !list with
    [] -> ()
  |autre::l ->
      let dis = distance !car !autre in
      begin
	if (sphere !car !autre) then ignore (impact car autre);
	if dis <= 90. then
	  begin
	    if ((dis <= 60.)&&(fabs (arg (pivot (!autre#get_coord -- !car#get_coord) (-. (arg !car#get_vitesse)))) <= pi/.6.))||((fabs (arg (pivot (!autre#get_vitesse) (pi/.2. -. (arg !car#get_vitesse)))) <= pi/.4.)&&(fabs (arg (pivot (!autre#get_coord -- !car#get_coord) (0.05 -. (pi/.6.) -. (arg !car#get_vitesse)))) <= pi/.6.))
then 
	      begin
		(*      if dis > (25. +. (norm !car#get_vitesse)) then
		   (!car#set_push (0.,0.);
		   react_ia car (ref l);)
		   else *) !car#set_push (0.,2.);
	      end;
	    if ((dis <= 60.)&&(fabs (arg (pivot (!car#get_coord -- !autre#get_coord) (-. (arg !autre#get_vitesse)))) <= pi/.6.))||((fabs (arg (pivot (!car#get_vitesse) (pi/.2. -. (arg !autre#get_vitesse)))) <= pi/.4.)&&(fabs (arg (pivot (!car#get_coord -- !autre#get_coord) (0.05-. (pi/.6.) -. (arg !autre#get_vitesse)))) <= pi/.6.))

then
	      begin
		(*  if dis > (25. +. (norm !autre#get_vitesse)) then
		   (!autre#set_push (0.,0.);
		   react_ia car (ref l);)
		   else *) !autre#set_push (0.,2.);
	      end;
	  end;
	react_ia car (ref l)  mob oc_out;
      end;;

*)