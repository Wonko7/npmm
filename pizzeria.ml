  
(*----------------CREATION DE PIZZERIA-----------------------*)
  
  
let coordonize (x,y) = function
    2 -> (cplx2 (cote * x + (div cote 2)) (500 - (y * cote)))
  |3 -> (cplx2 ((x + 1) * 50) (500 - (y * 50) - (div 50 2)))
  |5 -> (cplx2 (50 * x + (div 50 2)) (500 - ((y+1) * 50)))
  |7 -> (cplx2 (x * 50) (500 - (y * 50) - (div 50 2)));;

let rec listify  = function
    [] -> []
  |(a,b,c)::l -> if a = -1 then listify l else (coordonize (a,b) c)::(listify l);;

let case (x,y) = if (x >= abscisse) or (x < 0) or (y < 0) or (y >= ordonnee) then -1
else mappy.(y).(x)#get_kind;;

let cote_libre (x,y) =
  if (case (x,y) = 0) then
    if (case(x-1,y) > 1) then 7
    else if (case(x+1,y) > 1) then 3
    else if (case(x,y-1) > 1) then 2
    else if (case(x,y+1) > 1) then 5
    else 0
  else 0;;

let rec find_free (min_x,min_y) (max_x,max_y) = function
    100 -> (-1,-1,-1)
  |i -> 
      let a = Random.int (max_x-min_x) + min_x
      and b = Random.int (max_y-min_y) + min_y in
      let c = cote_libre (a,b) in
      if (c <> 0) then (a,b,c)
      else find_free (min_x,min_y) (max_x,max_y) (i+1);;


let piz_count = div (abscisse*ordonnee) 200;;

let piz_create piz_count =
  if piz_count = 2 then
    if abscisse > ordonnee then
      let mid = div abscisse 2 in
      let (a,b,c) = find_free (0,0) (mid,ordonnee) 1
      and (d,e,f) = find_free (mid,0) (abscisse,ordonnee) 1 in
      
      (* creation liste coord *)
      listify [(a,b,c);(d,e,f)];
      
    else
      let mid = div ordonnee 2 in
      let (a,b,c) = find_free (0,0) (abscisse,mid) 1
      and (d,e,f) = find_free (0,mid) (abscisse,ordonnee) 1 in
      
      (* creation liste coord *)
      listify [(a,b,c);(d,e,f)];
      
      
  else if piz_count = 3 then
    if abscisse > ordonnee then
      let mid = div abscisse 3 in
      let (a,b,c) = find_free (0,0) (mid,ordonnee) 1 
      and (d,e,f) = find_free (mid,0) (2*mid,ordonnee) 1 
      and (g,h,i) = find_free (2*mid,0) (abscisse,ordonnee) 1 in
      
      (* creation liste coord *)
      listify [(a,b,c);(d,e,f);(g,h,i)];
      
    else
      let mid = div ordonnee 3 in
      let (a,b,c) = find_free (0,0) (abscisse,mid) 1 
      and (d,e,f) = find_free (0,mid) (abscisse,2*mid) 1
      and (g,h,i) = find_free (0,2*mid) (abscisse,ordonnee) 1 in
      
      (* creation liste coord *)
      listify [(a,b,c);(d,e,f);(g,h,i)];
      
  else if piz_count = 4 then
    let midx = div abscisse 2 
    and midy = div ordonnee 2 in
    let (a,b,c) = find_free (0,0) (midx,midy) 1 
    and (d,e,f) = find_free (midx,0) (abscisse,midy) 1 
    and (g,h,i) = find_free (0,midy) (midx,ordonnee) 1
    and (j,k,l) = find_free (midx,midy) (abscisse,ordonnee) 1 in
    
    (* creation liste coord *)
    listify [(a,b,c);(d,e,f);(g,h,i);(j,k,l)];
    
  else
    let (a,b,c) =  find_free (0,0) (abscisse,ordonnee) 1 in
    
    (* creation liste coord *)
    listify [(a,b,c)];
    
;;

let pizzerias = piz_create piz_count;;

let count_pizzerias = 
  let rec count = function
      [] -> 0
    |e::l -> 1 + count l;
  in
  count pizzerias;;

(*--------------LIVRAISON-------------*)

let rec is_assez_loin p dist = function
    [] -> true
  |e::l -> (norm (p -- e) >= dist)&&(is_assez_loin p dist l);;

let rec est_dedans p = function
    [] -> false
  |e::l -> (e=p)||(est_dedans p l);;


let generate n =
  let liste = ref [] in
  for i=1 to n do
    let p::[] = listify [(find_free (0,0) (abscisse,ordonnee) 1)] in
    let p = ref p in    
    while not (is_assez_loin !p 50. pizzerias) && (not (est_dedans !p !liste)) do 
      let m::[] = listify [(find_free (0,0) (abscisse,ordonnee) 1)] in
      p := m;
    done;
    liste := !p::!liste;
  done;
  !liste;;

(*~~~~~~~~~~~~ CREATION DE POINTS ~~~~~~~~~~~*)

let rec plus_proche cm cp = function
    [] -> cp
  |e::l -> 
      if (norm (cm--cp)) > (norm (e--cp)) then plus_proche cm e l
      else plus_proche cm cp l;;

let c_plus_proche cm l = let e::l = l in plus_proche cm e l;; 

let rec draw_liv color = function
    [] -> ()
  |e::l -> (set_color color;
	    fill_circle (int_of_float e.re) (int_of_float e.im) 5; draw_liv color l);;

let rec retire cm = function
    [] -> ([],false)
  |e::l -> if (norm (e--cm) < 30.) then (l,true) else let (l,b) = (retire cm l) in (e::l,b);;

let rec pres_pizz cm = function
    [] -> false
  |e::l -> (norm (e--cm) < 30.)||(pres_pizz cm l);;
