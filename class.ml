
#use"fonctions.ml";;


class immeuble (x_init,y_init) =
  object
      val coord = cplx2 (x_init*50+25) (500-y_init*50-25)
      val rayon = rayon_immeuble
      val envergure = pi/.4.
      val masse = max_float
      val corners =
	let x = float_of_int (x_init*50+25)
	and y = float_of_int (500-y_init*50-25)
	and r = rayon_immeuble
	and envergure = pi/.4. in
	((x+.r*.cos (-.envergure),y+.r*.sin (-.envergure)),(x+.r*.cos (envergure),y+.r*.sin (envergure)),(x+.r*.cos (-.envergure+.pi),y+.r*.sin (-.envergure+.pi)),(x+.r*.cos (envergure+.pi),y+.r*.sin (envergure+.pi)))
    val case = (x_init,y_init)
    method get_case = case

      method get_corners = corners
      method get_coord = coord
      method get_vitesse = zero
      method get_moteur = zero
      method get_frottements = zero
      method get_x = coord.re
      method get_y = coord.im
      method get_car_angle = pi/.15.
      method get_rayon = rayon
      method get_envergure = envergure
      method get_masse = masse
      method collide (c:Complex.t) = ()
      method chock () = ()
      method push ((force:float),(turn:float)) = true
end;;



class
voiture(coord_init,arg_init,(ia_init:Complex.t),(dir_init:int),(time_init:float),kind) =
  object

      val mutable pencher = 0.
      method get_pencher = pencher
      val mutable col2 = (2.,2.,2.)
      method get_col2 = col2
      method set_col2(c) = col2 <- c

      val mutable coord = coord_init          (* coordonnees en complexe *)
      val mutable vitesse = zero              (* vecteur vitesse en complexe *)
      val mutable moteur = polar 2. arg_init  (* acceleration due au moteur *)
      val mutable frottements = zero          (* frottements *)
      val mutable collisions = zero           (* collisions *)
      val mutable delay = time_init           (* temps de reference *)
      val rayon = if kind = 1 then rayon_voiture else rayon_mob              (* rayon du cercle conscrit au vehicule *)
      val envergure = if kind = 1 then envergure_voiture else envergure_mob       (* angle definissant les 4 coins du vehicule *)
      val mutable acceleration = zero 
      val masse = 10.;
      val mutable temp_turn = (0.,0.)
      val mutable s_duration = 0.
	  
      val name = (num_ia := (!num_ia mod 1000)+1;!num_ia) 
      method get_name = string_of_int name
      val mutable frequence = 11000.
      method get_frequence = string_of_int (int_of_float frequence)
      method set_frequence(f) = frequence <- f
      val l = if kind = 1 then rayon_voiture *. 2. *. envergure_voiture else rayon_mob *. 2. *. envergure_mob

      val mutable ia = ia_init
      method get_ia = ia
      method set_ia(d) = ia <- d
      val mutable ia_push = (1.,0.)
      method get_push = ia_push
      method set_push(a,b) = ia_push <- (a,b)
      method reset_push = ia_push <- (1.,0.)
      val mutable carr = 2
      method reset_carr = carr <- 2
      method set_carr(t) = carr <- t
      method get_carr = carr

      val mutable mem_case = (div (int_of_float coord_init.re) 50,(div (500 - (int_of_float coord_init.im)) 50))
      method get_case = (div (int_of_float coord.re) 50,(div (500 - (int_of_float coord.im)) 50))
      method get_mem_case = mem_case
      method memo(a) = mem_case <- a
     



      val mutable stock = 0
      method get_stock = stock
      method livre = stock <- stock-1
      method reload = stock <- 4





      val mutable dir = dir_init
      method get_dir = dir
      method set_dir(t) = dir <- t

      method get_corners =
	let x = coord.re
	and y = coord.im
	and r = rayon
	and angle = arg moteur
	and envergure = envergure_voiture in
	((x+.r*.cos (angle-.envergure),y+.r*.sin (angle-.envergure)),(x+.r*.cos (angle+.envergure),y+.r*.sin (angle+.envergure)),(x+.r*.cos (angle-.envergure+.pi),y+.r*.sin (angle-.envergure+.pi)),(x+.r*.cos (angle+.envergure+.pi),y+.r*.sin (angle+.envergure+.pi)))
      val corners =
	let x = coord_init.re
	and y = coord_init.im
        and r = if kind = 1 then rayon_voiture else rayon_mob
	and angle = arg_init
	and envergure = envergure_voiture in
	((x+.r*.cos (angle-.envergure),y+.r*.sin (angle-.envergure)),(x+.r*.cos (angle+.envergure),y+.r*.sin (angle+.envergure)),(x+.r*.cos (angle-.envergure+.pi),y+.r*.sin (angle-.envergure+.pi)),(x+.r*.cos (angle+.envergure+.pi),y+.r*.sin (angle+.envergure+.pi)))
      method get_coord = coord
      method get_vitesse = vitesse
      method get_moteur = moteur
      method get_frottements = frottements
      method get_x = coord.re
      method get_y = coord.im
    (*    method get_car_angle = arg moteur  *)
      method get_rayon = rayon
      method get_envergure = envergure
      method get_masse = masse
	  
      method collide(c) = collisions <- collisions ++ c
      method chock() = if collisions <> zero then (let duration = s_duration in
                                                   coord <- coord -- (duration ** vitesse);
						   vitesse <- vitesse ++ (collisions // masse);
						   coord <- coord ++ (duration ** vitesse);
						   collisions <- zero;)


      method push(force,turn,brake) = 
	let duration = Sys.time () -. delay in
	let turn = (norm vitesse) *. (sin turn) /. l in
	let turn = duration *. turn in

	(*------MAJ SON-----*)
	if force <> 0. then frequence <- fup duration frequence else frequence <- fdown duration frequence;

	delay <- Sys.time ();
	frottements <- (aero *. (norm vitesse)) ** (vitesse);
	acceleration <- (frottements ++ (force ** moteur) ++ (brake ** vitesse)) // masse ;
	vitesse <- vitesse ++ (duration ** acceleration);
	  (* on va decomposer le vecteur vitesse en un vecteur colineaire a la direction de la voiture et *)
	  (* un normal a la direction puis on le fait pivoter pour simuler l action des roues *)
	vitesse <- pivot vitesse (-. (arg moteur));
	vitesse <- (0.2 +. (0.8 *. (fabs (cos (arg vitesse))))) ** vitesse; (*frottements pneus*) 
	vitesse <- pivot ((polar vitesse.im (pi/.2.)) ++ (pivot (polar vitesse.re 0.) turn)) (arg moteur); 
	coord <- coord ++ (duration ** vitesse);
	temp_turn <- (arg moteur,turn);
	moteur <- pivot moteur turn; 

        pencher <- pencher +. (duration *. turn *. cdeg *. 50.);
	pencher <- if (abs_float turn) = 0. then let mi = (duration)*.350. in 
	             if (abs_float pencher)<=mi then 0. else
                       if pencher > 0. then pencher -. mi else pencher +. mi
	           else pencher;
	pencher <- if pencher > 45. then 45. 
	           else if pencher < (-.45.) then (-.45.) else pencher;

	true
end;;
