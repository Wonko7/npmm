let pi        =  acos (-1.);;
let cdeg      =  180./.pi;;
#use"sia.ml";;



(**** 
vous etes mignons et tout avec vos complexes, mais ca m'aurai
arrangé que vous n'ayez pas redefini les fonctions sur les 
floats comme ** (puissance) ou sqrt... 

a part les modifs dans ce fichier j'ai rajouté des choses dans : 
gestion map.ml (type de la map)


TO DO : 
-affichage vehicules (if couleur = (2. 2. 2.) set rand)
-boussole
-barre de temps
-speedometer
-mini map
-derniere case


****)

(*******************************************************)
(*   MOTEUR GRAPHIQUE                                  *)
(*******************************************************)


(*********************************************)
(*   OPTIONS                                 *)
(*********************************************)

(*resolutions : 1280x960 1024x768 800x600 640x480*)
let iniw      =  1280;;
let inih      =   960;;
let bminimap  =  true;;
let bspeedo   =  true;;



(*********************************************)
(*   CONSTANTES                              *)
(*********************************************)


let alight    =  (0.0,-1.0,0.0);;
let clight    =  ref alight;;
let acam      =  (0.0,0.0,-1.0);;
let ccam      =  ref acam;;
let linesize  =  3.;;
let pi        =  acos (-1.);;
let cdeg      =  180./.pi;;
let a1        =  ref (0.0);;
let a2        =  ref 0.0;;
let axe1      =  ref (0.0);;
let bord      =  ref 2;;
let switch    =  ref 3;;
let scale2    =  0.01;;
let scale     =  2.5*.scale2;;
let scale3    =  (25.*.0.0017 *. scale2/. float_of_int (max (length mappy.(0) -1) (length mappy -1)));;
let broues    =  ref false;;
let chrono    =  ref 1.;;
let i         =  ref false;; 
let j         =  ref false;; 
let k         =  ref false;; 
let l         =  ref false;; 
let vmax      =  151.657508881;;
let bangle    =  ref 1.;; 
let vue       =  ref false;; 
let njj       =  length mappy -1;;
let nii       =  length mappy.(0) -1;;
let temps     =  Sys.time ();;
Random.self_init ();;


let col_route = (0.55,0.55,0.55);;




(*********************************************)
(*   FONCTIONS                               *)
(*********************************************)


let c2p (x,y) = Pervasives.sqrt(x*.x +. y*.y), 
  if x>0. then atan (y /. x) 
          else 
            let deno = (Pervasives.sqrt(x*.x +. y*.y)) in
	    if deno <> 0. then acos (y /. deno)+.pi/.2.
                          else acos (y /. (1./.99999000.))+.pi/.2.
;;

let p2c (r,o) = ((cos o)*.r,(sin o)*.r);;

let gnorme (x,y,z) = 
  Pervasives.sqrt (x*.x +. y*.y +. z*.z) ;;

let normalize (x,y,z) = 
  let n=gnorme (x,y,z) in
    (x/.n,y/.n,z/.n) ;;

let dotpr (a,b,c) (x,y,z) = 
  a*.x +. b*.y +. c*.z ;;

let rotvect (l) (a,b,c) ang = 
  let ang = ang/.cdeg in
          match l with 
   (1.0,0.0,0.0) -> let r,o = c2p (b,c)       in 
                    let b,c = p2c(r,o+.ang)   in 
                normalize     (a,b,c)
  |(0.0,1.0,0.0) -> let r,o = c2p (c,a)       in 
                    let c,a = p2c(r,o+.ang)   in 
                 normalize    (a,b,c)
  |            _ -> let r,o = c2p (a,b)       in 
                    let a,b = p2c(r,o+.ang)   in 
                  normalize   (a,b,c)
;;


let gnormal (a,b,c) (i,j,k) (x,y,z) = 
 let (a,b,c) = (a-.i,b-.j,c-.k) and 
     (x,y,z) = (x-.i,y-.j,z-.k) in
     normalize (b*.z -. c*.y, c*.x -. a*.z, a*.y -. b*.x);;


let p_vect (a,b,c) = 
begin
  print_float a;
  print_string "-";
  print_float b; 
  print_string "-";
  print_float c;
  print_string " 
"
end;;

let multrouple (a,b,c) d = 
  (a*.d,b*.d,c*.d);;

let addtrouple (a,b,c) d = 
  let coupe = function 
      u when u>=1. -> 1.
    | u when u<=0. -> 0.   
    | u            -> u 
  in

  let x = a+.d and
      y = b+.d and
      z = c+.d in
  (coupe x, coupe y, coupe z);;


(***************************cs*************************)
let cs_s () =
begin
	GlDraw.polygon_mode `both  `line;
	GlDraw.line_width linesize; 
        GlDraw.color (0.0,0.0,0.0);
(*      Gl.enable `blend;
        ligne mieux definie, mais avec blanc    *) 
        GlFunc.blend_func `src_alpha `one_minus_src_alpha;
(*
	GlDraw.polygon_mode `both  `line;
	GlDraw.line_width linesize;
        GlDraw.cull_face `;
	GlFunc.depth_func `lequal;
        GlDraw.color (0.0,0.0,0.0);  *)
end;;



let cs_e () =
begin
        Gl.disable `blend; 
(*  	GlFunc.depth_func `less;
	 GlDraw.cull_face `back;*)
	 GlDraw.polygon_mode `both `fill;

end;;




 (*********************************************)
 (*   TRAITER OBJETS                          *)
 (*********************************************)


 let randcol () = 
   let l = 0.5 in
 (l+.Random.float (1. -. l),l+.Random.float (1. -. l),l+.Random.float (1. -. l));;


(**
  *
  * borders : 
  * 0 : none
  * 1 : exterior
  * 2 : all edges
  *
  **)

let rec colorize a b c d color l border =
  let step = 0.1 and 
       ang = dotpr (gnormal a b c) l in
   
  if border=0 then
    begin
      GlDraw.begins `quads;
      (   match ang with
      an when an<=0.    -> GlDraw.color (addtrouple color (step*. -4.));
                           GlDraw.vertex3 a;
                           GlDraw.vertex3 b;
                           GlDraw.vertex3 c;
                           GlDraw.vertex3 d;

   |  an when an<=0.25   -> GlDraw.color (addtrouple color (step*. -3.));
                           GlDraw.vertex3 a;
                           GlDraw.vertex3 b;
                           GlDraw.vertex3 c;
                           GlDraw.vertex3 d;
   |  an when an<=0.5 -> GlDraw.color (addtrouple color (step*. -2.));
                           GlDraw.vertex3 a;
                           GlDraw.vertex3 b;
                           GlDraw.vertex3 c;
                           GlDraw.vertex3 d;
   |  an when an<=0.75 -> GlDraw.color (addtrouple color (step*. -1.));
                           GlDraw.vertex3 a;
                           GlDraw.vertex3 b;
                           GlDraw.vertex3 c;
                           GlDraw.vertex3 d;
   |  an               ->  GlDraw.color (addtrouple color (step*. 0.));
                           GlDraw.vertex3 a;
                           GlDraw.vertex3 b;
                           GlDraw.vertex3 c;
                           GlDraw.vertex3 d;
     );
    GlDraw.ends (); 
  end
  else if border=1 then   
    (   
       if dotpr !ccam (gnormal a b c) <= 0.00 then 
(*          colorize a b c d color l 2   *)
            begin
              colorize a b c d color l 0;
              	GlDraw.polygon_mode `both  `line;
        	GlDraw.line_width linesize;
                GlDraw.cull_face `front;
        	GlFunc.depth_func `lequal;
	(*        Gl.enable `blend;      
                GlFunc.blend_func `src_alpha `one_minus_src_alpha;*)
                GlDraw.color (0.0,0.0,0.0); 
              GlDraw.begins `quads;
              GlDraw.vertex3 a;
              GlDraw.vertex3 b;
              GlDraw.vertex3 c;
              GlDraw.vertex3 d;
              GlDraw.ends ();    
           	GlFunc.depth_func `less;
        	GlDraw.cull_face `back;
	        GlDraw.polygon_mode `both `fill;

            end 
       else colorize a b c d color l 0
     )
   else 
     begin
           colorize a b c d color l 0;
           cs_s ();
           GlDraw.begins `quads;
           GlDraw.vertex3 a;
           GlDraw.vertex3 b;
           GlDraw.vertex3 c;
           GlDraw.vertex3 d;
           GlDraw.ends ();    
           cs_e (); 
     end
;;


let dcube (a::b::c::d::e::f::g::h::[]) (c1::c2::c3::c4::c5::c6::[]) border =
begin
  if !broues then 
    begin 
  GlDraw.begins `quads;
  colorize a b c d c1 !clight 0;
  colorize b e h c c2 !clight border;
  colorize e f g h c3 !clight 0;
  colorize f a d g c4 !clight border;
  colorize f e b a c5 !clight border;
  colorize d c h g c6 !clight border;
  GlDraw.ends ()
    end 
  else
  begin
  GlDraw.begins `quads;
  colorize a b c d c1 !clight border;
  colorize b e h c c2 !clight border;
  colorize e f g h c3 !clight border;
  colorize f a d g c4 !clight border;
  colorize f e b a c5 !clight border;
  colorize d c h g c6 !clight border;
  GlDraw.ends ()
  end;
end;;
  

(*********************************************)
(*   MOBILETTE                               *)
(*********************************************)

let mlist1 = [
              (-0.375,3.28,1.70);(0.375,3.28,1.70);(0.375,2.625,1.5);(-0.375,2.625,1.5);(0.25,2.375,3.25);(-0.25,2.375,3.25);(-0.25,2.,3.25);(0.25,2.,3.25);
              (-0.25,2.,3.25);(0.25,2.,3.25);(0.25,1.625,3.25);(-0.25,1.625,3.25);(0.25,2.,5.5);(-0.25,2.,5.5);(-0.25,1.625,5.5);(0.25,1.625,5.5);
              (-0.25,2.25,5.75);(0.25,2.25,5.75);(0.25,2.,5.75);(-0.25,2.,5.75);(0.25,2.7,8.2);(-0.25,2.7,8.2);(-0.25,2.25,8.2);(0.25,2.25,8.2);
              (-0.325,3.2,1.1);(0.325,3.2,1.1);(0.325,2.7,1.1);(-0.325,2.7,1.1);  (0.375,3.28,1.4);(-0.375,3.28,1.4);(-0.375,2.625,1.2);(0.375,2.625,1.2)
             ];;

let mlist2 = [
              (-0.125,0.75,3.4);(0.125,0.75,3.4);(0.125,0.5,3.4);(-0.125,0.5,3.4);(0.125,0.5,4.9);(-0.125,0.5,4.9);(-0.125,0.25,4.9);(0.125,0.25,4.9);
              (-0.325,1.45,4.25);(0.325,1.45,4.25);(0.325,0.8,4.25);(-0.325,0.8,4.25);(0.325,1.3,5.1);(-0.325,1.3,5.1);(-0.325,0.75,4.9);(0.325,0.75,4.9);
              (-0.25,1.6,3.);(0.25,1.6,3.);(0.25,1.,2.75);(-0.25,1.,2.75);(0.25,1.375,4.1);(-0.25,1.375,4.1);(-0.25,0.8,3.75);(0.25,0.8,3.75);
              (-0.125,2.7,5.5);(0.125,2.7,5.5);(0.125,1.625,5.5);(-0.125,1.625,5.5);(0.125,2.7,5.75);(-0.125,2.7,5.75);(-0.125,1.625,5.75);(0.125,1.625,5.75);
              (0.25,3.5,1.46);(0.375,3.5,1.46);(0.375,1.,0.5);(0.25,1.,0.5);(0.375,3.5,1.76);(0.25,3.5,1.76);(0.25,1.,0.8);(0.375,1.,0.8);
              (-0.375,3.5,1.46);(-0.25,3.5,1.46);(-0.25,1.,0.5);(-0.375,1.,0.5);(-0.25,3.5,1.76);(-0.375,3.5,1.76);(-0.375,1.,0.8);(-0.25,1.,0.8)
             ];;

let mlist3 = [
              (0.25,3.625,1.46);(0.375,3.625,1.46);(0.375,3.5,1.46);(0.25,3.5,1.46);(1.,3.625,2.5);(0.875,3.625,2.5);(0.875,3.5,2.5);(1.,3.5,2.5);
              (-0.375,3.625,1.46);(-0.25,3.625,1.46);(-0.25,3.5,1.46);(-0.375,3.5,1.46);(-0.875,3.625,2.5);(-1.,3.625,2.5);(-1.,3.5,2.5);(-0.875,3.5,2.5);
              (-0.25,3.,5.);(0.25,3.,5.);(0.25,2.7,4.9);(-0.25,2.7,4.9);(0.5,3.1,6.);(-0.5,3.1,6.);(-0.5,2.7,6.25);(0.5,2.7,6.25)
             ];;
(**** ini ****)


let rec create_col_list l1 l2 = 
  match (l1,l2) with 
    ([],_) -> []
  |(0::l1,e::l2) -> create_col_list l1 l2 
  |(i::l1,e::l2) -> e::(create_col_list ((i-1)::l1) (e::l2));;

let mlist1col = 
  let cadre = (0.0,0.4,0.0) in
    create_col_list [14;1;3;1;5] [cadre;(0.8,0.0,0.0);cadre;(0.9,0.85,0.0);cadre];;

let mlist2col = 
  let chrome = (1.0,1.0,1.0) in
    create_col_list [36] [chrome];;

let mlist3col = 
  let tissu = (0.8,0.1,0.1) in
    create_col_list [18] [tissu];;


let rec scale_list l s = match l with
  []   -> []
 |e::l -> (multrouple e s)::scale_list l s;;

let mlist1 = scale_list mlist1 scale;;
let mlist2 = scale_list mlist2 scale;;
let mlist3 = scale_list mlist3 scale;;



(*******pour les roues*******)

let roues p r1 r2 e = 
  let rec boucle ang p r1 r2 e =
    if ang >= pi*.2. then (* [(e,0.,r2);(e*. -1.,0.,r2);(e*. -1.,0.,r1);(e,0.,r1)] *)(e*. -1.,0.,r2)::(e,0.,r2)::(e,0.,r1)::(e*. -1.,0.,r1)::[]
      else let (c,s) = (cos ang, sin ang) in
           let (e1,e2,e3,e4) = ((e*. -1.,s*.r2,c*.r2),(e,s*.r2,c*.r2),(e,s*.r1,c*.r1),(e*. -1.,s*.r1,c*.r1)) in
(*             e2::e1::e4::e3::e1::e2::e3::e4::(boucle (ang +.(pi*.2. /.p)) p r1 r2 e)*)
               e1::e2::e3::e4::e2::e1::e4::e3::(boucle (ang +.(pi*.2. /.p)) p r1 r2 e)
  in (*(e*. -1.,0.,r2)::(e,0.,r2)::(e,0.,r1)::(e*. -1.,0.,r1)::(boucle 0. p r1 r2 e);; *)
(e,0.,r2)::(e*. -1.,0.,r2)::(e*. -1.,0.,r1)::(e,0.,r1)::(boucle 0. p r1 r2 e);;


let rec create_rcol p chrome pneu = 
  if p=0 then [] 
         else  pneu::pneu::pneu::pneu::pneu::chrome::(create_rcol (p-1) chrome pneu);;

let rlist2 = (roues 142. (scale*.0.75*.7.) (scale*.7.) (scale*.0.25*.7.));;
let rlist1 = (roues 20. (scale*.0.75) (scale) (scale*.0.25)) ;;
let chrome = (1.0,1.0,1.0) and pneu = (0.8,0.5,0.5);;
let clist2 = create_col_list [(int_of_float 142.)*6+6] [chrome] and 
    clist1 = create_rcol ((int_of_float 20.)+1) chrome pneu;;

let droues () = 

  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord
 | _ -> ())
in 
  begin 
    plist rlist1 clist1 1;
  end;;



(**** tout ****)


let dmob () = 
  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord)
in 
  begin 
    plist mlist1 mlist1col !bord;
    plist mlist2 mlist2col !bord; 
    plist mlist3 mlist3col !bord;
    GlMat.translate3 (0. ,1.0*.scale,0.8*.scale);
      broues := true;  
      droues ();
      broues := false;
    GlMat.translate3 (0. ,0.,6.2*.scale);
      broues := true;  
      droues ();
      broues := false;
  end;;


(*********************************************)
(*   VOITURES                                *)
(*********************************************)

(*
let vlist1 = [
              (-3.,3.,11.5);(3.,3.,11.5);(3.,1.,11.5);(-3.,1.,11.5);(3.,3.,0.);(-3.,3.,0.);(-3.,1.,0.);(3.,1.,0.);
              (-2.5,5.,10.5);(2.5,5.,10.5);(3.,3.,11.5);(-3.,3.,11.5);(2.5,5.,4.25);(-2.5,5.,4.25);(-3.,3.,3.5);(3.,3.,3.5)
             ];;
*)
let vlist1 = [
              (3.,3.,11.5);(-3.,3.,11.5);(-3.,1.,11.5);(3.,1.,11.5);(-3.,3.,0.);(3.,3.,0.);(3.,1.,0.);(-3.,1.,0.);
              (2.5,5.,10.5);(-2.5,5.,10.5);(-3.,3.,11.5);(3.,3.,11.5);(-2.5,5.,4.25);(2.5,5.,4.25);(3.,3.,3.5);(-3.,3.,3.5)
             ];;

let vlist1 = scale_list vlist1 (scale*. 1.);;

let vcol1 =
  let cadre = (randcol ()) in
    create_col_list [12] [cadre];;



let vrlist = scale_list (roues (20.) (0.) (2. *.scale) (0.8 *.scale)) scale;;
let vrlist = (roues (5.) (0.) (1. *.scale) (0.4 *.scale));;
let vrclist = create_col_list [(5)*6+6] [pneu];;


let dvroues () = 
  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord
 | _ -> ())
  in 
  begin 
    plist vrlist vrclist 1;
  end;;

let dcar vcol1 = 
  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord)
  in 
    begin 
      plist vlist1 vcol1 !bord;
      GlMat.translate3 (2.75*.scale ,1.0*.scale,2.0*.scale); 
      broues := true; 
      dvroues ();
      GlMat.translate3 (0. ,0. , 8.0*.scale); 
      dvroues ();
      GlMat.translate3 (-5.5*.scale ,0. ,0.); 
      dvroues ();
      GlMat.translate3 (0. ,0. ,-8.0*.scale); 
      dvroues ();
      broues := false; 
      GlMat.translate3 (0. ,0. ,-1.*. -8.0*.scale); 
      GlMat.translate3 (-1.*. -5.5*.scale ,0. ,0.); 
      GlMat.translate3 (0. ,0. , -1.*.8.0*.scale); 
      GlMat.translate3 (-1.*.2.75*.scale ,-1.*.1.0*.scale,-1.*.2.0*.scale); 


    end;;



let rec draw_cars2 l = match l with 
     [] -> ()
  |e::l -> let ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = !e#get_corners in 
                      GlDraw.begins `quads;
                      GlDraw.color (0.26,0.85,0.24);
                      GlDraw.vertex3 (x1*.scale2,0.01,y1*.scale2*. -1.);
                      GlDraw.vertex3 (x2*.scale2,0.01,y2*.scale2*. -1.);
                      GlDraw.vertex3 (x3*.scale2,0.01,y3*.scale2*. -1.);
                      GlDraw.vertex3 (x4*.scale2,0.01,y4*.scale2*. -1.);
                      GlDraw.ends ();  
           draw_cars2 l;;


let draw_cars l = 
  let rec draw_l l = match l with 
     [] -> ()
  |e::l -> let ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = !e#get_corners   in 
           let (x,y) = ((x1+.x2)/.2.,(y1+.y2)/.2.)                  in 
	   let a = (arg !e#get_moteur)*.cdeg                       in  

	   let col = !e#get_col2                                    in
	   let col = if col = (2.,2.,2.) then 
             begin
	       !e#set_col2 (randcol ());!e#get_col2
             end else col                                           in 
	   let vcol1 = (* create_col_list [12] [col] *) col::col::col::col::col::col::col::col::col::col::col::col::[]                  in
                      GlMat.translate3 (x*.scale2,0.,-1.*.y*.scale2);
                      GlDraw.color col;
	              GlMat.rotate3 (a -. 90.) (0.0, 1.0, 0.0); 
		      if l = [] then 
		      begin
     	                GlMat.rotate3 (!e#get_pencher *. ((norm !e#get_vitesse)/.vmax)) (0.0, 0.0, 1.0); 
			dmob ();
		      end
		      else dcar vcol1;
	              GlMat.rotate3 (-1.*.a +. 90.) (0.0, 1.0, 0.0); 
                      GlMat.translate3 (-1.*.x*.scale2,0.,y*.scale2);
	 

           draw_l l in
        draw_l l
;;


(*********************************************)
(*   FLECHE                                  *)
(*********************************************)
let fltps = Sys.time ();;
let flist = [
             (-.1.,10.,-.0.25);(1.,10.,-.0.5);(0.25,2.,-.0.25);(-.0.25,2.,-.0.25);(1.,10.,0.25);(-.1.,10.,0.25);(-.0.25,2.,0.25);(0.25,2.,0.25);
             (-.1.,3.,-.0.255);(0.,2.,-.0.255);(0.,0.,-.0.255);(-.1.,2.5,-.0.255);(0.,2.,0.255);(-.1.,3.,0.255);(-.1.,2.5,0.255);(0.,0.,0.255);
             (0.,2.,-.0.255);(1.,3.,-.0.255);(1.,2.5,-.0.255);(0.,0.,-.0.255);(1.,3.,0.255);(0.,2.,0.255);(0.,0.,0.255);(1.,2.5,0.255)

            ];;
let flist = scale_list flist scale;;
let clist2 = create_col_list [18] [(0.66,1.25,0.64)];;
let clist = create_col_list [18] [(1.4,0.4,0.4)];;


 

let draw_fleche (c,r,clist) = 

  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord)
in 

  begin
    GlMat.rotate3 (-1.*.r) (0.0, 1.0, 0.0);  
    GlMat.translate3 (0.,c,0.);
    plist flist clist (!bord);
    GlMat.translate3 (0.,-.1.*.c,0.);
    GlMat.rotate3 (r) (0.0, 1.0, 0.0);  
  end;;

(*pizzerias *)
let fleche_drawlist () = 
  let rec b1 l = 
    match l with 
     []       -> ()
  |c::l ->  let dur = Sys.time () -. temps in let x = c.re and y = c.im in
                    GlMat.translate3 (x*.scale2,0.,-.1.*.y*.scale2);
                    draw_fleche (((cos (dur*.6.))+.1.)*.scale*.15.,dur*.155.,clist2);
                    GlMat.translate3 (-.1.*.x*.scale2,0.,y*.scale2);
                    b1 l;
                   in 
  let rec b2 l = 
    match l with 
     []       -> ()
  |c::l ->  let dur = Sys.time () -. temps in let x = c.re and y = c.im in
                    GlMat.translate3 (x*.scale2,0.,-.1.*.y*.scale2);
                    draw_fleche (((cos (dur*.6.))+.1.)*.scale*.15.,dur*.155.,clist);
                    GlMat.translate3 (-.1.*.x*.scale2,0.,y*.scale2);
                    b1 l;
                   in 

b1 pizzerias; (*b2 livraison*)
;;



(*********************************************)
(*   BATIMENTS                               *)
(*********************************************)

(* constantes *)

let b_h1 = 50.*.scale2   and 
     b_h = 200.*.scale2  and
     b_t = 25            and
     b_e = 50.*.scale2   and
     bt  = ref 0.
;;
let b_h1 = 10.*.scale2   and 
     b_h = 30.*.scale2  and
     b_t = 25            and
     b_e = 50.*.scale2   and
     bt  = ref 0.
;;




let min3 a b c = if a<=b && a<=c then a 
                 else if c<=b && c<=a then c else b;;

let create_bat () = 
  let a = b_h1 +. (Random.float b_h) and
      b = b_h1 +. (Random.float b_h) and
      c = b_h1 +. (Random.float b_h) in
  let moy = (a+.c)/.2.               in
  let d = (moy -. b) +. moy          in
  let (a,b,c,d) = if d < 0. then (a-.d,b-.d,c-.d,0.) 
                            else (a,b,c,d) in
  [(b_e,a,0.);(0.0,b,0.);(0.0,0.,0.);(b_e,0.,0.);(0.0,c,-1.*.b_e);(b_e,d,-1.*.b_e);(b_e,0.0,-1.*.b_e);(0.0,0.0,-1.*.b_e)];;

let blist = scale_list (create_bat ()) scale;;
let bclist = create_col_list [6] [randcol ()];;
    


let d1bat () = 
  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord)
  in 
    
      plist blist bclist !bord;;

let d2bat ((a5),b5) = 
  let rec plist l col bord= (match (l,col) with
     ([],_)                 -> ()
 |(a::b::c::d::e::f::g::h::l, o1::o2::o3::o4::o5::o6::l2) -> dcube (a::b::c::d::e::f::g::h::[]) (o1::o2::o3::o4::o5::o6::[]) (bord);plist l l2 bord)
  in 
    
      plist a5 b5 !bord;;



let draw_map ()=
  begin
    for j=0 to njj do
      for i=0 to nii do
        let z = (float_of_int (njj-j))*. -1.  *.taille_case*.scale2   and 
            y = (float_of_int (i))            *.taille_case*.scale2   in
        match mappy.(j).(i)#get_kind with
                0 ->  GlMat.translate3 (y,0.,z);
                      d2bat ((mappy.(j).(i)#get_bat),(mappy.(j).(i)#get_col));
                      GlMat.translate3 (y*. -1.,0.,z*. -1.)
               |_ ->  
		      GlDraw.begins `quads;
                      GlDraw.color col_route;
                      GlDraw.vertex3 (y,0.,z+.taille_case*.scale2-.taille_case*.scale2*.1.);
                      GlDraw.vertex3 (y+.taille_case*.scale2,0.,z+.taille_case*.scale2-.taille_case*.scale2*.1.);
                      GlDraw.vertex3 (y+.taille_case*.scale2,0.,z-.taille_case*.scale2*.1.);
                      GlDraw.vertex3 (y,0.,z-.taille_case*.scale2*.1.);
                      GlDraw.ends (); 
      done;
    done;
  end;;


(*********************************************)
(*   PIZZAS COMPTEUR                         *)
(*********************************************)

let draw_piz n = 
  let max = 4       and
      dist = -.0.0015  and
      x = 0.007     and
      y = 0.001     in

  let rec b i = 
    if i>max then GlMat.translate3 (0.,0.,-.1.*.(float_of_int (max-1))*.dist)       (*retour au debut*)
    else 
     begin
       GlMat.translate3 (0.,0.,dist);
      GlDraw.begins `quads;
       GlDraw.color (if i>n then (0.8,0.8,0.8) else (1.,1.,1.));
       GlDraw.vertex3 (0.0,0.0,0.0);
       GlDraw.vertex3 (x,0.0,0.0);
       GlDraw.vertex3 (x,0.0,-.1.*.y);
       GlDraw.vertex3 (0.0,0.0,-.1.*.y);
      GlDraw.ends ();
       b (i+1);
     end
  in
begin
      GlMat.translate3 (0.,0.,-1.*.dist);
      b 1;
end;;

(*********************************************)
(*   SPEEDOMETER                             *)
(*********************************************)

(** appellé dans minimob pour eviter de parcourir la liste **)

let draw_speedo v = 
  let p = pi/.2./.50. in
  let r = 0.013  in
  let v = ((v*.3.*.pi)/.(vmax*.7.))  in
  let rec b i = 
    if i<(pi/.2.) then
      begin
       let s = sin i and c = cos i in
       let s1 = sin (i+.p) and c1 = cos (i+.p) in
       GlDraw.begins `triangles;
       GlDraw.color (if v>=i then (1.,1.,1.) else (0.8,0.8,0.8));
(*     GlDraw.color (if v>=i && v<(i+.p) then (0.26,0.24,0.85) else (0.8,0.8,0.8));*)
       GlDraw.vertex3 (-.1.*.c*.r,0.0,-.1.*.s*.r);
       GlDraw.vertex3 (0.0,0.0,0.0);
       GlDraw.vertex3 (-.1.*.c1*.r,0.0,-.1.*.s1*.r);  

       GlDraw.ends ();
       b (i+.p);
      end
    else ();
  in 
     GlDraw.begins `triangles;
       b 0.;
     GlDraw.ends ();;



(*********************************************)
(*   MINIMAP                                 *)
(*********************************************)
let draw_minimap ()=
if bminimap then
  begin
    for j=0 to njj do
      for i=0 to nii do
        let z = (float_of_int (njj-j))*. -1.  *.taille_case*.scale3   and 
            y = (float_of_int (i))                         *.taille_case*.scale3   in
        match mappy.(j).(i)#get_kind with
                0 -> (* GlDraw.begins `quads;
                      GlDraw.color (0.,0.,0.);
                      GlDraw.vertex3 (y,0.,z-.taille_case*.scale3*.2.);
                      GlDraw.vertex3 (y+.taille_case*.scale3,0.,z-.taille_case*.scale3*.2.);
                      GlDraw.vertex3 (y+.taille_case*.scale3,0.,z-.taille_case*.scale3*.3.);
                      GlDraw.vertex3 (y,0.,z-.taille_case*.scale3*.3.);
                      GlDraw.ends (); *) ()
               |_ ->  GlDraw.begins `quads;
                      GlDraw.color (1.,1.,1.);
                      GlDraw.vertex3 (y,0.,z-.taille_case*.scale3*.2.);
                      GlDraw.vertex3 (y+.taille_case*.scale3,0.,z-.taille_case*.scale3*.2.);
                      GlDraw.vertex3 (y+.taille_case*.scale3,0.,z-.taille_case*.scale3*.3.);
                      GlDraw.vertex3 (y,0.,z-.taille_case*.scale3*.3.);
                      GlDraw.ends (); 
      done;
    done;
  end
else ()
;;


let rec draw_minimob l = match l with 
      [] -> ()
  |e::[] ->
            if bminimap then
              let ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = !e#get_corners in 
              let (x5,y5) = ((x1+.x2+.x3+.x4)/.4.,(y1+.y2+.y3+.y4)/.4.) in 
                        GlDraw.begins `quads;
                        GlDraw.color (0.26,0.24,0.85);
                        GlDraw.vertex3 ((x5-.50.)*.scale3,0.00,(y5)*.scale3*. -1.);
                        GlDraw.vertex3 ((x5)*.scale3,0.00,(y5-.50.)*.scale3*. -1.);
                        GlDraw.vertex3 ((x5+.50.)*.scale3,0.00,(y5)*.scale3*. -1.);
                        GlDraw.vertex3 ((x5)*.scale3,0.00,(y5+.50.)*.scale3*. -1.);
                        GlDraw.ends ();
             else ();
             GlMat.translate3 (0.103,0.,-.0.004);
             draw_speedo (norm !e#get_vitesse);
	     GlMat.translate3 (0.002,0.0,0.0);
	     draw_piz (!e#get_stock);
  |e::l  ->         draw_minimob l;;




(*********************************************)
(*   GLUT                                    *)
(*********************************************)



let init_gl width height =
    GlDraw.shade_model `smooth;
    GlClear.color (0.67, 0.82, 1.);
    GlClear.depth 1.0;
    GlClear.clear [`color; `depth];
    Gl.enable `depth_test; 
    GlMisc.hint `perspective_correction `nicest;
    GlMisc.hint `line_smooth `nicest;
    Gl.enable `line_smooth;
    Gl.enable `cull_face;
    Gl.disable `lighting;
    Glut.fullScreen ();
;;

let reshape_cb ~w ~h =
  let 
    ratio = (float_of_int w) /. (float_of_int h) 
  in
    GlDraw.viewport 0 0 w h;
    GlMat.mode `projection;
    GlMat.load_identity ();
    GluMat.perspective 45.0 ratio (0.1, 100.0);
    GlMat.mode `modelview;
    GlMat.load_identity ()
;;





(*---------------------------------------------tests----------------------------------------------------*)


(*********************************************)
(*   INIT                                    *)
(*********************************************)


let mob =  new voiture(cplx 600. 375.,0.,cplx 200. 375.,3,Sys.time ());;

let bu = ref mob;;

let liste_voitures =  ref [bu];;



let mob = bu and
    list_mob = !liste_voitures;;

let actif = ref true;; (* inutile je crois, mais j'ai la flemme de verifier, chuis occupé *)


let immeubles = ref [];;
let generatrices = ref [];; 
let liste = ref list_mob;;
let livraison = ref (generate 4);;
ronde (!mob#get_case) Init immeubles generatrices;


(*********************************************)
(*   INIT MAP                                *)
(*********************************************)


for j=0 to (length mappy -1) do
  for i=0 to (length mappy.(0) -1) do
    match mappy.(j).(i)#get_kind with
      0 -> mappy.(j).(i)#set_col (randcol ()::randcol ()::randcol ()::randcol ()::randcol ()::randcol ()::[]);
           mappy.(j).(i)#set_bat (create_bat ())
     |_ -> ()
  done;
done;;




(*********************************************)
(*   DESSINER                                *)
(*********************************************)

let rec camera l = match l with 
  e::[] -> let ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = !e#get_corners   in 
(*         let (x,y) = (((x1+.x2+.x3+.x4) /. 4.),((y1+.y2+.y3+.y4) /. 4.)) in*)
           let (x,y) = (((x1+.x2) /. 2.),((y1+.y2) /. 2.))                           in
	   let v = norm !e#get_vitesse                                               in
	   let a = (arg !e#get_moteur)*.cdeg -. !e#get_pencher*.0.2*.(v/.vmax)       in  
           GlMat.translate3 (0.0,0.0,-80.*.scale2 +. !axe1 +.  35.*.(v/.vmax)*.scale2);
	   GlMat.rotate3 (10. -. 5.*.(v/.vmax) +. !a1) (1.0,0.0,0.0);
	   GlMat.rotate3 (-1.*.a +. 90.) (0.0,1.0,0.0);
           GlMat.translate3 (-1.*.x*.scale2,-.10.*.scale2-. 0.*.(v/.vmax)*.scale2  ,y*.scale2);
(*print_float v;
print_string " 
";*)
 |e::l  -> camera l


let dessiner () = 
begin
  GlClear.clear [`color; `depth];
  GlMat.load_identity ();

  if !vue then 
    begin
      GlMat.translate3 (0.,0.,-1600.*.scale2 +. !axe1);
      GlMat.rotate3 (90.) (1.0, 0.0, 0.0); 
      GlMat.translate3 (((float_of_int (length mappy.(0) -1))*.taille_case*.scale2)/. -2. ,0.,
                        ((float_of_int (length mappy -1))*.taille_case*.scale2)/. 2.);      
    end
  else
    camera !liste;
  fleche_drawlist ();
  draw_map ();

  draw_cars2 !liste; 
  draw_cars  !liste;  
  fleche_drawlist ();
  GlMat.load_identity ();
  GlMat.translate3 (0.,0.,-0.11);
  GlMat.rotate3 (90.) (1.0, 0.0, 0.0); 
  GlMat.translate3 (-0.057,0.,0.0475);
  draw_minimap ();
  GlMat.translate3 (0.,0.0001,0.);
  draw_minimob list_mob;
  Glut.swapBuffers ();
end;;

(*********************************************)
(*                GESTION IA                 *)
(*********************************************)

let rec treat_ia mob carte  immeubles mob tmp_gen= function
    [e] -> [e]
  |ia::tmp -> if (norm (!ia#get_coord -- !ia#get_ia) < !ia#get_rayon) then
      (let (x,y) = !ia#get_case and dir = !ia#get_dir and carr = !ia#get_carr in
      let (x,y) = match dir with
	2 -> (x,y-1);
	  |3 -> (x+1,y);
	  |5 -> (x,y+1);
	  |7 -> (x-1,y);
          |_ -> failwith"pas bonne direction";
      in match !carte.(y).(x)#get_kind with
        2 -> turn ia (x,y) (2,2)
      |3 -> turn ia (x,y) (3,2)
      |5 -> turn ia (x,y) (5,2)
      |7 -> turn ia (x,y) (7,2)
      |n -> turn ia (x,y) (path dir (got_it n 2) carr (x,y)));
      react_ia ia (ref tmp) mob;

      crash_dummies ia immeubles mob;
      tmp_gen := vire_de_la_liste (!ia#get_case) (!tmp_gen);



      let (a,b) = !ia#get_push in
      !ia#push(a*.300.,if_under (arg (pivot (!ia#get_ia-- !ia#get_coord) (-. arg !ia#get_vitesse))) (pi/.6.),b *. (-.10.));
      !ia#reset_push;



      !ia#chock();
      if (distance !mob !ia) > 212. then   (*supression des vehicules trop éloignés*)
	treat_ia mob carte immeubles mob tmp_gen tmp
      else      
	ia::treat_ia mob carte immeubles mob tmp_gen tmp;;



(*********************************************)
(*   BOUCLE PRINCIPALE DANS GLUT             *)
(*********************************************)

let unit_of_bool b = ();;
let float_of_bool x = if x then 1. else 0.;;
let bprincipale () = 
   begin
   (*-----OPERATIONS SUR MOB--------*)
   
   let (mx,my) = (!mob#get_mem_case) and (ax,ay) = (!mob#get_case) in
   let px = ref mx and py = ref my in
   if (!px <> ax)||(!py <> ay) then
   begin
   if (!px < ax) then
   ( ronde (!px+1,!py) Right immeubles generatrices;
   px := !px+1; )
   else 
   if (!px > ax) then
   ( ronde (!px-1,!py) Left immeubles generatrices;
   px := !px-1; );
   if (!py < ay) then
   ( ronde (!px,!py+1) Down immeubles generatrices;
   py := !py+1; )
   else 
   if (!py > ay) then
   ( ronde (!px,!py-1) Up immeubles generatrices;
   py := !py-1; );	
   end;
   !mob#memo(!px,!py);
   
   if (pres_pizz !mob#get_coord pizzerias) then
   !mob#reload;
   
   if (!mob#get_stock <> 0) then
   (let (l,b) = (retire (!mob#get_coord) !livraison) in
   if b then
   (!mob#livre;
   livraison := (generate 1)@l;););
   
   
   
   let direction= dir_voit !mob in
   !mob#set_dir(direction);
   
   
   
   let virage = (float_of_bool !j) *. (-.pi/.1.5) +. (float_of_bool !l) *. (pi/.1.5) in
   ignore (!mob#push((float_of_bool !i) *. 4600.+.10.,virage,(float_of_bool !k) *. (-.10.)));



    crash_dummies mob immeubles mob;
    
    let tmp_gen = ref (!generatrices) in
    
    liste := treat_ia mob (ref mappy) immeubles mob tmp_gen !liste;
    
    new_cars liste !tmp_gen;

    !mob#chock();
    dessiner ();
end;;





let main () =
  let 
    width  = iniw and
    height = inih
  in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "NPMM");
    Glut.displayFunc bprincipale;

    Glut.specialFunc ~cb:(fun ~key ~x ~y -> match key with
        |       Glut.KEY_F1   -> vue := not (!vue)
        |       Glut.KEY_F3   -> Glut.reshapeWindow iniw inih; Glut.positionWindow 0 10
        |       Glut.KEY_F4   -> Glut.fullScreen ();
(*      |       Glut.KEY_F5   -> a2 := !a2 +. 10.;
	                           clight := (rotvect (0.,1.,0.) !clight (-10.));
        |	Glut.KEY_F6   -> a2 := !a2 -. 10.;
	                           clight := (rotvect (0.,1.,0.) !clight (10.));
        |	Glut.KEY_F7   -> a1 := !a1 +. 10.;
	                           clight := (rotvect (1.,0.,0.) !clight (-10.));
        |	Glut.KEY_F8   -> a1 := !a1 -. 10.;
	                           clight := (rotvect (1.,0.,0.) !clight (10.));
        |	Glut.KEY_F11  -> axe1 := !axe1 +. 2.5;
        |	Glut.KEY_F12  -> axe1 := !axe1 -. 2.5;

        |	Glut.KEY_F1     -> bord := (!bord+1) mod 3
        |	Glut.KEY_F2     -> switch := (!switch+1) mod 6
        |	Glut.KEY_F9     -> bt := !bt+.1.
        |	Glut.KEY_F10    -> bt := !bt-.1.
*)
        |       Glut.KEY_LEFT   -> l := true
        |	Glut.KEY_RIGHT  -> j := true
        |	Glut.KEY_UP     -> i := true
        |	Glut.KEY_DOWN   -> k := true

        |	_ -> ());
   Glut.specialUpFunc ~cb:(fun ~key ~x ~y -> match key with
        |       Glut.KEY_LEFT   -> l := false
        |	Glut.KEY_RIGHT  -> j := false
        |	Glut.KEY_UP     -> i := false
        |	Glut.KEY_DOWN   -> k := false
        |	_ -> ());

    Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> match key with
        |27(*esc*) -> exit 0
        | _        -> ());


    Glut.reshapeFunc reshape_cb;    
    Glut.idleFunc(Some bprincipale);
    init_gl width height; 
    Glut.mainLoop ();;


(*********************************************)
(*   C'EST PARTI...                          *)
(*********************************************)


let _ = main ();;


(*********************************************)
(*   COMMENTAIRES                            *)
(*********************************************)

(*
while !actif do
    clear_graph ();
    draw_map mappy;
    if key_pressed() then begin
      actif := (match read_key () with
       '4' -> !mob#push(0.05,pi/.30.,0.)
      |'6' -> !mob#push(0.05,0.-.pi/.30.,0.)
      |'8' -> !mob#push(15.,0.,0.)
      |'2' -> !mob#push(-.0.1,0.,0.)
      |'5' -> false
      |_ -> !mob#push(100000.,0.,0.)); end
    else
      begin actif := !mob#push(0.,0.,0.); end;
    
    let tmp = ref list_mob in
    while (match !tmp with [e] -> false | _ -> true) do
      begin      
	let ia::reste = !tmp in
	tmp := reste;
	if (norm (!ia#get_coord -- !ia#get_ia) < !ia#get_rayon) then
	  (let (x,y) = !ia#get_case and dir = !ia#get_dir and carr = !ia#get_carr in
	  let (x,y) = match dir with
	   2 -> (x,y-1);
	  |3 -> (x+1,y);
	  |5 -> (x,y+1);
	  |7 -> (x-1,y);
          |_ -> failwith"pas bonne direction";
	  in match mappy.(y).(x)#get_kind with
           2 -> turn ia (x,y) (2,2)
          |3 -> turn ia (x,y) (3,2)
	  |5 -> turn ia (x,y) (5,2)
	  |7 -> turn ia (x,y) (7,2)
	  |n -> turn ia (x,y) (path dir (got_it n 2) carr (x,y)));
	react_ia ia tmp;
	let (a,b) = !ia#get_push in
	!ia#push(a*.600.,if_under (arg (pivot (!ia#get_ia -- !ia#get_coord) (-. arg !ia#get_vitesse))) (pi/.200.),b *. (-.10.));
	!ia#reset_push;
	draw !ia;
	!ia#chock();
      end;
    done;
    
    !mob#chock();

    draw !mob;
    synchronize ()
done;;

auto_synchronize false;;
open_graph "";;               


let draw obj =
  let ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = obj#get_corners in
  begin
    set_color black;
    moveto (int_of_float x1) (int_of_float y1);
    lineto (int_of_float x2) (int_of_float y2);
    lineto (int_of_float x3) (int_of_float y3);
    lineto (int_of_float x4) (int_of_float y4);
    lineto (int_of_float x1) (int_of_float y1);
    draw_string (string_of_float (norm obj#get_vitesse));
end;;


let draw_map map=
  for j=0 to (length mappy -1) do
    for i=0 to (length mappy.(0) -1) do
      match map.(j).(i)#get_kind with
	0 -> (set_color (rgb 113 56 0);fill_rect (i*50) (550-j*50) 50 50;)
       |_ -> (set_color (rgb 200 200 200);fill_rect (i*50) (550-j*50) 50 50;)
    done;
  done;;



auto_synchronize false;;
draw_map mappy ;;





let rec dessine = function
     [] -> ();
  |  e::l -> draw !e; dessine l;;

*)


