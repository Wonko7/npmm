(*map test*)

open Array;;
class case (type_init:int) =
  object
    val kind = type_init
    val mutable col = [(0.,0.,0.)]
    val mutable bat = [(0.,0.,0.)]

    method get_kind = kind
    method get_col = col
    method get_bat = bat
    method set_col(t) = col <- t
    method set_bat(t) = bat <- t
end;;



let map=open_in "map.txt";;
let abscisse = 16;;
let ordonnee = 10;;
let fmdead = (input_line map);;
let fmdead = (input_line map);;

let mappy = let tmp = new case 0 in create_matrix ordonnee abscisse tmp;;



let rec store m map x y = try let n = int_of_char(input_char map) in if (n<>33) then let tmp = new case n in (m.(y).(x)<- tmp; store m map (x+1) y) else store m map 0 (y+1);
with End_of_file -> ();;

store mappy map 0 0;;
close_in map;;


