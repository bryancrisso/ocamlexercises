type 'a tree =
  | Lf
  | Br of 'a * 'a tree * 'a tree;;

let rec tcons v = function
  | Lf -> Br (v, Lf, Lf)
  | Br (w, t1, t2) -> Br (v, tcons w t2, t1);;

let rec arrayoflist l = 
match l with
[]-> Lf
| x::xs -> tcons x (arrayoflist xs);;

let rec listofarray = function
| Lf -> []
| Br (v, t1, t2) -> let rec combine = function
                    | [], [] -> []
                    | x::xs, y::ys -> x::y::(combine (xs, ys))
                    | x, [] -> x
                    |[], y -> y
                    in
                    v::(combine (listofarray t1, listofarray t2));;

let removehead = function
| Lf -> Lf
| t -> arrayoflist (List.tl (listofarray t))