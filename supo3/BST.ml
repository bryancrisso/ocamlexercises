type 'a tree =
 Lf
| Br of 'a * 'a tree * 'a tree

exception Collision

let rec update k v = function
| Lf -> Br((k,v), Lf, Lf)
| Br ((a,x), t1, t2) ->
  if a < k then
    Br((a,x), t1, update k v t2)
  else if k < a then
    Br((a,x), update k v t1, t2)
  else
    raise Collision

exception Missing

let rec deletes k = function
| Lf -> raise Missing
| Br((a,x), t1, t2) -> 
  if k < a then Br((a,x), deletes k t1, t2)
  else if k > a then Br((a,x), t1, deletes k t2)
  else 
    let rec append = function
    | Lf, t -> t
    | t, Lf -> t
    | Br((a,x), t1, t2), Br((k,v), s1, s2) -> 
      if k < a then Br((a,x), append (t1, (Br((k,v), s1, s2))), t2)
      else if k > a then Br((a,x), t1, append (t2, (Br((k,v), s1, s2))))
      else Br((k,v), s1, s2)
    in append(t1, t2)

let rec tcons v = function
  | Lf -> Br ((v,1), Lf, Lf)
  | Br ((w,_), t1, t2) -> Br ((v,1), tcons w t2, t1)

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
                    v::(combine (listofarray t1, listofarray t2))


let rec delete k = function
| Lf -> raise Missing
| Br((a,x), Lf, Lf) -> Lf
| Br((a,x), t1, t2) -> 
  if k < a then Br((a,x), delete k t1, t2)
  else if k > a then Br((a,x), t1, delete k t2)
  else 
    let rec least = function
    | Lf -> raise Missing
    | Br(k, Lf, Lf) -> k
    | Br(k, t1, t2) -> least t1
    in
    Br(least t2, t1, delete ((fun (a,b) -> a)(least t2)) t2)