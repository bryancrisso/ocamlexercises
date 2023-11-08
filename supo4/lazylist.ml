type 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

let rec maps f = function
| Nil -> Nil
| Cons(x, xs) -> Cons(f x, fun () -> maps f (xs()))

type 'a tree = Lf | Br of 'a * (unit -> 'a tree) * (unit -> 'a tree)

let rec append xq yq =
  match xq with
  | Nil -> yq
  | Cons(x,xs) -> Cons(x, fun () -> append (xs()) yq)

let rec preord = function
| Lf -> Nil
| Br (v, t1, t2) -> Cons(v, fun () -> append (preord (t1 ())) (preord (t2())))

let rec change till amt chg chgs =
  match till, amt with
  | _ , 0 -> Cons(chg, fun () -> chgs)
  | [] , _ -> chgs
  | c::till , amt -> if amt < 0 then chgs
  else change (c::till) (amt - c) (c::chg)
  (change till amt chg chgs)

let rec get n s =
  match n, s with
  | 0, _ -> []
  | n, Nil -> []
  | n, Cons (x, xf) -> x :: get (n-1) (xf ())

let rec interleave xq yq =
  match xq with
  | Nil -> yq
  | Cons(x, xf) -> Cons(x, fun () -> interleave yq (xf ()))

let rec bits b = 
  Cons(b, fun () -> interleave (bits (0::b)) (bits (1::b)))

let rec pali = function
  | Nil -> Nil
  | Cons(x, xs) -> 
    let pal x = 
      x = List.rev x
    in
      if pal x then
        Cons(x, fun () -> pali (xs ()))
      else pali (xs ())

let rec filter p = function
| Nil -> Nil
| Cons(x, xs) -> 
  if p x then 
    Cons (x, fun () -> filter p (xs ()))
  else filter p (xs ())

let rec zipWithS f xq yq =
  match xq, yq with 
  | Nil, _ -> yq
  | _, Nil -> xq
  | Cons(x,xs), Cons(y,ys) ->  
    Cons (f x y, fun () -> zipWithS f (xs ()) (ys()))

let rec from k = Cons(k, fun () -> from (k+1))

let sieve = let rec sieve_aux = function
| Nil -> Nil
| Cons(x, xf) -> Cons(x, fun() -> sieve_aux (filter (fun i -> i mod x <> 0) (xf ())))
  in sieve_aux (from 2);;