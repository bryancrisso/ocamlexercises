let rec t n = 
  match n with
  | 1 -> 1
  | _ -> 2 * t (n/2) + 1

let rec power x n s =
  if n = 1 then x *. s
  else if n mod 2 = 0 then
    power (x *. x) (n / 2) s
  else
    power (x *. x) (n / 2) (x *. s)

let rec sumr = function
  | [] -> 0
  | x::xs -> x + sumr xs

let rec sumi l s =
  match l with
  | [] -> s
  | x::xs -> sumi (xs) (x+s)

let lastr l = List.hd (List.rev l)

let rec lasti l p =
  match l with
  | [] -> p
  | x::xs -> lasti xs x

let rec evenl l = 
  match l with
  | [] -> []
  | _::[] -> []
  | x::y::t -> y::evenl t

let rec tails l t =
  match l with
  | [] -> []::t
  | x::xs -> l::tails (xs) (t)