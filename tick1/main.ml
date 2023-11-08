let evalquad a b c x =
  a*.x*.x +. b *. x +. c;;

let rec facr n =
  match n with
  1 -> 1
  | _ -> facr (n-1) * n;;

let rec helper n total =
  if (n = 1) then
    total
  else
    helper (n-1) (n * total);;

let rec faci n =
  helper n 1;;

let rec sumhelp (x : float) (n : int) =
  if n = 1 then
    x
  else
    x +. sumhelp (x/.2.) (n-1)

let sumt n =
  sumhelp (1.) (n);;

let approx_eq a b = abs_float (a -. b) < 1e-10;;