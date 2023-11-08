let rec fac n total =
  if (n = 1) then
    total
  else
    fac (n-1) (n * total);;

let rec eapprox n = 
  if n = 1 then
    1.
  else
    1./.(float_of_int( fac (n-1) 1)) +. eapprox (n-1)


let even n = n mod 2 = 0

let rec power x n =
  if n = 1 then x
  else if even n then
    power (x *. x) (n / 2)
  else
    x *. power (x *. x) (n / 2)

let rec exp z n =
  if n = 1 then
    1.
  else
    power(z) (n-1)/.(float_of_int( fac (n-1) 1)) +. exp z (n-1)

let rec gcd a b =
  if a = 0 then b
  else if b = 0 then a
  else if even a && even b then 2 * gcd (a/2) (b/2)
  else if even a && not (even b) then gcd (a/2) (b)
  else if not (even a) && even b then gcd (a) (b/2)
  else if not (even a) && not (even b) then gcd (abs (a - b)) (min a b)
  else 0
