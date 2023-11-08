let rec power x n =
  if n = 1 then x
  else if n mod 2 = 0 then
    power (x *. x) (n / 2)
  else
    x *. power (x *. x) (n / 2)

let poweri x n = 
  let xp = ref x in (*The number being powered*)
  let np = ref n in (*Exponent*)
  let fin = ref false in
  while not !fin do
    match !np with
    | 0 -> xp := 1.; fin := true;
    | 1 -> fin := true
    | n -> 
      if !np mod 2 = 0 then 
        (xp := (!xp)*.(!xp);
        np := !np/2)
      else 
        (xp := (!xp)*.(!xp)*.(!xp);
        np := !np/2)
  done;
  !xp

let swap xr yr = 
  let w, z = !yr, !xr in
  xr := w;
  yr := z

(*list of references to one reference to a list*)
let rec l2r = function
| [] -> []
| r::rs -> (!r)::(l2r rs)

(*one reference to a list to list of references*)
let rec r2l r = 
  match !r with
  | []-> []
  | x::xs -> (ref x)::(r2l (ref xs))