let identity n =
  let row x = Array.init n (fun i -> if i = x then 1 else 0) in
  Array.init n row

let transpose a = 
  let row x = Array.init (Array.length a) (fun i -> Array.get (Array.get a i) x) in
  Array.init (Array.length (Array.get a 0)) row