let rec memb x = function
| [] -> false
| y::ys -> (y=x) || (memb x ys)

let rec subset p q = 
  match p, q with
  | [], [] -> true
  | [], _ -> true
  | _, [] -> false
  | x::xs, _ -> (memb x q) && (subset xs q)

let rec union p q =
  match p, q with
  | [], [] -> []
  | [], _ -> q
  | _, [] -> p
  | x::xs, y::ys -> 
    if x < y then 
      x::(union xs q)
    else if x > y then
      y::(union p ys)
    else x::(union xs ys)

let rec intersection p q =
  match p with
  | []->[]
  | x::xs ->
    if memb x q then
      x::(intersection xs q)
    else intersection xs q