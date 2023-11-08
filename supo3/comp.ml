let rec pairwise = function
| (x,y), (w,z) -> x < w && y < z

let rec lexicographic = function
| (x,y), (w,z) -> x < w || (x == w && y < z)

let rec pairwise2 compa compb a b =
  match a, b with
 (x,y), (w,z) -> (compa x w) && (compb y z)

let rec lexicographic2 compa compb a b = 
  match a, b with
 (x,y), (w,z) -> (compa x w) || (x == w && (compb y z))

let insort lessequal =
  let rec ins x = function
  | [] -> [x]
  | y::ys -> if lessequal x y then x :: y :: ys
  else y :: ins x ys
  in
  let rec sort = function
  | [] -> []
  | x::xs -> ins x (sort xs)
  in
  sort

let compare a b = pairwise2 (<=) (lexicographic2 (>) (<)) a b

let sort = insort (compare)

let v = sort [(1,(2,3))]