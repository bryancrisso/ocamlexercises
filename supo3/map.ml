let rec map2 f = function
| [] -> []
| x::xs -> match x with 
            [] -> []
          | y::ys -> let rec map g = function
                      | [] -> []
                      | z::zs -> g z :: map g zs
                    in
                      (f y :: map f ys)::map2 f xs

let rec map g = function
  | [] -> []
  | z::zs -> g z :: map g zs


let rec mapo f = function
| None -> None
| Some x -> Some (f x)