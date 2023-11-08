let rec map g = function
  | [] -> []
  | z::zs -> g z :: map g zs

let rec change till amt = match till, amt with
  | _, 0 -> [ [] ]
  | [], _ -> []
  | c::till, amt ->
    if amt < c then change till amt else
      let cons c a = c::a
      in map (cons c) (change (c::till) (amt-c))
        @ change till amt