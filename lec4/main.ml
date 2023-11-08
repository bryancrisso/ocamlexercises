let rec take = function
| ([], _) -> []
| (x::xs, i) ->
    if i > 0 then
      x :: take (xs, i - 1)
    else
      []

let rec drop = function
| ([], _) -> []
| (x::xs, i) ->
    if i > 0 then
      drop (xs, i - 1)
    else
      x::xs