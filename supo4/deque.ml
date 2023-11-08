type 'a deque = 
| Q of 'a list * 'a list

let norm = function
| Q([], tls) -> Q (List.rev tls, [])
| Q(hds, []) -> Q ([], List.rev hds)
| q -> q

let qnull = function
| Q([],[]) -> true
| _ -> false

let enqt (Q (hds, tls)) x = norm (Q (hds, x::tls))

let enqh (Q (hds, tls)) x = norm (Q (x::hds, tls))

exception Empty

let deqt = function
| Q (x::hds, tls) -> norm (Q (hds, tls))
| _ -> raise Empty

let deqh = function
| Q (hds, x::tls) -> norm (Q(hds, tls))
| _ -> raise Empty

let qhd = function
| Q (x::_, _) -> x
| _ -> raise Empty

let qtl = function
| Q(_, x::_) -> x
| _ -> raise Empty