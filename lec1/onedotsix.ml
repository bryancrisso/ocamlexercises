let zero = (1.0 +. sqrt 5.0)/.2.0;;

let rec gratio n =
  if n = 0
    then zero
  else 
    1.0/.(gratio(n-1)-.1.0);;