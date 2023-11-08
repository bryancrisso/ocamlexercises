let rec mul (x:float) (n:int) (sum:float) = 
  if n = 1 then sum
  else mul (x) (n-1) (sum+.x)