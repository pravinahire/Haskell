> inc x = x + 1 
> dec x = x - 1
> pow x 0 = 1
> pow x 1 = x
> pow x y = pow (x*2) (dec y) 
> deci' 0 sum ind = sum
> deci' x sum ind = deci' (div x 10) (sum + ((rem x 10)*(pow 2 ind))) (inc ind)      
> deci x = deci' x 0 0
