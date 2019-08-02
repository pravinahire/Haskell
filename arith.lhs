> dec x = x-1
> inc  x = x+1
> add x 0 = x
> add x y = add (inc x) (dec y)
> sub x 0 = x
> sub x y = sub (dec x) (dec y) 
> mul' res x 1 = res
> mul' res x 0 = 0
> mul' res x y = mul' (add res x)  x  (dec y)
> mult x y = mul'  x  x  y
> div' res 0 y = res
> div' res x y | x<0 =res-1 
>	     | otherwise =  div' (inc res)  (sub x y)   y
> devide x y = div' 0  x  y
> oddeven 0 = "Even"
> oddeven 1 = "Odd"
> oddeven x = oddeven (sub x 2) 
> reciproc x  | (x==0) = 0
>                  | otherwise = (1 / x) 
> greater x y | x>y = x  
>	     | otherwise = y
> fact 0 = 1
> fact 1 = 1
> fact x = x * (fact (x-1))
> isprime' x 1 = True
> isprime' x  y  | ((rem x y)==0)  = False 
>                    | otherwise  = isprime' x (y-1)
> isprime x = isprime'  x  (x-1)
> addl e ls = e:ls
> remove (e:ls) = ls  
> rev' [] nl = nl
> rev' (e:ls) nl =  rev' ls (e:nl)
> rev ls = rev'  ls []
> iselem' x [] = False
> iselem' x (e:ls) | (x==e) = True
>                       | otherwise = iselem' x ls  
> iselem x ls = iselem' x ls
> power x  1  =  x
> power x  0  =  1
> power x  p  =  x*(power x (dec p)) 

 power x p   | (p==1) = x
                   | (p==0) = 1
                   | otherwise =  x*(power x (dec p)) 

> isperfect' x sumn 0  | (sumn == x) = True
>                              |  otherwise = False
> isperfect' x sumn facto | ((rem x facto ) == 0) = isperfect' x (sumn + facto) (dec facto)
>                                    | otherwise = isperfect' x sumn (dec facto)
> isperfect x = isperfect'  x  0  (x-1)
> isarm' x sumn 0  | (sumn == x) = True
>                              |  otherwise = False
> isarm' x sumn dn | (dn > 0) = isarm'     x     (sumn + (power (rem dn 10)  3) )    (div dn 10)
>                                    | otherwise = isarm' x sumn dn
> isarm x = isarm'  x  0  x
> fibo' x a b ls   | (x == 2) =(rev ls)
>                      | otherwise = fibo' (dec x)  b  (a+b) ((a+b):ls) 
> fibo  x = fibo' x 0 1 [1,0] 
> remlower ls = [ x | x <- ls , ( iselem x ['A' .. 'Z' ] ) || (x == ' ' ) ]
> remupper ls = [ x | x <- ls , ( iselem x ['a' .. 'z' ] ) || (x == ' ' ) ]