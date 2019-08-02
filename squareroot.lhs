 # Program to calculate square root of a number by using Newtons method having formula Yn+1 = (Yn + (x / Yn)) / 2  where x is the number and initially Y0 = x 

> dec x = x - 1
> sroot' x yn  0  = yn
> sroot' x yn iter  = sroot' x  ((yn + (x / yn))/2)  (dec iter)  
> sroot x = sroot'  x   x   1000 
>