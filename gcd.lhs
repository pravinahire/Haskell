> gcd' a b | ((rem a b)==0) = b
>          | otherwise = gcd' b (rem a b)
> gcdof1 a b | (a>b) = gcd' a b
>            | otherwise = gcd' b a

-------------------------------------------------------------------------------------------------------------------------------------------

> gcdof2 a b | ((rem a b)==0)&&(a>b) = b
>            | ((rem b a)==0)&&(b>a) = a
>            | otherwise = gcdof2 b (rem a b)
>
