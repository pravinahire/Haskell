> maxolist [] = error " NULL "
> maxolist [x] = x
> maxolist (e:f:ls) | (e > f) = maxolist (e:ls)
>                   | otherwise = maxolist (f:ls)
