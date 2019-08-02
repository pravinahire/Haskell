>  rev' [] nl  = nl
>  rev' (e:ls) nl = rev' ls (e:nl)
>  rev ls = rev' ls [] 
>  newlist  ls [] = (rev ls)               
>  newlist  ls  (e:l2) =  newlist (e:ls) l2
>  concate l1 l2 = newlist  (rev l1)  l2
>  iselem' x [] = False
>  iselem' x  (e:ls) | (x==e) = True
>                        | otherwise = iselem' x ls   
>  iselem x ls = iselem' x ls