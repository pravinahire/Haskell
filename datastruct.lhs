>  insfirst e ls = e:ls
>  remfirst (e:ls) = ls
>  reverse' [] rs = rs
>  reverse' (e:ls) rs = reverse' ls (e:rs) 
>  rev ls = reverse' ls []
>  insend ls e =  rev (insfirst e  (rev ls) )
>  remlast' (e:ls) =(rev ls) 
>  remlast ls  = remlast' (rev ls)
>  inc x = x + 1
>  llength' [] l = l
>  llength' (e:ls) l =llength' ls (inc l)  
>  llength ls = llength' ls 0
>  dec x = x-1  
>  insert [] ls = ls  
>  insert (e:nl) ls = insert nl (e:ls)
>  inspos' pos ele (e:ls) nl  | (pos==2) = insert (e:nl) (ele:ls)
>                                    | (pos==1) = insfirst ele (e:ls)
>                                    | (pos>((llength ls ) + 1)) = error " Position Excided " 
>                                    | otherwise = inspos' (dec pos) ele ls (e:nl)
>  inspos pos ele ls = inspos' pos ele ls [] 
>  delpos' pos (e:ls)  nl      | (pos==1) = insert nl  ls
>                                      | ((pos)>(llength ls)) = remlast (e:ls)
>                                      | otherwise = delpos' (dec pos) ls (e:nl) 
>  delpos pos  ls  = delpos' pos ls [] 
