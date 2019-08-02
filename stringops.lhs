> inc x = x + 1
> lcount' [] l c = c
> lcount'(e:ls) l c |(e==l) = lcount' ls l (inc c) 
>                       | otherwise = lcount' ls l c
> lcount ls l = lcount' ls l 0 
> vowelc' []  c = c
> vowelc' (e:ls)  c |((e=='a')||(e=='e')||(e=='i')||(e=='o')||(e=='u')) = vowelc' ls  (inc c) 
>                         | otherwise = vowelc' ls  c
> vowelc ls  = vowelc' ls  0
> lpos' [] l c = error " Element not found " 
> lpos' (e:ls) l c |(e==l) = c
>                      | otherwise = lpos' ls l (inc c) 
> lpos ls l = lpos' ls l 1
> wcount' [] c = c
> wcount' (e:ls) c |(e==' ') = wcount' ls (inc c) 
>                        | otherwise = wcount' ls c 
> wcount ls  = wcount' ls 1
> wordins' [] ws ts = False
> wordins' (e:ls) ws ts|(e==' ')&&((reverse ts)==ws) = True 
>                      |(e==' ') = wordins' ls ws [] 
>                      |(ls==[])&&((reverse (e:ts))==ws) = True 
>                      | otherwise = wordins' ls ws (e:ts) 
> wordins ls ws = wordins' ls ws [] 
> wordoccs' [] ws ts c = c
> wordoccs' (e:ls) ws ts c |(e==' ')&&((reverse ts)==ws) =  wordoccs' ls ws [] (inc c) 
>                          |(e==' ') = wordoccs' ls ws [] c
>                          |(ls==[])&&((reverse (e:ts))==ws) = wordoccs' ls ws [] (inc c)   
>                          | otherwise = wordoccs' ls ws (e:ts) c
> wordoccs ls ws = wordoccs' ls ws [] 0 
> wpos' [] ws ts p = error " Word not found "
> wpos' (e:ls) ws ts p |(e==' ')&&((reverse ts)==ws) =  p
>                               |(e==' ') = wpos' ls ws [] (inc p) 
>                               |(ls==[])&&((reverse (e:ts))==ws) = p   
>                               | otherwise = wpos' ls ws (e:ts) p
> wpos ls ws = wpos' ls ws [] 1 
> wposns' [] ws ts p pl = reverse pl
> wposns' (e:ls) ws ts p pl  |(e==' ')&&((reverse ts)==ws) = wposns' ls ws [] (inc p) (p:pl) 
>                                       |(e==' ') = wposns' ls ws [] (inc p) pl 
>                                       |(ls==[])&&((reverse (e:ts))==ws) = wposns' ls ws (e:ts) (inc p) (p:pl)
>                                       | otherwise = wposns' ls ws (e:ts) p pl
> wposns ls ws = wposns' ls ws [] 1 []
