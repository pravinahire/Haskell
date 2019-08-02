-------------------------------------------------------------------------------------------------------------------------------------------
   # to find remainder when second number didides the first

> remainder x y | (x>y) = remainder (x-y) y
>               | (x==y) = 0
>               | otherwise = x

-------------------------------------------------------------------------------------------------------------------------------------------
   # to reverse given list

> revlist' [] rl = rl
> revlist' (e:ls) rl = revlist' ls (e:rl)
> revlist ls = revlist' ls []

------------------------------------------------------------------------------------------------------------------------------------------

> inc x = x+1
> dec x = x-1

-------------------------------------------------------------------------------------------------------------------------------------------
  # to find length of a list

> listlen [] = 0
> listlen (e:ls) = 1 + listlen ls

-------------------------------------------------------------------------------------------------------------------------------------------
  # to find sum of all elements of a list

> sumlist []  = 0
> sumlist (e:ls) = e + (sumlist ls)

-------------------------------------------------------------------------------------------------------------------------------------------
  # Equality of given two lists

> listcomp [] [] = True
> listcomp ls [] = False
> listcomp [] rs = False
> listcomp (e:ls) (f:rs) | (e==f) = listcomp ls rs
>                        | otherwise = False

-------------------------------------------------------------------------------------------------------------------------------------------
  # comparision of lists ( in terms of  length )

> islarg ls rs | (listlen ls)>(listlen rs) = GT
>              | (listlen ls)<(listlen rs) = LT
>              | otherwise = EQ

-------------------------------------------------------------------------------------------------------------------------------------------

> maxele' [] max = max
> maxele' (e:ls) max | (e>max) = maxele' ls e
>                    | otherwise = maxele' ls max
> maxele (e:ls) = maxele' ls e


> isfactor f s | ((remainder s f)==0) = True
>              | otherwise = False


> divisors' x d ls | (x==d) = (revlist (d:ls))
>                  | ((remainder x d) == 0) = divisors' x (inc d) (d:ls)
>                  | otherwise = divisors' x (inc d) ls
> divisors x = divisors' x 1 []


> isprime' 1 f = False
> isprime' n 1 = True
> isprime' n f | (isfactor f n) = False
>              | otherwise = isprime' n (dec f)
> isprime n = isprime' n (n-1)


> primes 0 ls = ls
> primes n ls | (isprime n) = primes (n-1) (n:ls)
>             | otherwise = primes (n-1) ls
> primesupto n = primes n []


> isele x [] = False
> isele x (e:ls) | (x==e) = True
>                | otherwise = isele x ls


> selectels' x [] sl = sl
> selectels' x (e:ls) sl | (x==e) = selectels' x ls  (e:sl)
>                        | otherwise = selectels' x ls sl
> selectels x ls = selectels' x ls []


> evens' [] el = (revlist el)
> evens' (e:ls) el | ((remainder e 2)==0) = evens' ls (e:el)
>                  | otherwise = evens' ls el
> evens ls = evens' ls []


> doublels' [] dl = (revlist dl)
> doublels' (e:ls) dl = doublels' ls ((2*e):dl)
> doublels ls = doublels' ls []


> squrels' [] dl = (revlist dl)
> squrels' (e:ls) dl = squrels' ls ((e*e):dl)
> squrels ls = squrels' ls []


> cube ls = [ x*x*x | x <- ls ]


> squre ls = [ x*x | x <- ls ]


> double ls = [ 2*x | x <- ls ]


> touples fl sl = [ (x,y) | x <- fl , y <- sl ]


> lists tl = [ [ x | (x,y) <- tl ] , [ y | (x,y) <- tl ] ]


> digitlist' n dl | ((div n 10)==0) = ((remainder n 10):dl)
>                 | otherwise = digitlist' (div n 10) ((remainder n 10):dl)
> digitlist n = digitlist' n []


> issublist' sl [] cl [] = True
> issublist' sl [] cl pl = False
> issublist' sl ls cl [] = True
> issublist' sl (e:ls) cl (f:pl) | (e==f) = issublist' sl ls (e:cl) pl
>                                | (sl == (revlist cl)) = True
>                                | otherwise = issublist' sl ls [] sl
> issublist sl ls = issublist' sl ls [] sl


> delaltele' [] nl c = (revlist nl)
> delaltele' (e:ls) nl c | (c==1) = delaltele' ls (e:nl) 0
>                        | (c==0) = delaltele' ls nl 1
> delaltele ls = delaltele' ls [] 1


> duplicate x ls = [ x | _ <- ls ]


> insele' x [] nl = (revlist nl)
> insele' x (e:ls) nl = insele' x ls (x:e:nl)
> insele x ls = insele' x ls []


> revwords ls = (revlist ls)


> duplwords' dw [] ds nl = (revlist nl)
> duplwords' (e:dw) (f:ls) ds nl | (f == ' ') = duplwords' ds ls ds (' ':nl)
>                                | (dw == []) = duplwords' (' ':dw) ls ds (e:nl)
>                                | otherwise = duplwords' dw ls ds (e:nl)
> duplwords dw ls = duplwords' dw ls dw []


> reverseno' n sum  | ((div n 10) == 0) = ((sum * 10) + (remainder n 10))
>                   | otherwise = reverseno' (div n 10) ((sum * 10) + (remainder n 10))
> reverseno n = reverseno' n 0


> maxofl' [] max = max
> maxofl' (e:ls) max | (e>=max) = maxofl' ls e
>                    | otherwise = maxofl' ls max
> maxofl (e:ls) = maxofl' ls e


> lessthan' x [] nl = (revlist nl)
> lessthan' x (e:ls) nl | (e<x) = lessthan' x ls (e:nl)
>                       | otherwise = lessthan' x ls nl
> lessthan x ls = lessthan' x ls []


> greaterthan' x [] nl = (revlist nl)
> greaterthan' x (e:ls) nl | (e>x) = greaterthan' x ls (e:nl)
>                       | otherwise = greaterthan' x ls nl
> greaterthan x ls = greaterthan' x ls []


> greateql' x [] nl = (revlist nl)
> greateql' x (e:ls) nl | (e>=x) = greateql' x ls (e:nl)
>                       | otherwise = greateql' x ls nl
> greateql x ls = greateql' x ls []


> lesseql' x [] nl = (revlist nl)
> lesseql' x (e:ls) nl | (e<=x) = lesseql' x ls (e:nl)
>                     | otherwise = lesseql' x ls nl
> lesseql x ls = lesseql' x ls []


> sortlist [] = []
> sortlist (e:ls) = (sortlist (lessthan e ls)) ++ [e] ++ (sortlist (greateql e ls)) 


> containall' [] sl tl = True
> containall' (e:fl) (f:sl) tl | (e==f) = containall' fl tl tl
>                              | (sl==[]) = False
>                              | otherwise = containall' (e:fl) sl tl
> containall fl sl = containall' fl sl sl


> occurs' x [] c = c
> occurs' x (e:ls) c | (e==x) = occurs' x ls (inc c)
>                    | otherwise = occurs' x ls c
> occurs x ls = occurs' x ls 0


> tuples [] [] nl = (revlist nl)
> tuples (x:tl) (y:osl) nl = tuples tl osl ((x,y):nl)


> occureach' [] tl osl = tuples tl (revlist osl) []
> occureach' (e:ls) tl osl = occureach' ls tl ((occurs e tl):osl)
> occureach ls = occureach' ls ls []


> repeatl' 0 ls nl = nl
> repeatl' x ls nl = repeatl' (dec x) ls (ls ++ nl)
> repeatl x ls = repeatl' x ls []


> generate' x (e:ls) nl  gl | (x==0) = (reverse gl)
>                           | (ls==[]) = generate' (dec x)  nl nl (e:gl)
>                           | otherwise = generate' (dec x) ls nl (e:gl)
> generate x ls  = generate' x ls ls []
>

-------------------------------------------------------------------------------------------------------------------------------------------    # function for printing staircase of given number of steps using user defined functions


> staircase1 x =(repeatl x ((generate 50 "*") ++ (repeatl 4 ((generate 93 " ") ++  "*" ++ (generate 48 " ") ++ "*")) ++ (generate 93 " ") ++ (generate 10 "*")) ++ (generate 40 "*"))

-------------------------------------------------------------------------------------------------------------------------------------------
    # function for printing staircase of given number of steps using list comprehension


> staircase2 x =(repeatl x (['*' | _<-[1..50] ] ++ (repeatl 4 ([' ' | _<-[1..93] ] ++  ['*'] ++ [ ' ' | _<-[1..48] ] ++ ['*'])) ++ [ ' ' | _<-[1..93] ] ++ [ '*' | _<-[1..10] ])) ++ [ '*' | _<-[1..40] ]

-------------------------------------------------------------------------------------------------------------------------------------------    # function fot printing staircase of specified width , hight and number of steps 

> staircase3 w h x =(repeatl x ((generate w "*") ++ (repeatl h (print ("\n") ++  "*" ++ (generate (w-2) " ") ++ "*")) ++ print ("\n") ++ (generate 10 "*")) ++ (generate (w-10) "*"))

-------------------------------------------------------------------------------------------------------------------------------------------

> eleofpos' x (e:ls) pos | (x==pos) = e
>                        | otherwise = eleofpos' x ls (inc pos)
> eleofpos x ls = eleofpos' x ls 1


> posofele' x [] pos = error "Not found"
> posofele' x (e:ls) pos | (x==e) = pos
>                        | otherwise = posofele' x ls (inc pos)
> posofele x ls = posofele' x ls 1

-------------------------------------------------------------------------------------------------------------------------------------------    # function for printing box of string of given width and hight

> stringbox (e:f:ls) w h = ( (generate w (e:f:ls) ) ++ (repeatl (h-2) ((generate (143-w) " ") ++  [f] ++ (generate (w-2) " ") ++ [f])) ++ (generate (143-w) " ") ++ (generate w (e:f:ls) ))
>

-------------------------------------------------------------------------------------------------------------------------------------------

> box' ls w oh (f:e:tl) h rs (g:ps) | (oh==0)||(w==0) = error " Invalid width or hight "
>                                   | (oh==1) = generate w rs
>                                   | (h==oh) = ((generate w ls) ++ (box' ((generate (143-w) " ") ++ [e] ++ (generate (w-2) " ") ++ [e]) w oh (e:tl) (dec h) rs (g:ps)))
>                                   | (oh==0)||(w==0) = error " Invalid hight "
>                                   | (oh==1) = generate w rs
>                                   | (h==1) = ((generate (143-w) " ") ++ (generate w rs))
>                                   | ((h/=1)&&(tl==[])) = (ls ++ (box' ((generate (143-w) " ") ++  [e] ++ (generate (w-2) " ") ++ [e]) w oh ('p':rs) (dec h) rs (g:ps)))
>                                   | otherwise = (ls ++ (box' ((generate (143-w) " ") ++  [e] ++ (generate (w-2) " ") ++ [e]) w oh (e:tl) (dec h) rs (g:ps)))
>
> boxstring ls w h = box' ls w h ls h ls ls

-------------------------------------------------------------------------------------------------------------------------------------------

>
>
>
>
>
>
>
>

