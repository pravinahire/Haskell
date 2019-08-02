> stairs n = writeFile "stairs.sh" (stairs' n 0 0 0 "_" "." "|")

> stairs' n l h v hc c vc | (l == n) = lastline "_" (3+((l-1)*7))
>                         | (h<(3+(l*7))) && (v == 0)  =  "\nprintf \""++hc++"\"; \n sleep 0.3 " ++ stairs' n l (h+1) v hc c vc
>                         | (h==(3+(l*7))) && (v == 0)  =   "\nprintf \""++c++"\n\"; \n sleep 0.3 " ++ "\n" ++ stairs' n l 0 (v+1) hc c vc
>                         | (h<(3+(l*7))) && (v < 3 ) =  "\nprintf \" \"; \n sleep 0.3 "  ++ stairs' n l (h+1) v hc c vc
>                         | (h==(3+(l*7))) && (v < 3) =   "\nprintf \""++vc++"\n\"; \n sleep 0.3 "  ++ stairs' n l 0 (v+1) hc c vc
>                         | (v == 3) = stairs' n (l+1) 0 0 hc c vc
> lastline c n | (n /= 0) = "\nprintf \""++c++"\"; \n sleep 0.3 "  ++ lastline c (n-1)
>              | otherwise =  "\nprintf \"|\n\" "

