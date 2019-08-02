> bineql' 0 ls = ls
> bineql' x ls = bineql' ( div x 2) ((rem x 2 ):ls)
> bineql  x = bineql' x []   