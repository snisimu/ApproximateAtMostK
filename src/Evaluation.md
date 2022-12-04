# literals vs possibility

## (2,2)*

ghci> forM_ [1..5] $ \x -> reportApproxWith binomial (replicate x (2,2), 2) 2
(k=4,n=8)
approx(order): 64
counter: 133
approx/counter: 0.48120302
overall: 131/163(80.3%)
just: 38/70(54.2%)
(k=8,n=16)
approx(order): 168
counter: 585
approx/counter: 0.28717950
overall: 26755/39203(68.2%)
just: 4902/12870(38.0%)
(k=16,n=32)
approx(order): 376
counter: 2449
approx/counter: 0.15353206
overall(randam): 4454/10000(44.5%)
just(randam): 694/10000(6.9%)
(k=32,n=64)
approx(order): 792
counter: 10017
approx/counter: 0.07906559
overall(randam): 1380/10000(13.8%)
just(randam): 11/10000(0.1%)
(k=64,n=128)
approx(order): 1624
counter: 40513
approx/counter: 0.04008590
overall(randam): 123/10000(1.2%)
just(randam): 0/10000(0.0%)

## (3,3)

reportWith binomial (replicate x (3,3),3) 4

## (3,2)

ghci> reportApproxWith binomial (replicate 1 (3,2), 2) 3
(k=6,n=12)
order literals: 324
ghci> reportLiterals (6,12) 
binary: 390
counter: 319

## (2,3)

ghci> reportApproxWith binomial (replicate 1 (2,3), 3) 3
(k=9,n=18)
order literals: 327
ghci> reportLiterals (9,18)
binary: 1008
counter: 748

## ([(3,3)],3) 4

ghci> reportWith binomial ([(3,3)],3) 4
(k=12,n=27)
- approx(order): 3450
- counter: 1540
- approx/counter: 2.24025960
just(randam): 975/9997(9.7%)
overall(randam): 4118/9998(41.1%)
efficiency: 0.18385474
accuracy: 0.35714287
point(e*a): 0.06566241

## (5,2)

ghci> reportApproxWith binomial (replicate 1 (5,2), 2) 5
(k=10,n=20)
order literals: 7420
ghci> reportLiterals (10,20)
binary: 1230
counter: 931

> ghci> possibilityRate (replicate 1 (5,2), 2) 5
> overall: 524162/616666(84.9%)
> just: 92252/184756(49.9%)

# top vs bottom

### 2-2-3

> ghci> reportApproxWith counter [(2,2),(2,3)] 2
> (k=6,n=12)
> literals: 259
> ghci> possibilityRate [(2,2),(2,3)] 2
> 1808/2510(72.0%)
> ghci> reportApproxWith counter [(2,3),(2,2)] 3
> (k=6,n=12)
> literals: 220
> ghci> possibilityRate [(2,3),(2,2)] 3
> 1646/2510(65.5%)

> ghci> reportLiterals (6,12) 
> binary: 390
> counter: 319

### 2-3-3

> ghci> reportApproxWith counter [(2,2),(3,3)] 2
> (k=9,n=18)
> literals: 511
> ghci> possibilityRate [(2,2),(3,3)] 2
> 66048/155382(42.5%)
> ghci> reportApproxWith counter [(2,3),(2,3)] 3
> (k=9,n=18)
> literals: 418
> ghci> possibilityRate [(2,3),(2,3)] 3
> 79566/155382(51.2%)

> ghci> reportLiterals (9,18)
> binary: 1008
> counter: 748

# 

ghci> solutionSpaceRatio False False ((([(2,3)],2),3),(1,1))
0.64890283
