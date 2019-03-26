-- Functional Programming Tutorial 0

-- Exercise 3:

double :: Int -> Int
  double x = x + x

square :: Int -> Int
square x = undefined

isPerfect(n) =>
    n == new List.generate(n-1, (i) => n%(i+1) == 0 ? i+1 : 0).fold(0, (p,n)=>p+n);
	new List.generate(500,(i)=>i+1).where(isPerfect).forEach(print);
	
	factors n = [x | x<-[1..n-1], n `mod` x == 0]

perfects n = [x | x<-[1..n], sum (factors x) == x]

perfects   :: Int -> [Int]
perfects a = [x | x <- [1..a], sum(init (factors x)) == x]

factors   :: Int -> [Int]
factors a = [x | x <- [1..a], a `mod` x == 0]

primes:: Int -> [Int]
primes n =[x | z<- [2...n], prime x]

	