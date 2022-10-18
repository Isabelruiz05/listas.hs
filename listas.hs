--1. Ejercicio: firstToEnd
firstToEnd :: (Read a)=>[a]->[a]
firstToEnd []=[]
firstToEnd[x]=[x]
firstToEnd (x:xs) =  xs ++ [x]

--2. Ejercicio : minAndMax
minAndMax :: (Ord a)=>[a]->[a]
minAndMax []= error "Lista vacia"
minAndMax [x]= error "Falta 1 elemento"
minAndMax xs= [minimum xs, maximum xs]

--3. Ejercicio: minorsFirstElement
minorsFirstElement :: (Integral a)=>[a]->[a]
minorsFirstElement [] = error "Lista vacia"
minorsFirstElement [a] = error "Solo hay un elemento"
minorsFirstElement list = [x | x <- tail list , x<head list]

--4. Ejercicio: greaterOrEqualFirstElement
greaterOrEqualFirstElement :: Integral a => [a] -> [a]
greaterOrEqualFirstElement [] = error "Lista vacia"
greaterOrEqualFirstElement [a] = error "Solo hay un elemento"
greaterOrEqualFirstElement list = [x | x <- tail list , x >=head list]

--5. Ejercicio: minorsToSumFirstAndSecondElem
minorsToSumFirstAndSecondElement :: Integral a => [a] -> [a]
minorsToSumFirstAndSecondElement [] = error "Lista vacia"
minorsToSumFirstAndSecondElement [a] = error "Solo hay un elemento"
minorsToSumFirstAndSecondElement list= [x | x <- tail (tail list) , x <head list + head(tail list)]

--6. Ejercicio: listSumDuplaToList
listSumDuplaToList :: Integral a => [(a,a)] -> [a]
listSumDuplaToList [] = []
listSumDuplaToList list = fst(head list)+snd (head list) : listSumDuplaToList (tail list)

--7. Ejercicio: listMultTripletaToList
multipleta ::(Integral a)=> [(a,a,a)]->[a]
multipleta []=[]
multipleta xs = [x*y*z|(x,y,z)<- xs]

--8. Ejercicio: changeFstToSnd
changeFstToSnd :: (Integral a) => [(a,a)] -> [(a,a)]
changeFstToSnd [] = []
changeFstToSnd xs = (snd (head xs),fst (head xs)) : changeFstToSnd (tail xs)


--9. Ejercicio: sumVectors
sumVectors :: (Num a) => [(a,a)] -> (a,a)
sumVectors [x] = (fst x,snd x)
sumVectors list = (fst (head list) + fst (sumVectors (tail list)),snd (head list) + snd (sumVectors (tail list)))

--10. Ejercicio: dividers
divisores :: Int -> [Int]
divisores x = 
    [y | y <- [1..a], x `mod` y == 0]
    where a = x `div` 2

--11. Ejercicio: primeNumbers
esPrimo :: (Integral a)=>a -> Bool
esPrimo n = factores n == [1,n]
 where factores n = [x | x <- [1..n], n `mod` x == 0]
primeNumbers :: (Integral a)=>a->[a]
primeNumbers 0=[]
primeNumbers n = [x | x <- [1..n], esPrimo x]

--12. Ejercicio:infinitePrimeNumbers
infinitePrimeNumbers :: [Integer]
infinitePrimeNumbers = filterPrime [2..]
  where filterPrime (p:xs) =p : filterPrime [x | x <- xs, x `mod` p /= 0]

