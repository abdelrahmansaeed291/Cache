data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)


--General--

convertBinToDec :: Integral a => a -> a
convertBinToDec x = convertBinToDec3 x 0 
convertBinToDec3 0 _ = 0
convertBinToDec3 x y =(mod x 10)*2^y + convertBinToDec3 (div x 10) (y+1)

-----------------------------------------------------------------------------------------------
replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem x (y:ys) 0 = (x:ys) 
replaceIthItem x (y:ys) z =  y:replaceIthItem x ys (z-1)

----------------------------------------------------------------------------------------------

splitEvery :: (Num a1, Ord a1) => a1 -> [a] -> [[a]]

splitEvery _ []=[]
splitEvery n l = (split1 n l ): splitEvery n (split2 n l) 

split1 0 _ =[] 
split1 n (y:ys) = y:split1 (n-1) ys

split2 0 l =l 
split2 n (y:ys) = split2 (n-1) ys

-----------------------------------------------------------------------------------------------

--logBase2 :: Floating a => a -> a
logBase2 x =logBase 2 x

-----------------------------------------------------------------------------------------------

--getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits numOfSets "fullyAssoc" cache = 0
getNumBits numOfSets "setAssoc" cache = ceiling(logBase2 n) where n = numOfSets
getNumBits numOfSets "directMap" cache = ceiling(logBase2 n) where n = fromIntegral (length (cache))
	
	
	
	
	
	
--if(cacheType=="fullyAssoc") then 0
	--								 else if (cacheType=="setAssoc") then ceiling(logBase2 numOfSets)
		--							 else ceiling(logBase2(fromIntegral (length cache)))

------------------------------------------------------------------------------------------------

fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char]
fillZeros n 0=n
fillZeros n a='0':fillZeros n (a-1)

------------------------------------------------------------------------------------------------
---GetDataDirect----

getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataFromCache stringAddress cache "directMap" bitsNum = directHelper (convertAddress (read stringAddress :: Int) bitsNum "directMap")  cache "directMap" 0
-------------------------------------------------------------------------------------------------
---GetDataSet----

	check (div (read stringAddress :: Int) (10^bitsNum))  (getset (mod (read stringAddress :: Int) (10^bitsNum)) (splitEvery (2^bitsNum) cache)) 0
getDataFromCache stringAddress cache "setAssoc" bitsNum=

-------------------------------------------------------------------------------------------------
--convertAddressSET----
--convertAddress :: (Integral a, Integral b) => b -> a -> String -> (b,b)
convertAddress binAddress bitsNum "setAssoc" = (div  binAddress (10^bitsNum),mod binAddress (10^bitsNum))
convertAddress binAddress bitsNum "directMap" = (div binAddress (10^bitsNum), mod binAddress (10^bitsNum))

----------------------------------------------------------------------------------------------------
---- DirectReplace ----

replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])
replaceInCache tag idx memory oldCache "directMap" bitsNum = 
    (
	(getdata (convertBinToDec idx  + convertBinToDec3 tag bitsNum) memory),
	(newcache (convertBinToDec idx)
	(changedirect (getdata (convertBinToDec idx  + convertBinToDec3 tag bitsNum) memory)tag(getitem (convertBinToDec idx) oldCache))
	 oldCache)
	)
-------------------------------------------------------------------------------------------------------
--- FullyReplace----
	
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum =
	(
	(getdata (convertBinToDec tag ) memory),
	(newcache (getplace oldCache 0 0)
	(changedirect (getdata (convertBinToDec idx  + convertBinToDec3 tag bitsNum) memory)tag(getitem (getplace oldCache 0 0) oldCache))
	(increment oldCache))
	)	
	
------------------------------------------------------------------------------------------------------	
---SetReplace----

replaceInCache tag idx memory oldCache "setAssoc" bitsNum =
	(
	(getdata (convertBinToDec idx  + convertBinToDec3 tag bitsNum) memory),
	 collect((newcache (convertBinToDec idx)
	(newcache (getplace (getset (convertBinToDec idx) (splitEvery (2^bitsNum) oldCache))  0 0)
	(changedirect (getdata (convertBinToDec idx  + convertBinToDec3 tag bitsNum) memory) tag (getitem (getplace (getset (convertBinToDec idx) (splitEvery (2^bitsNum) oldCache)) 0 0) (getset (convertBinToDec idx) (splitEvery (2^bitsNum) oldCache))))
	(increment (getset (convertBinToDec idx) (splitEvery (2^bitsNum) oldCache))))
	(splitEvery (2^bitsNum) oldCache)))
	)
	
-------------------------------------------------------------------------------------------------------	
---Methods---

check x [] l =	NoOutput
check tag ((It (T x) (D y) b z):ys) l =if(b==True && tag ==x) then(Out (y, l)) else(check tag ys (l+1))

directHelper _ [] _ _ = NoOutput
directHelper (t,i) ((It (T x) (D y) b z):ys) "directMap" counter = if (counter == i) && (b == True) && (x == t) then Out (y, 0) else directHelper (t,i) ys "directMap" (counter + 1)



getset 0 (x:xs) =x
getset n (x:xs) =getset (n-1) xs 

collect [] =[]
collect (x:xs)=(x++collect xs)
	
newcache 0 x (y:ys) =(x:ys)
newcache n x (y:ys) =y:newcache (n-1) x ys

changedirect x  y (It (T _) (D _) _ _)=(It (T y) (D x) True 0)	

getitem 0 (y:ys) = y
getitem x (y:ys) = getitem (x-1) ys

getdata 0 (y:ys) = y
getdata x (y:ys) = getdata (x-1) ys 

increment [] =[]
increment ((It (T x) (D y) True z):ys)=  (It (T x) (D y) True (z+1)):increment ys
increment ((It (T x) (D y) False z):ys)=  (It (T x) (D y) False (z)):increment ys

getplace ((It (T _) (D _) False _):ys) count r =count
getplace ((It (T a) (D b) True z1):(It (T c) (D d) False z2):ys) count  r =(count+1)
getplace [_] count r =r
getplace ((It (T a) (D b) True z1):(It (T c) (D d) True z2):ys) count  r =
	if z1>=z2 then(getplace ((It (T a) (D b) True z1):ys) (count+1)  r)  
	else (getplace ((It (T c) (D d) True z2):ys) (count+1)  (count+1) )
	
---------------------------------------------------------------------------------------------------------

getData :: (Eq t, Integral b) => String -> [Item t] -> [t] -> [Char] -> b -> (t, [Item t])
getData stringAddress cache memory cacheType bitsNum
	| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
	| otherwise = (getX x, cache)
	where
		x = getDataFromCache stringAddress cache cacheType bitsNum
		address = read stringAddress:: Int
		(tag, index) = convertAddress address bitsNum cacheType
getX (Out (d, _)) = d

	
runProgram :: (RealFloat a1, Eq a2) => [[Char]] -> [Item a2] -> [a2] -> [Char] -> a1 -> ([a2], [Item a2])
runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets =
	((d:prevData), finalCache)
	where
		bitsNum = round(logBase2 numSets)
		(d, updatedCache) = getData addr cache memory cacheType bitsNum
		(prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets

	
	

	
	
	

	
	

	
	
	
	

	
	
