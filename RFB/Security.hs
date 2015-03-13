module RFB.Security where

import Data.Char (ord, chr)

-- new defined functions used by DES Encryption starts here
----------------
desEncryption :: [Int] -> [[Int]] -> [Int]
desEncryption xs subkeys = res
        where
        chaIP = initPermutation xs
        l0 = firstHalf chaIP
        r0 = lastHalf chaIP -- input for feistel function

        l1 = r0
        r1 = xorTuple (zip l0 (feistel r0 0 subkeys))

        l2 = r1
        r2 = xorTuple (zip l1 (feistel r1 1 subkeys))

        l3 = r2
        r3 = xorTuple (zip l2 (feistel r2 2 subkeys))

        l4 = r3
        r4 = xorTuple (zip l3 (feistel r3 3 subkeys))

        l5 = r4
        r5 = xorTuple (zip l4 (feistel r4 4 subkeys))

        l6 = r5
        r6 = xorTuple (zip l5 (feistel r5 5 subkeys))
        
        l7 = r6
        r7 = xorTuple (zip l6 (feistel r6 6 subkeys))
        
        l8 = r7
        r8 = xorTuple (zip l7 (feistel r7 7 subkeys))
        
        l9 = r8
        r9 = xorTuple (zip l8 (feistel r8 8 subkeys))
        
        l10 = r9
        r10 = xorTuple (zip l9 (feistel r9 9 subkeys))
        
        l11 = r10
        r11 = xorTuple (zip l10 (feistel r10 10 subkeys))
        
        l12 = r11
        r12 = xorTuple (zip l11 (feistel r11 11 subkeys))
        
        l13 = r12
        r13 = xorTuple (zip l12 (feistel r12 12 subkeys))
        
        l14 = r13
        r14 = xorTuple (zip l13 (feistel r13 13 subkeys))
        
        l15 = r14
        r15 = xorTuple (zip l14 (feistel r14 14 subkeys))
        
        r16 = r15
        l16 = xorTuple (zip l15 (feistel r15 15 subkeys))
        
        resultBits = finalPermutation (l16 ++ r16)
        
        result = splitEvery 8 resultBits
        res = map binToDec result

-- DES functions
------------------------------------------------------------------------
-- initial permutation
initPermutation :: [Int] -> [Int]
initPermutation xs = permutation xs ip

-- final permutation
finalPermutation :: [Int] -> [Int]
finalPermutation xs = permutation xs fp

-- feistel functions
------------------------------------------------------------------------
feistel :: [Int] -> Int -> [[Int]] -> [Int]
feistel rightBlock roundCount subkeys = feistelPermutation (feistelSub (splitEvery 6 (feistelMix rightBlock (subkeys !! roundCount))))

-- feistel key mixing with feistel expansion
feistelMix :: [Int] -> [Int] -> [Int]
feistelMix half subkey = xorTuple (zip (feistelExpansion half) subkey)
-- feistel expansion
feistelExpansion :: [Int] -> [Int]
feistelExpansion xs = permutation xs e
-- feistel substitution
feistelSub :: [[Int]] -> [Int]
feistelSub mixs = concat (localSub mixs 0)
-- feistel permutation
feistelPermutation :: [Int] -> [Int]
feistelPermutation xs = permutation xs p

-- some local functions used in feistel functions
localSub :: [[Int]] -> Int -> [[Int]]
localSub mixs i
               | i == 7 = [lookupSTable (mixs !! i) (subs !! i)]
               | i < 7  = lookupSTable (mixs !! i) (subs !! i) : localSub mixs (i+1)

lookupSTable :: [Int] -> [Int] -> [Int]
lookupSTable block table = ext4BL 
        (decToBin (table !! ((getRow block) * 16 + (getColumn block))))

-- extend to 4-bit list, with 0s added by the left side
ext4BL :: [Int] -> [Int]
ext4BL xs = reverse (extendTo4Bits (reverse xs))

getRow :: [Int] -> Int
getRow bitList
               | h == 0 && l == 0 = 0
               | h == 0 && l == 1 = 1
               | h == 1 && l == 0 = 2
               | h == 1 && l == 1 = 3 
               where h = head bitList
                     l = last bitList   
                                         
getColumn :: [Int] -> Int
getColumn xs = binToDec (init (tail xs))

-- Key management
------------------------------------------------------------------------
-- permuted choice1
permutedChoice1 :: [Int] -> [Int]
permutedChoice1 xs = permutation xs pc1

-- permuted choice2
permutedChoice2 :: [Int] -> [Int]
permutedChoice2 xs = permutation xs pc2

getSubkeys :: [Char] -> [[Int]]
getSubkeys mykey = subkeys
        where
        -- VNC authentication: Mirror
        key2 = concatMap charToBitsVNC mykey
        -- General DES
        -- key2 = concatMap charToBits mykey
        key64 = extendTo64Bits key2
        
        keypc1 = permutedChoice1 key64
        left0 = firstHalf keypc1
        right0 = lastHalf keypc1
        
        left1 = rotateLeft left0 (ls !! 0)
        right1 = rotateLeft right0 (ls !! 0)
        subkey1 = permutedChoice2 (left1 ++ right1)
        
        left2 = rotateLeft left1 (ls !! 1)
        right2 = rotateLeft right1 (ls !! 1)
        subkey2 = permutedChoice2 (left2 ++ right2)
        
        left3 = rotateLeft left2 (ls !! 2)
        right3 = rotateLeft right2 (ls !! 2)
        subkey3 = permutedChoice2 (left3 ++ right3)
        
        left4 = rotateLeft left3 (ls !! 3)
        right4 = rotateLeft right3 (ls !! 3)
        subkey4 = permutedChoice2 (left4 ++ right4)
        
        left5 = rotateLeft left4 (ls !! 4)
        right5 = rotateLeft right4 (ls !! 4)
        subkey5 = permutedChoice2 (left5 ++ right5)
        
        left6 = rotateLeft left5 (ls !! 5)
        right6 = rotateLeft right5 (ls !! 5)
        subkey6 = permutedChoice2 (left6 ++ right6)
        
        left7 = rotateLeft left6 (ls !! 6)
        right7 = rotateLeft right6 (ls !! 6)
        subkey7 = permutedChoice2 (left7 ++ right7)
        
        left8 = rotateLeft left7 (ls !! 7)
        right8 = rotateLeft right7 (ls !! 7)
        subkey8 = permutedChoice2 (left8 ++ right8)
        
        left9 = rotateLeft left8 (ls !! 8)
        right9 = rotateLeft right8 (ls !! 8)
        subkey9 = permutedChoice2 (left9 ++ right9)
        
        left10 = rotateLeft left9 (ls !! 9)
        right10 = rotateLeft right9 (ls !! 9)
        subkey10 = permutedChoice2 (left10 ++ right10)
        
        left11 = rotateLeft left10 (ls !! 10)
        right11 = rotateLeft right10 (ls !! 10)
        subkey11 = permutedChoice2 (left11 ++ right11)
        
        left12 = rotateLeft left11 (ls !! 11)
        right12 = rotateLeft right11 (ls !! 11)
        subkey12 = permutedChoice2 (left12 ++ right12)
        
        left13 = rotateLeft left12 (ls !! 12)
        right13 = rotateLeft right12 (ls !! 12)
        subkey13 = permutedChoice2 (left13 ++ right13)
        
        left14 = rotateLeft left13 (ls !! 13)
        right14 = rotateLeft right13 (ls !! 13)
        subkey14 = permutedChoice2 (left14 ++ right14)
        
        left15 = rotateLeft left14 (ls !! 14)
        right15 = rotateLeft right14 (ls !! 14)
        subkey15 = permutedChoice2 (left15 ++ right15)
        
        left16 = rotateLeft left15 (ls !! 15)
        right16 = rotateLeft right15 (ls !! 15)
        subkey16 = permutedChoice2 (left16 ++ right16)
        
        subkeys = [subkey1, subkey2, subkey3, subkey4, subkey5,
                 subkey6, subkey7, subkey8, subkey9, subkey10,
                 subkey11, subkey12, subkey13, subkey14, subkey15, subkey16]

-- Other functions
------------------------------------------------------------------------

-- if the password length shorter than 8(64 bits), add 0s to the end
-- TODO These extend functions add 0s to the tail
-- However, in most of cases 0s need to be added before head
extendTo64Bits :: [Int] -> [Int]
extendTo64Bits bitList = reverse (extendList (reverse bitList) (length bitList) 64)

extendTo8Bits :: [Int] -> [Int]
extendTo8Bits bitList = reverse (extendList (reverse bitList) (length bitList) 8)

extendTo4Bits :: [Int] -> [Int]
extendTo4Bits bitList = reverse (extendList (reverse bitList) (length bitList) 4)

extendList :: [Int] -> Int -> Int -> [Int]
extendList xs a n  
                | a == n = xs
                | a < n = extendList (0:xs) (a+1) n
                | a > n = take 64 xs -- should be improved to allow longer(than 8 chars) password

-- Permutation: permute xs according to indextable.
permutation :: [Int] -> [Int] -> [Int] -- or [Integral]
permutation xs [] = []
permutation xs (i:indextable)
        | i+1 > length xs = 0 :  permutation xs indextable -- error case
        | otherwise = (xs !! i) : permutation xs indextable

-- Get half list
firstHalf :: [Int] -> [Int]
firstHalf [] = []
firstHalf xs = take (div (length xs) 2) xs

lastHalf :: [Int] -> [Int]
lastHalf [] = []
lastHalf xs = drop (div (length xs) 2) xs

-- split a list into pieces with length n(may not be n for last piece)
splitEvery :: Int -> [Int] -> [[Int]]
splitEvery n xs
                | n >= length xs = [xs]
                | otherwise = (take n xs) : splitEvery n (drop n xs) 

-- Convert char to binary (8-bit list)
charToBits :: Char -> [Int]
charToBits c = reverse (extendTo8Bits (reverse (decToBin (ord c))))
-- 
charToBitsVNC :: Char -> [Int]
charToBitsVNC xs = reverse (charToBits xs)

-- Convert decimal to 8-bit binary
decToBin8 :: Int -> [Int]
decToBin8 xs = extendTo8BitsLeft (decToBin xs)

extendTo8BitsLeft :: [Int] -> [Int]
extendTo8BitsLeft xs = reverse (extendTo8Bits (reverse xs))

-- Convert decimal(Int) to binary
decToBin :: Int -> [Int]
decToBin n = reverse (localD2B n)

localD2B :: Int -> [Int]
localD2B 0 = [0]
localD2B 1 = [1]
localD2B n = (mod n 2) : localD2B (div n 2)

-- Convert binary to decimal
binToDec :: [Int] -> Int
binToDec xs = localB2D (reverse xs)
 
localB2D :: [Int] -> Int
localB2D [] = 0
localB2D (x:xs) = x + 2 * localB2D xs

-- Shift(rotate) a bit list
shiftLeft :: [Int] -> Int -> [Int]
shiftLeft bitList n = rotateLeft bitList n

rotateLeft :: [Int] -> Int -> [Int]
rotateLeft bitlist n = drop n bitlist ++ take n bitlist

xorTuple :: [(Int, Int)] -> [Int]
xorTuple [] = []
xorTuple ((a,b):xs) = (xorlocal a b) : xorTuple xs

xorlocal :: Int -> Int -> Int
xorlocal a b 
        | a == b = 0
        | a /= b = 1

decrease :: Int -> Int
decrease x = x - 1


-- tables
------------------------------------------------------------------------
-- Makes IP(Initial Permutation)
ip0 = [58 :: Int,50,42,34,26,18,10,2,60,52,44,36,28,20,12,4,62,54,46,38,30,22,14,6,64,56,48,40,32,24,16,8,57,49,41,33,25,17,9,1,59,51,43,35,27,19,11,3,61,53,45,37,29,21,13,5,63,55,47,39,31,23,15,7]
ip = map decrease ip0

-- Makes IP-1(FP, Final Permutation)
fp0 = [40 :: Int,8, 48,16,56,24,64,32,39,7, 47,15,55,23,63,31,38,6, 46,14,54,22,62,30,37,5, 45,13,53,21,61,29,36,4, 44,12,52,20,60,28,35,3, 43,11,51,19,59,27,34,2, 42,10,50,18,58,26,33,1, 41,9, 49,17,57,25]
fp = map decrease fp0

-- Make s E (Expansion in Feistel)
e0 = [32 :: Int,1, 2, 3, 4, 5,4, 5, 6, 7, 8, 9,8, 9,10,11,12,13,12,13,14,15,16,17,16,17,18,19,20,21,20,21,22,23,24,25,24,25,26,27,28,29,28,29,30,31,32,1]
e = map decrease e0

-- Makes S.
s0 = [14 :: Int,4,13,1,2,15,11,8,3,10,6,12,5,9,0,7,0,15,7,4,14,2,13,1,10,6,12,11,9,5,3,8,4,1,14,8,13,6,2,11,15,12,9,7,3,10,5,0,15,12,8,2,4,9,1,7,5,11,3,14,10,0,6,13]
s1 = [15 :: Int,1,8,14,6,11,3,4,9,7,2,13,12,0,5,10,3,13,4,7,15,2,8,14,12,0,1,10,6,9,11,5,0,14,7,11,10,4,13,1,5,8,12,6,9,3,2,15,13,8,10,1,3,15,4,2,11,6,7,12,0,5,14,9]
s2 = [10 :: Int,0,9,14,6,3,15,5,1,13,12,7,11,4,2,8,13,7,0,9,3,4,6,10,2,8,5,14,12,11,15,1,13,6,4,9,8,15,3,0,11,1,2,12,5,10,14,7,1,10,13,0,6,9,8,7,4,15,14,3,11,5,2,12]
s3 = [7 :: Int,13,14,3,0,6,9,10,1,2,8,5,11,12,4,15,13,8,11,5,6,15,0,3,4,7,2,12,1,10,14,9,10,6,9,0,12,11,7,13,15,1,3,14,5,2,8,4,3,15,0,6,10,1,13,8,9,4,5,11,12,7,2,14]
s4 = [2 :: Int,12,4,1,7,10,11,6,8,5,3,15,13,0,14,9,14,11,2,12,4,7,13,1,5,0,15,10,3,9,8,6,4,2,1,11,10,13,7,8,15,9,12,5,6,3,0,14,11,8,12,7,1,14,2,13,6,15,0,9,10,4,5,3]
s5 = [12 :: Int,1,10,15,9,2,6,8,0,13,3,4,14,7,5,11,10,15,4,2,7,12,9,5,6,1,13,14,0,11,3,8,9,14,15,5,2,8,12,3,7,0,4,10,1,13,11,6,4,3,2,12,9,5,15,10,11,14,1,7,6,0,8,13]
s6 = [4 :: Int,11,2,14,15,0,8,13,3,12,9,7,5,10,6,1,13,0,11,7,4,9,1,10,14,3,5,12,2,15,8,6,1,4,11,13,12,3,7,14,10,15,6,8,0,5,9,2,6,11,13,8,1,4,10,7,9,5,0,15,14,2,3,12]
s7 = [13 :: Int,2,8,4,6,15,11,1,10,9,3,14,5,0,12,7,1,15,13,8,10,3,7,4,12,5,6,11,0,14,9,2,7,11,4,1,9,12,14,2,0,6,10,13,15,3,5,8,2,1,14,7,4,10,8,13,15,12,9,0,3,5,6,11]
-- substitution table
subs = [s0, s1, s2, s3, s4, s5, s6, s7]
 
-- Makes P
p = [15 :: Int,6,19,20,28,11,27,16,0,14,22,25,4,17,30,9,1,7,23,13,31,26,2,8,18,12,29,5,21,10,3,24]

-- Makes PC-1 and -2
-- Permuted choices
pc1 = [56 :: Int,48,40,32,24,16,8,0,57,49,41,33,25,17,9,1,58,50,42,34,26,18,10,2,59,51,43,35,62,54,46,38,30,22,14,6,61,53,45,37,29,21,13,5,60,52,44,36,28,20,12,4,27,19,11,3]
pc2 = [13 :: Int,16,10,23,0,4,2,27,14,5,20,9,22,18,11,3,25,7,15,6,26,19,12,1,40,51,30,36,46,54,29,39,50,44,32,47,43,48,38,55,33,52,45,41,49,35,28,31]

-- Generates left shift table
ls = [1 :: Int,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1]
