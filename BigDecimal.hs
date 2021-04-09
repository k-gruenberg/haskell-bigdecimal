-- Entering ':i Double' in GHCi gives:
-- data Double = GHC.Types.D# GHC.Prim.Double#
--  	-- Defined in ‘GHC.Types’
-- instance Eq Double -- Defined in ‘GHC.Classes’
-- instance Ord Double -- Defined in ‘GHC.Classes’
-- instance Enum Double -- Defined in ‘GHC.Float’
-- instance Floating Double -- Defined in ‘GHC.Float’
-- instance Fractional Double -- Defined in ‘GHC.Float’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Real Double -- Defined in ‘GHC.Float’
-- instance RealFloat Double -- Defined in ‘GHC.Float’
-- instance RealFrac Double -- Defined in ‘GHC.Float’
-- instance Show Double -- Defined in ‘GHC.Float’
-- instance Read Double -- Defined in ‘GHC.Read’
--
-- Down below we try to instantiate all of the above for a BigDecimal type with infinitely many digits after the decimal point:


-- Please be aware that, due to its infinite decimal expansion, a BigDecimal has the following restrictions compared to the ordinary number types:
--   * Equality of two BigDecimals can never be proven, only disproven!
--     This means that (==) will only ever return False but never True and that `compare` will only ever return LT or GT but never EQ.
--   * One can devide by or calculate the reciprocal of 2.00000... but not 1.99999... (this is due to the infinite series used for reciprocal calculation)
--   * As of now, one cannot add 0.00000... + 0.00000... for technical reasons (this is something that could possibly be fixed later...)



module BigDecimal (
  BigDecimal,
  (~==),                 -- :: BigDecimal -> BigDecimal -> Bool
  show_N_decimal_places, -- :: Int -> BigDecimal -> String
  print_BigDecimal,      -- :: BigDecimal -> IO ()
  toRational_precision,  -- :: Int -> BigDecimal -> Rational
  evalInfiniteSum,       -- :: [BigDecimal] -> BigDecimal
  modulo,                -- :: (Num a, Ord a) => a -> a -> a
) where

import Data.Ratio ((%),numerator,denominator) -- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ratio.html -- type Rational = Ratio Integer
import System.IO (hFlush, stdout)             -- used for `print_BigDecimal`
import Control.Monad (forM_)                  -- used for `print_BigDecimal`








data Sign = Plus | Minus deriving (Eq)

instance Show Sign where
  show Plus  = ""
  show Minus = "-"

-- conversion to and from `signum`:
instance Enum Sign where
  fromEnum Plus  =  1
  fromEnum Minus = -1
  toEnum   1  = Plus
  toEnum (-1) = Minus
  toEnum   0  = Plus
  toEnum   _  = error "toEnum for Sign expects either a 1, -1 or 0, i.e. something that Prelude.signum returns"

instance Num Sign where
  x + y = undefined
  x * y = undefined
  abs    x      = undefined
  negate Plus   = Minus
  negate Minus  = Plus
  signum Plus   = Plus  -- 1
  signum Minus  = Minus -- -1
  fromInteger i = toEnum $ fromInteger i







type DecDigit = Int -- Even though it wastes a lot of space, we're storing decimal digits as Ints.
data BigDecimal = BigDecimal {sign :: Sign, integerPart :: Integer, fractionalPart :: [DecDigit]}
-- * Using a scale to specifiy where the decimal point is (like it's done in Java's BigDecimal) wouldn't allow laziness,
--   therefore we're storing the digits after the decimal point as an infinite(always!) lazy list of (decimal) digits.
-- * Because 0 == -0 we have to store the sign of a BigDecimal seperately from its integerPart - otherwise we couldn't tell apart +0.1234... and -0.1234... for example!
--   As a result, we will require that the integerPart shall always be a positive Integer!


show_N_decimal_places :: Int -> BigDecimal -> String
show_N_decimal_places n BigDecimal{sign=s, integerPart=i, fractionalPart=f} 
  | n <= 0    = (show s) ++ (show i)
  | otherwise = (show s) ++ (show i) ++ "." ++ (concat $ (map show) $ (take n) f) ++ "..."


instance Show BigDecimal where
  show = show_N_decimal_places 50


print_BigDecimal :: BigDecimal -> IO ()
print_BigDecimal BigDecimal{sign=s, integerPart=i, fractionalPart=f} = do
  putStr $ show s
  putStr $ show i
  putStr "."
  hFlush stdout
  forM_ f (putStr . show)


instance Read BigDecimal where
  readsPrec p r = [(fromRational i, t) | (i,t) <- readsPrec p r] 
-- -- -- -- Copied from Prelude (https://www.haskell.org/onlinereport/standard-prelude.html): -- -- --
-- instance  Read Int  where
--   readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]  


-- Attention!:
-- 1) Since their decimal expansions are infinite, equality of two BigDecimals can never be proven, only disproven!
-- 2) 2.999999... == 3.000000...
instance Eq BigDecimal where
  x /= y = integerPart diff /= 0 || any (/=0) (fractionalPart diff) where diff=x-y   -- x /= y iff (x-y) /= +-0  -- any (/=0) can never return False! 


-- Operator to check whether two BigDecimals are approximately equal - always terminates, unlike (==):
(~==) :: BigDecimal -> BigDecimal -> Bool
x ~== y = integerPart diff == 0 && all (==0) (take 100 (fractionalPart diff)) where diff=x-y


-- Attention again!:
-- * Since equality of two BigDecimals can never be proven (only disproven), `compare` will either return LT, GT or never terminate - it will never return EQ!
-- * compare (2.999999...) (3.000000...) should be EQ i.e. never terminate!
instance Ord BigDecimal where
  compare x y = if x /= y then (if sign (x-y) == Plus then GT else LT) else EQ -- the else-case is never reached because x /= y can never return False
  -- Note that we have to do the x /= y check to ensure that (x-y) /= 0.0000... which would means that 'sign (x-y)' could be either Plus or Minus, randomly!
  -- The following approach doesn't consider the '2.999999... == 3.000000...' case!:
  -- compare BigDecimal{integerPart=i1,fractionalPart=f1} BigDecimal{integerPart=i2,fractionalPart=f2}
  --  | i1 <  i2 = LT
  --  | i1 >  i2 = GT
  --  | i1 == i2 = if (x < y) then LT else GT where (x,y) = head $ (dropWhile (\(x,y) -> x==y)) (zip f1 f2) -- find the first digit after the decimal point that differs!


instance Enum BigDecimal where
  toEnum int                                                   = BigDecimal {sign = toEnum $ signum int, integerPart = abs $ toInteger int, fractionalPart = [0,0..]}
  fromEnum BigDecimal{sign=s, integerPart=i, fractionalPart=f} = (fromEnum s) * (fromInteger i) -- fromEnum 3.14 == 3 ; fromEnum (-3.14) == -3


instance Num BigDecimal where
  -- ===== ===== BigDecimal Addition: ===== =====
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Minus,integerPart=i2,fractionalPart=f2} = -- (-x)+(-y) == -(x+y)
      negate BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} 
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Plus ,integerPart=i2,fractionalPart=f2} = -- (-x)+y == y+(-x)
      BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} + BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1}
  x + y -- x+y == x+y+0+0+0+0+0+... -- However, we need to ensure that the absolute value of the numbers given to `evalInfiniteSum` are given in more or less non-ascending order!
    | integerPart x > integerPart y  = evalInfiniteSum ([x,y] ++ [0,0..])
    | integerPart y > integerPart x  = evalInfiniteSum ([y,x] ++ [0,0..])
    | integerPart x /= 0             = evalInfiniteSum ([x,y] ++ [0,0..]) -- the integerParts of x and y are the same but not zero: the order is irrelevant, just use [x,y]
    | fst digitPair >= snd digitPair = evalInfiniteSum ([x,y] ++ [0,0..]) -- x >= y (x is larger/has less leading zeros and should therefore come first)
    | otherwise                      = evalInfiniteSum ([y,x] ++ [0,0..]) -- y > x  (y ...)
    where digitPair = head $ (dropWhile (==(0,0))) $ zip (fractionalPart x) (fractionalPart y) -- the first decimal place where x and y are not both 0
    -- ToDo/Problem: the above call will never terminate when trying to add 0 to 0 !!!
    --   The only way to solve this is to actually implement the addition of 2 numbers seperately from `evalInfiniteSum`!

  -- ===== ===== BigDecimal Multiplication: ===== =====
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Plus ,integerPart=i2,fractionalPart=f2} = -- (-x)*y = -(x*y)
      negate BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} 
  BigDecimal{sign=Plus ,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Minus,integerPart=i2,fractionalPart=f2} = -- x*(-y) = -(x*y)
      negate BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2}
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Minus,integerPart=i2,fractionalPart=f2} = -- (-x)*(-y) = x*y
      BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} 
  -- From now on, we can assume that both factors are positve:
  -- We will split the multiplication into four parts: (i1+f1)*(i2+f2) = i1*i2 + i1*f2 + f1*i2 + f1*f2
  BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} * BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} =
      BigDecimal{sign=Plus, integerPart=i1*i2, fractionalPart=[0,0..]} -- i1*i2
    + mulIntFraction i1 f2                                             -- i1*f2
    + mulIntFraction i2 f1                                             -- f1*i2
    + mulFractionFraction f1 f1                                        -- f1*f2
    where -- Idea: 12 * 0.123456789... =
          --       1.2
          --     + 0.24
          --     + 0.036
          --     + 0.0048
          --     + 0.00060
          --     + ...
          --   Evaluate that infinite sum using `evalInfiniteSum` (as it's strictly decreasing!)
          mulIntFraction :: Integer -> [DecDigit] -> BigDecimal
          mulIntFraction int fraction = evalInfiniteSum [mul int (fraction!!(digitPos-1)) digitPos | digitPos <- [1..]::[Int] ]

          mul :: Integer -> DecDigit -> Int -> BigDecimal -- == intFactor * digit * 10^(-position) ; e.g. mul 12 3 3 == 0.036 (see above)
          mul intFactor digit position
            | noOfCarryDigits < position = BigDecimal{sign=Plus, integerPart=0, fractionalPart=error "ToDo"}
            | otherwise                  = BigDecimal{sign=Plus, integerPart=error "ToDo", fractionalPart=error "ToDo"}
              where noOfCarryDigits = length (show (intFactor*(toInteger digit))) - 1 -- (== 1 for the 0.036 example above)
          
          -- Idea: 0.232323... * 0.454545... =
          --       2 * 0.454545... << 1
          --     + 3 * 0.454545... << 2
          --     + 2 * 0.454545... << 3
          --     + 3 * 0.454545... << 4
          --     + ...
          --   where "<< n" means shift the decimal point n to the left (i.e. 'divide' by 10^n)
          --   Evaluate the products using `mulIntFraction` and the infinite sum using `evalInfiniteSum` (it's also strictly decreasing!)
          mulFractionFraction :: [DecDigit] -> [DecDigit] -> BigDecimal
          mulFractionFraction frac1 frac2 = evalInfiniteSum [(mulIntFraction (toInteger (frac1!!(digit-1))) frac2) << digit | digit <- [1..]::[Int]]
          
          (<<) :: BigDecimal -> Int -> BigDecimal -- shift the decimal point n to the left (i.e. 'divide' by 10^n), e.g. 12.345... << 1 == 1.2345...
          x << 0 = x
          BigDecimal{sign=s,integerPart=i,fractionalPart=f} << 1 = BigDecimal{sign=s, integerPart= i `div` 10    , fractionalPart= (fromInteger (i `mod` 10)):f}
          BigDecimal{sign=s,integerPart=i,fractionalPart=f} << n = BigDecimal{sign=s, integerPart= i `div` (10^n), fractionalPart= (nLastDigits n i)       ++ f}
          
          nLastDigits :: Int -> Integer -> [DecDigit] -- e.g. nLastDigits 2 123456 == [5,6]
          nLastDigits n integer = map (read . (:"")) (show (integer `mod` (10^n))) -- (:"") is to convert '3' to "3" because read expects a String, not a Char



  negate BigDecimal{sign=s,integerPart=i,fractionalPart=f} = BigDecimal {sign = negate s, integerPart = i, fractionalPart = f}
  abs    BigDecimal{sign=s,integerPart=i,fractionalPart=f} = BigDecimal {sign = Plus, integerPart = i, fractionalPart = f} 
  fromInteger integer                                      = BigDecimal {sign = toEnum $ fromInteger $ signum integer, integerPart = abs integer, fractionalPart = [0,0..]}
  signum bigdec = if (bigdec /= 0) then (if sign bigdec == Plus then 1 else (-1)) else 0 -- the else-case is unreachable!




-- https://wiki.haskell.org/Rational : "Values of type Rational represent rational numbers exactly as the ratio of two Integers. [...];
--                                      applying toRational to a Real number will produce its rational value (or its closest approximation)."
instance Real BigDecimal where
  toRational = toRational_precision 100 -- Setting precision=100: the number of decimalPlaces in fractionalPart to be considered

toRational_precision :: Int -> BigDecimal -> Rational
toRational_precision precision BigDecimal{sign=s, integerPart=i,fractionalPart=f} = (shiftDecimalPointRight precision ((toInteger $ fromEnum s) * i) f) % (10^precision)
    where shiftDecimalPointRight shift intPart fracPart = intPart*(10^(toInteger shift)) + (sum $ multiplyByPowersOfTen $ (map toInteger) $ reverse $ (take shift) fracPart)
          multiplyByPowersOfTen xs = mul 0 xs -- multiplyByPowersOfTen [1,2,3,4] == [1,20,300,4000]
          mul power []     = []
          mul power (x:xs) = (x * 10^power):(mul (power+1) xs)



instance Fractional BigDecimal where -- one needs to implement either ‘recip’ or ‘/’
  fromRational rational =
    BigDecimal {sign = toEnum $ fromInteger $ signum $ numerator rational, integerPart = abs $ numerator rational, fractionalPart = [0,0..]}
    / BigDecimal {sign = Plus, integerPart = denominator rational, fractionalPart = [0,0..]} -- as you can see below the denominator of a Rationals is always positve:
  -- numerator   (3 % (-4)) == (-3)
  -- denominator (3 % (-4)) == 4
  -- numerator   ((-3) % (-4)) == 3
  -- denominator ((-3) % (-4)) == 4

  -- Use the following two Taylor Series expansions for calculating the reciprocal of a BigDecimal: 
  --   * 1/x     = 1 - (x - 1) + (x - 1)^2 - (x - 1)^3 + (x - 1)^4 - (x - 1)^5 + O((x - 1)^6)    | converges when abs(x-1) < 1 ; expansion at x=1
  --   * 1/(a+b) = the sum of  (b^n * (-a)^(-n))/a  as n goes from 0 to infinity                 | for abs(b) < abs(a)
  -- We will use the 1st expansion when the integerPart is 0 (or 1, which is optional) and the 2nd expansion in all other cases.
  -- When when want to calculate the reciprocal of x=1.99999... (i.e. a=1, b=0.99999...) we got a problem with both series and `recip` shall never terminate :(
  -- Calculating the reciprocal of x=2.00000... (i.e. a=2, b=0.00000...) works just fine with the second series however as 0.00000... < 2
  -- 
  recip x@BigDecimal{sign=s,integerPart=i,fractionalPart=f}
    | s==Minus            = negate $ recip $ negate x -- 1/(-x) = -(1/x)
    | i==1 && all (==9) f = 0.5                       -- see above: (recip 1.99999...) shall never terminate as *both* series expansions won't work
    | i==0 && all (==0) f = undefined                 -- 1/0 is undefined anyways (never returns 'undefined' however as equality is unsolvable for BigDecimals!)
    | i==0                = evalInfiniteSum [ (-1)^n * (x-1)^n | n <- [0..]::[Int]]               -- use the first series expansion as abs(x-1) < 1
    | otherwise           = oneOverAplusB i BigDecimal{sign=Plus,integerPart=0,fractionalPart=f}  -- use the second series expansion
       where oneOverAplusB :: Integer -> BigDecimal -> BigDecimal -- evaluates the Taylor Series for 1/(a+b):
             oneOverAplusB a b = evalInfiniteSum [ (b^n) * (oneOverInt (-a)^n) * (oneOverInt a) | n <- [0..]::[Int]]
                          
             oneOverInt :: Integer -> BigDecimal -- 1/x, takes a positive non-zero Integer, returns the reciprocal as a BigDecimal
             oneOverInt = error "ToDo: oneOverInt - implement standard division algorithm: 1/Integer -> BigDecimal"



instance Floating BigDecimal where
  pi = BigDecimal {sign = Plus, integerPart = 3, fractionalPart = tail a000796_list}         
         --[   1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7,
         -- 9, 5, 0, 2, 8, 8, 4, 1, 9, 7, 1, 6, 9, 3, 9, 9, 3, 7, 5, 1, 0, 5, 8, 2, 0, 9, 7, 4, 9, 4,
         -- 4, 5, 9, 2, 3, 0, 7, 8, 1, 6, 4, 0, 6, 2, 8, 6, 2, 0, 8, 9, 9, 8, 6, 2, 8, 0, 3, 4, 8, 2,
         -- 5, 3, 4, 2, 1, 1, 7, 0, 6, 7, 9, 8, 2, 1, 4] --  Source: https://oeis.org/A000796
         -- ++ [0,0..]}
  exp   x = evalInfiniteSum [ x^n / fact n | n <- [0..]::[Int]]
  log   x = 2 * atanh ((x-1) / (x+1))
  sin   x = evalInfiniteSum [ (-1)^n * ( (x'^(2*n+1)) / fact (2*n+1) ) | n <- [0..]::[Int]] where x' = x `modulo` (2*pi)
  cos   x = evalInfiniteSum [ (-1)^n * ( (x'^(2*n)  ) / fact (2*n)   ) | n <- [0..]::[Int]] where x' = x `modulo` (2*pi)
  sinh  x = evalInfiniteSum [          ( (x ^(2*n+1)) / fact (2*n+1) ) | n <- [0..]::[Int]] 
  cosh  x = evalInfiniteSum [          ( (x ^(2*n)  ) / fact (2*n)   ) | n <- [0..]::[Int]] 
  asin  x = evalInfiniteSum [ ( doubleFact (2*n-1) / doubleFact (2*n) ) * ( (x^(2*n+1)) / (fromIntegral (2*n+1)) ) | n <- [0..]::[Int]]
  acos  x = (pi/2) - asin x
  atan  x
    | (abs x <= 1) = evalInfiniteSum [ (-1)^n * ( (x^(2*n+1)) / (fromIntegral (2*n+1)) ) | n <- [0..]::[Int]]
    | otherwise    = 2 * atan ( x / (1+sqrt(1+x^2)) )
  asinh x = x * evalInfiniteSum [ ( doubleFact(2*n-1) * (-x^2)^n ) / ( doubleFact(2*n) * (fromIntegral (2*n+1)) ) | n <- [0..]::[Int] ]
  acosh x = log(2*x) - evalInfiniteSum [ ( doubleFact (2*n-1) / (2 * (fromIntegral n) * doubleFact(2*n)) ) * (x^(-2*n)) | n <- [1..]::[Int]]
  atanh x = evalInfiniteSum [ (x^(2*n+1)) / (fromIntegral (2*n+1)) | n <- [0..]::[Int]]


fact :: Int -> BigDecimal
fact x = BigDecimal {sign = Plus, integerPart = fact' $ toInteger x , fractionalPart = [0,0..]}
  where fact' 0 = 1
        fact' n = product [1..n]

doubleFact :: Int -> BigDecimal
doubleFact x = BigDecimal {sign = Plus, integerPart = doubleFact' $ toInteger x , fractionalPart = [0,0..]}
  where doubleFact' :: Integer -> Integer
        doubleFact' n
          | (n == (-1) || n == 0) = 1                    -- definition copied from de.wikipedia.org # Fakultaet # Doppelfakultaet
          | (n > 0 && even n)     = product [n,(n-2)..2]
          | (n > 0 && odd n)      = product [n,(n-2)..1]

-- The mod function from Prelude only works on Integrals!!
modulo :: (Num a, Ord a) => a -> a -> a -- (subtract the second argument from the first argument as often as possible)
modulo x y
  | (x >= y)  = modulo (x-y) y
  | otherwise = x






instance RealFrac BigDecimal where -- properFraction :: (RealFrac a, Integral b) => a -> (b, a)
  properFraction BigDecimal{sign=s,integerPart=i,fractionalPart=f}
    = ( fromInteger ( (toInteger $ fromEnum s) * i ) , BigDecimal{sign=s, integerPart=0, fractionalPart=f} )
-- properFraction   3.14  == (3,0.14000000000000012)
-- properFraction (-3.14) == (-3,-0.14000000000000012)


-- instance RealFloat BigDecimal where -- impossible because BigDecimal is NOT an actual floating point number








-- -- -- -- -- -- evalInfiniteSum -- -- -- -- --
-- This is a helper function not only for evaluating the Taylor Series needed for the Floating typeclass but also for (+) and (*) of the Num typeclass and the recip function
--   of the Fractional typeclass.
-- Therefore this is the function containing the main (calculation) logic for handling the infinitude of BigDecimals!
--
-- e.g. evalInfiniteSum [1.111111111111...,
--                       0.111111111111...,
--                       0.011111111111...,
--                       0.001111111111...,
--                       0.000111111111...,
--                       0.000011111111...,
--                       ...]
--                    == 1.234567... (which is 100/81 as a fraction by the way)
--
-- Because the decimal expansion of ALL(!) BigDecimal is infinite, we have to use left-to-right addition!
-- The function also has to be able to deal with some of the values being negative!
-- For simplicity we assume that the given list of BigDecimals is always infinite.
--   When one wants to sum only finitely many BigDecimals, the rest shall be filled up with infinitely many (BigDecimal) zeros.
--
-- In order for this function to work, the absolute value of the numbers in the given list have to be in descending/non-ascending order!
--   (more or less, i.e. at least in terms of their magnitude/leading zeros)
-- To be more precise: When one of the numbers begins with n zeros, this function assumes that all the numbers that follow will also begin with at least n zeros!
--
evalInfiniteSum :: [BigDecimal] -> BigDecimal
evalInfiniteSum nums = BigDecimal {sign           = toEnum $ fromInteger $ signum (infiniteSumWithCarry!!0),
                                   integerPart    = abs (infiniteSumWithCarry!!0),
                                   fractionalPart = (map fromInteger) $ tail $ infiniteSumWithCarry}

  where integerPartSum          = sum $ takeWhile (/=0) (map (\num -> (toInteger $ fromEnum $ sign num) * integerPart num) nums)         :: Integer
        columnSum n             = sum $ takeWhile (/=0) (map (\num -> toInteger ((fractionalPart num)!!n * (fromEnum $ sign num))) nums) :: Integer -- sum of the n-th column
        infiniteSumWithoutCarry = integerPartSum : [columnSum n | n <- [0..]::[Int]] -- the 0th element in this list (integerPartSum) is the only one allowed to be >9 or <0 ...
        -- Now comes the difficult part: carrying from left to right:
        infiniteSumWithCarry    = carry infiniteSumWithoutCarry
        
        carry :: [Integer] -> [Integer] -- e.g. carry [1,2,3,4,5,6,7,8,9,10,0,0,0,0...] == [1,2,3,4,5,6,7,9,0,0,0,0...] (also has to deal with the numbers being negative!)
        carry (a:b:cs)
          | b > 9     = (a + (b `div` 10)):(carry ((b `mod` 10):cs)) -- e.g. carry [4,23,...] == 6:(carry 3:...)
          | b < 0     = error "ToDo"
          | otherwise =  a                :(carry ( b          :cs))







-- The decimal expansion/digits of Pi | Source: https://oeis.org/A000796 (Reinhard Zumkeller 2013)
a000796_list = map fromInteger $ piStream (1, 0, 1)
   [(n, a*d, d) | (n, d, a) <- map (\k -> (k, 2 * k + 1, 2)) [1..]] where
   piStream z xs'@(x:xs)
     | lb /= approx z 4 = piStream (mult z x) xs
     | otherwise = lb : piStream (mult (10, -10 * lb, 1) z) xs'
     where lb = approx z 3
           approx (a, b, c) n = div (a * n + b) c
           mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 
