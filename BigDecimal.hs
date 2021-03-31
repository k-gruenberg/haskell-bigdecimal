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

module BigDecimal (
  BigDecimal,
  show_N_decimal_places, -- :: Int -> BigDecimal -> String
  toRational_precision,  -- :: Int -> BigDecimal -> Rational
  evalInfiniteSum,       -- :: [BigDecimal] -> BigDecimal
  modulo,                -- :: (Num a, Ord a) => a -> a -> a
) where

import Data.Ratio ((%),numerator,denominator) -- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Ratio.html -- type Rational = Ratio Integer




data BigDecimal = BigDecimal {integerPart :: Integer, fractionalPart :: [Int]} deriving (Eq) -- Equality of two BigDecimals can never be proven, only disproven!
-- using a scale to specifiy where the decimal point is (like it's done in Java's BigDecimal) wouldn't allow laziness,
--   therefore we're storing the digits after the decimal point as an infinite(always!) lazy list of digits

show_N_decimal_places :: Int -> BigDecimal -> String
show_N_decimal_places n BigDecimal{integerPart=i,fractionalPart=f} 
  | n <= 0    = show i
  | otherwise = (show i) ++ "." ++ (concat $ (map show) $ (take n) f) ++ "..."


instance Show BigDecimal where
  show = show_N_decimal_places 50 -- show BigDecimal{integerPart=i,fractionalPart=f} = (show i) ++ "." ++ (concat $ (map show) $ (take 50) f) ++ "..."


instance Read BigDecimal where
  readsPrec p r = [(fromRational i, t) | (i,t) <- readsPrec p r] 
-- -- -- -- Copied from Prelude (https://www.haskell.org/onlinereport/standard-prelude.html): -- -- --
-- instance  Read Int  where
--   readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]  


instance Ord BigDecimal where
  compare BigDecimal{integerPart=i1,fractionalPart=f1} BigDecimal{integerPart=i2,fractionalPart=f2}
    | i1 <  i2 = LT
    | i1 >  i2 = GT
    | i1 == i2 = if (x < y) then LT else GT where (x,y) = head $ (dropWhile (\(x,y) -> x==y)) (zip f1 f2) -- find the first digit after the decimal point that differs!
                 -- Equality of two BigDecimals can never be proven, only disproven!


instance Enum BigDecimal where
  toEnum int                                          = BigDecimal {integerPart = toInteger int, fractionalPart = [0,0..]}
  fromEnum BigDecimal{integerPart=i,fractionalPart=f} = fromInteger i -- fromEnum 3.14 == 3 ; fromEnum (-3.14) == -3


-- !!! Because the decimal expansion of ALL(!) BigDecimal is infinite, we have to use left-to-right addition !!!
instance Num BigDecimal where
  BigDecimal{integerPart=i1,fractionalPart=f1} + BigDecimal{integerPart=i2,fractionalPart=f2} = error "ToDo"

  BigDecimal{integerPart=i1,fractionalPart=f1} * BigDecimal{integerPart=i2,fractionalPart=f2} = error "ToDo"

  negate BigDecimal{integerPart=i,fractionalPart=f} = BigDecimal {integerPart = negate i, fractionalPart = f}
  abs    BigDecimal{integerPart=i,fractionalPart=f} = BigDecimal {integerPart = abs i   , fractionalPart = f} 
  signum BigDecimal{integerPart=i,fractionalPart=f} = BigDecimal {integerPart = signum i, fractionalPart = [0,0..]}
  fromInteger integer                               = BigDecimal {integerPart = integer , fractionalPart = [0,0..]}



-- https://wiki.haskell.org/Rational : "Values of type Rational represent rational numbers exactly as the ratio of two Integers. [...];
--                                      applying toRational to a Real number will produce its rational value (or its closest approximation)."
instance Real BigDecimal where
  toRational = toRational_precision 100 -- Setting precision=100: the number of decimalPlaces in fractionalPart to be considered

toRational_precision :: Int -> BigDecimal -> Rational
toRational_precision precision BigDecimal{integerPart=i,fractionalPart=f} = (shiftDecimalPointRight precision i f) % (10^precision)
    where shiftDecimalPointRight shift intPart fracPart = intPart*(10^(toInteger shift)) + (sum $ multiplyByPowersOfTen $ (map toInteger) $ reverse $ (take shift) fracPart)
          multiplyByPowersOfTen xs = mul 0 xs -- multiplyByPowersOfTen [1,2,3,4] == [1,20,300,4000]
          mul power []     = []
          mul power (x:xs) = (x * 10^power):(mul (power+1) xs)



instance Fractional BigDecimal where
  fromRational rational = BigDecimal {integerPart = numerator rational, fractionalPart = [0,0..]} / BigDecimal {integerPart = denominator rational, fractionalPart = [0,0..]} 
  recip BigDecimal{integerPart=i,fractionalPart=f} = BigDecimal {integerPart = error "ToDo", fractionalPart = error "ToDo"}
    -- 1/(i+f) = (i-f)/[(i+f)*(i-f)] = (i-f)/[i^2-f^2] -- not helpful :(


instance Floating BigDecimal where
  pi = BigDecimal {integerPart = 3, fractionalPart = tail a000796_list}         
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
fact x = BigDecimal {integerPart = fact' $ toInteger x , fractionalPart = [0,0..]}
  where fact' 0 = 1
        fact' n = product [1..n]

doubleFact :: Int -> BigDecimal
doubleFact x = BigDecimal {integerPart = doubleFact' $ toInteger x , fractionalPart = [0,0..]}
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


-- -- -- -- evalInfiniteSum -- -- --
-- e.g. evalInfiniteSum [1.111111111111...,
--                       0.111111111111...,
--                       0.011111111111...,
--                       0.001111111111...,
--                       0.000111111111...,
--                       0.000011111111...,
--                       ...]
--                    == 1.234567...
evalInfiniteSum :: [BigDecimal] -> BigDecimal
evalInfiniteSum = error "ToDo"



instance RealFrac BigDecimal where
  properFraction BigDecimal{integerPart=i,fractionalPart=f} = (fromInteger i, BigDecimal{integerPart=0, fractionalPart=f}) -- properFraction 3.14 == (3,0.14000000000000012)


-- instance RealFloat BigDecimal where -- impossible because BigDecimal is NOT an actual floating point number





-- Source: https://oeis.org/A000796 (Reinhard Zumkeller 2013)
a000796_list = map fromInteger $ piStream (1, 0, 1)
   [(n, a*d, d) | (n, d, a) <- map (\k -> (k, 2 * k + 1, 2)) [1..]] where
   piStream z xs'@(x:xs)
     | lb /= approx z 4 = piStream (mult z x) xs
     | otherwise = lb : piStream (mult (10, -10 * lb, 1) z) xs'
     where lb = approx z 3
           approx (a, b, c) n = div (a * n + b) c
           mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 
