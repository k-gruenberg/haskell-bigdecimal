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
  signum Plus   = 1
  signum Minus  = -1
  fromInteger i = toEnum $ fromInteger i








data BigDecimal = BigDecimal {sign :: Sign, integerPart :: Integer, fractionalPart :: [Int]}
-- * Using a scale to specifiy where the decimal point is (like it's done in Java's BigDecimal) wouldn't allow laziness,
--   therefore we're storing the digits after the decimal point as an infinite(always!) lazy list of digits.
-- * Because 0 == -0 we have to store the sign of a BigDecimal seperately from its integerPart - otherwise we couldn't tell apart +0.1234... and -0.1234... for example!
--   As a result, we will require that the integerPart shall always be a positive Integer!


show_N_decimal_places :: Int -> BigDecimal -> String
show_N_decimal_places n BigDecimal{sign=s, integerPart=i, fractionalPart=f} 
  | n <= 0    = (show s) ++ (show i)
  | otherwise = (show s) ++ (show i) ++ "." ++ (concat $ (map show) $ (take n) f) ++ "..."


instance Show BigDecimal where
  show = show_N_decimal_places 50 


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
  toEnum int                                                  = BigDecimal {sign = toEnum $ signum int, integerPart = abs $ toInteger int, fractionalPart = [0,0..]}
  fromEnum BigDecimal{sign=s, integerPart=i,fractionalPart=f} = (fromEnum s) * (fromInteger i) -- fromEnum 3.14 == 3 ; fromEnum (-3.14) == -3


-- !!! Because the decimal expansion of ALL(!) BigDecimal is infinite, we have to use left-to-right addition !!!
instance Num BigDecimal where
  -- ===== ===== BigDecimal Addition: ===== =====
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Minus,integerPart=i2,fractionalPart=f2} = -- (-x)+(-y) == -(x+y)
      negate BigDecimal{sign=Plus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} 
  BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Plus ,integerPart=i2,fractionalPart=f2} = -- (-x)+y == y+(-x)
      BigDecimal{sign=Plus,integerPart=i2,fractionalPart=f2} + BigDecimal{sign=Minus,integerPart=i1,fractionalPart=f1}
  BigDecimal{sign=Plus ,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Plus ,integerPart=i2,fractionalPart=f2} = error "ToDo: x+y"
  BigDecimal{sign=Plus ,integerPart=i1,fractionalPart=f1} + BigDecimal{sign=Minus,integerPart=i2,fractionalPart=f2} = error "ToDo: x-y"


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
    + mulIntFraction i1 f2 -- i1*f2
    + mulIntFraction i2 f1 -- f1*i2
    + BigDecimal{sign=Plus, integerPart=0, fractionalPart=mulFractionFraction f1 f1} -- f1*f2
    where mulIntFraction :: Integer -> [Int] -> BigDecimal
          mulIntFraction = error "ToDo"
          
          mulFractionFraction :: [Int] -> [Int] -> [Int]
          mulFractionFraction = error "ToDo"


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
    / BigDecimal {sign = Plus, integerPart = denominator rational, fractionalPart = [0,0..]} -- as you can see below the denominator of a Rationals is always +
  -- numerator   (3 % (-4)) == (-3)
  -- denominator (3 % (-4)) == 4
  -- numerator   ((-3) % (-4)) == 3
  -- denominator ((-3) % (-4)) == 4

  recip BigDecimal{sign=s,integerPart=i,fractionalPart=f} = BigDecimal {sign = s, integerPart = error "ToDo", fractionalPart = error "ToDo"}
    -- 1/(i+f) = (i-f)/[(i+f)*(i-f)] = (i-f)/[i^2-f^2] -- not helpful :(


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



instance RealFrac BigDecimal where -- properFraction :: (RealFrac a, Integral b) => a -> (b, a)
  properFraction BigDecimal{sign=s,integerPart=i,fractionalPart=f}
    = ( fromInteger ( (toInteger $ fromEnum s) * i ) , BigDecimal{sign=s, integerPart=0, fractionalPart=f} )
-- properFraction   3.14  == (3,0.14000000000000012)
-- properFraction (-3.14) == (-3,-0.14000000000000012)


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
