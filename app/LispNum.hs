module LispNum where

import Data.Bifunctor
import Data.Complex (Complex (..), realPart)
import Data.Ratio (denominator, numerator, (%))

data LispNum
  = LInt Integer
  | LRatio Rational
  | LFloat Double
  | LComplex (Complex Double)

instance Enum LispNum where
  toEnum = LInt . fromIntegral
  fromEnum (LInt n) = fromIntegral n

instance Eq LispNum where
  (==) (LInt n) (LInt m) = n == m
  (==) (LRatio n) (LRatio m) = n == m
  (==) (LFloat n) (LFloat m) = n == m
  (==) (LComplex n) (LComplex m) = n == m
  (==) (LInt n) (LRatio m) = fromInteger n == m
  (==) (LInt n) (LFloat m) = fromInteger n == m
  (==) (LInt n) (LComplex m) = fromInteger n == m
  (==) (LRatio n) (LFloat m) = fromRational n == m
  (==) (LRatio n) (LComplex m) = fromRational n == m
  (==) (LFloat n) (LComplex m) = (n :+ 0) == m
  a == b = b == a

instance Ord LispNum where
  (<=) (LInt n) (LInt m) = n <= m
  (<=) (LRatio n) (LRatio m) = n <= m
  (<=) (LFloat n) (LFloat m) = n <= m
  (<=) (LComplex n) (LComplex m) = (realPart . abs $ n) <= (realPart . abs $ m)
  (<=) (LInt n) (LRatio m) = fromInteger n <= m
  (<=) (LInt n) (LFloat m) = fromInteger n <= m
  (<=) (LInt n) (LComplex m) = fromInteger n <= (realPart . abs $ m)
  (<=) (LRatio n) (LFloat m) = fromRational n <= m
  (<=) (LRatio n) (LComplex m) = fromRational n <= (realPart . abs $ m)
  (<=) (LFloat n) (LComplex m) = n <= (realPart . abs $ m)
  a <= b = b <= a

instance Real LispNum where
  toRational (LInt n) = toRational n
  toRational (LRatio n) = n
  toRational (LFloat n) = toRational n

instance Integral LispNum where
  quotRem (LInt n) (LInt m) = bimap LInt LInt $ quotRem n m
  toInteger (LInt n) = n

instance Show LispNum where
  show (LInt n) = show n
  show (LFloat f) = show f
  show (LRatio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
  show (LComplex (a :+ b)) = show a ++ "+" ++ show b ++ "i"

instance Fractional LispNum where
  fromRational = LRatio
  recip (LInt n) = LRatio . recip . fromInteger $ n
  recip (LRatio n) = LRatio . recip $ n
  recip (LFloat n) = LFloat . recip $ n
  recip (LComplex n) = LComplex . recip $ n

instance Num LispNum where
  (+) (LInt n) (LInt m) = LInt (n + m)
  (+) (LRatio n) (LRatio m) = LRatio (n + m)
  (+) (LFloat n) (LFloat m) = LFloat (n + m)
  (+) (LComplex n) (LComplex m) = LComplex (n + m)
  (+) (LInt n) (LRatio m) = LRatio (n % 1 + m)
  (+) (LInt n) (LFloat m) = LFloat (fromIntegral n + m)
  (+) (LInt n) (LComplex m) = LComplex (fromIntegral n + m)
  (+) (LRatio n) (LFloat m) = LFloat (fromRational n + m)
  (+) (LRatio n) (LComplex m) = LComplex (fromRational n + m)
  (+) (LFloat n) (LComplex m) = LComplex ((n :+ 0) + m)
  a + b = b + a

  (*) (LInt n) (LInt m) = LInt (n * m)
  (*) (LRatio n) (LRatio m) = LRatio (n * m)
  (*) (LFloat n) (LFloat m) = LFloat (n * m)
  (*) (LComplex n) (LComplex m) = LComplex (n * m)
  (*) (LInt n) (LRatio m) = LRatio (n % 1 * m)
  (*) (LInt n) (LFloat m) = LFloat (fromIntegral n * m)
  (*) (LInt n) (LComplex m) = LComplex (fromIntegral n * m)
  (*) (LRatio n) (LFloat m) = LFloat (fromRational n * m)
  (*) (LRatio n) (LComplex m) = LComplex (fromRational n * m)
  (*) (LFloat n) (LComplex m) = LComplex ((n :+ 0) * m)
  a * b = b * a

  abs (LInt n) = LInt (abs n)
  abs (LRatio n) = LRatio (abs n)
  abs (LFloat n) = LFloat (abs n)
  abs (LComplex n) = LComplex (abs n)

  signum (LInt n) = LInt (signum n)
  signum (LRatio n) = LRatio (signum n)
  signum (LFloat n) = LFloat (signum n)
  signum (LComplex n) = LComplex (signum n)

  negate (LInt n) = LInt (negate n)
  negate (LRatio n) = LRatio (negate n)
  negate (LFloat n) = LFloat (negate n)
  negate (LComplex n) = LComplex (negate n)

  fromInteger = LInt
