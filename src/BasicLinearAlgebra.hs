{-| 
Module      : BasicLinearAlgebra
Description : Provides very basic matrix operations.
-}
module BasicLinearAlgebra (
  Matrix(..), 
  extractM,
  extractMaybe,
  mSize,
  mSizeMult,
  mIsSquare,
  mSizeEq,
  mMultipliable,
  mMultiply,
  mAdd, 
  mScale 
) where

import Data.Ratio
import GHC.OldList (transpose)

type Line = Int
type Column = Int
type Scale = Rational 

-- | Type of matrix contains a list of lists of Int
newtype Matrix = Matrix [[Rational]] 

instance Show Matrix where
  show (Matrix m) = fmtMatrix m

hasDecimal :: Rational -> Bool
hasDecimal x = denominator x /= 1

showRatio :: Rational -> String 
showRatio = \rat -> 
  if not $ hasDecimal rat 
  then show (fromInteger $ numerator rat :: Int)
  else show (fromRational rat :: Double)
  

-- | Used for the implementation of Show typeclass for the type Matrix
fmtMatrix :: [[Rational]] -> String
fmtMatrix m = 
  col m 
  where col :: [[Rational]] -> String
        col [] = "" 
        col (x:xs) = 
          "[ " ++ line x ++ "\n" ++ col xs 
        line :: [Rational] -> String 
        line [] = "]"
        line (x:xs) = (show (showRatio x)) ++ " " ++ line xs

-- | Used to extract the list of lists of Int from Matrix type 
extractM :: Matrix -> [[Rational]]
extractM (Matrix m) = m

-- | Extract the data from Maybe Monad
-- | if value empty it returns the fallback value
-- | the second argument is a fallback value
extractMaybe :: Maybe a -> a -> a
extractMaybe (Just a) _ = a
extractMaybe Nothing fallback = fallback 

-- | Get the size of the matrix in a tuple (Int, Int) 
mSize :: Matrix -> Maybe (Line, Column)
mSize (Matrix m@(x:_)) = Just $ (length m , length x) 
mSize _ = Nothing

-- | Returns the resulting size of the matrix after multiplication of m and m'.
mSizeMult :: Matrix -> Matrix -> Maybe (Line, Column)
mSizeMult m m' = 
  if mMultipliable m m' 
  then Just $ mSizeMult' m m'  
  else Nothing
  where
    mSizeMult' :: Matrix -> Matrix -> (Line, Column)
    mSizeMult' matrix matrix' = 
      let s = mSize matrix
          s' = mSize matrix'
      in (fst $ extractMaybe s (0,0) , snd $ extractMaybe s' (0,0))

-- | Verify if matrix is square matrix
mIsSquare :: Matrix -> Bool
mIsSquare m =
  let s = mSize m
  in fmap fst s == fmap snd s


-- | Verify if both matrix are of equal size
mSizeEq :: Matrix -> Matrix -> Bool
mSizeEq m m' =
  let s = mSize m
      s' = mSize m'
  in and [
    (fmap fst s) == (fmap fst s'),
    (fmap snd s) == (fmap snd s')
  ]

-- | Verify if both matrices can be multiplied
mMultipliable :: Matrix -> Matrix -> Bool
mMultipliable m m' = 
  let s = mSize m
      s' = mSize m'
  in fmap snd s == fmap fst s'

-- | Multiply both matrices (must first be verified with mMultipliable)
mMultiply :: Matrix -> Matrix -> Matrix
mMultiply matrix@(Matrix m) m' =
  let col = extractM $ column m'
      s = extractMaybe (mSizeMult matrix m') (0,0)
  in mLines (calculate m col) s
  where
    calculate :: [[Rational]] -> [[Rational]] -> [Rational] 
    calculate l1 l2 = [calculate' x z [] | x <- l1, z <- l2] 

    calculate' :: [Rational] -> [Rational] -> [Rational] -> Rational 
    calculate' [] [] acc = foldl' (+) (0 :: Rational) acc
    calculate' [] (_:_) _ = 0
    calculate' (_:_) [] _ = 0
    calculate' (x:xs) (z:zs) acc = calculate' xs zs ((x * z) : acc)

{- | 
Takes a list of Int and returns a Matrix 
by making the lines of the matrix 
with the size resulting from mSizeMult and the take function 
-}
mLines :: [Rational] -> (Line, Column) -> Matrix
mLines list (_, c) = Matrix $ takeN c list         
  where
    takeN :: Int -> [Rational] -> [[Rational]]
    takeN _ [] = []
    takeN col list' = take col list' : takeN col (drop col list')
      
-- | Returns the columns of a Matrix in the form of another matrix
column :: Matrix -> Matrix
column (Matrix matrix) =
  Matrix $ transpose matrix

-- | Add matrices together (must first be verified with mSizeEq)
mAdd :: Matrix -> Matrix -> Matrix
mAdd matrix matrix' =
  let lineM = addM' matrix matrix' 
      s = mSize matrix
  in Matrix $ listToM (extractMaybe s (0,0)) lineM
  where addM' :: Matrix -> Matrix -> [Rational]
        addM' m m' =
          let elems = concat $ extractM m
              elems' = concat $ extractM m'
          in zipWith (+) elems elems'
        listToM :: (Line, Column) -> [Rational] -> [[Rational]]
        listToM _ [] = []
        listToM s@(_, col) list = 
          let taken = take col list
              dropped = drop col list
          in taken : listToM s dropped 

-- | Apply a scale to a matrix 
mScale :: Matrix -> Scale -> Matrix
mScale (Matrix m) s = 
  Matrix $ mScale' m s
  where
    mScale' :: [[Rational]] -> Rational -> [[Rational]]
    mScale' [] _ = []
    mScale' (x:xs) scale = fmap (* scale) x : mScale' xs scale
    
