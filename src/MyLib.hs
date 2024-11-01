module MyLib where
import GHC.OldList (transpose)

newtype Matrix = Matrix [[Int]] 

extractM :: Matrix -> [[Int]]
extractM (Matrix m) = m

extractMaybe :: Maybe a -> a -> a
extractMaybe (Just a) _ = a
extractMaybe Nothing fallback = fallback 

instance Show Matrix where
  show (Matrix m) = fmtMatrix m

fmtMatrix :: [[Int]] -> String
fmtMatrix m = 
  col m 
  where col :: [[Int]] -> String
        col [] = "" 
        col (x:xs) = 
          "[ " ++ line x ++ "\n" ++ col xs 
        line :: [Int] -> String 
        line [] = "]"
        line (x:xs) = (show x) ++ " " ++ line xs

type Line = Int
type Column = Int

mSize :: Matrix -> Maybe (Line, Column)
mSize (Matrix m@(x:_)) = Just $ (length m , length x) 
mSize _ = Nothing

mIsSquare :: Matrix -> Bool
mIsSquare m =
  let s = mSize m
  in fmap fst s == fmap snd s

columnmSizeEq :: Matrix -> Matrix -> Bool
mSizeEq m m' =
  let s = mSize m
      s' = mSize m'
  in and [
    (fmap fst s) == (fmap fst s'),
    (fmap snd s) == (fmap snd s')
  ]

mMultipliable :: Matrix -> Matrix -> Bool
mMultipliable m m' = 
  let s = mSize m
      s' = mSize m'
  in fmap snd s == fmap fst s'

mMultiply :: Matrix -> Matrix -> Matrix
mMultiply m m' =
  let col = column m'
  in undefined

-- TODO array of columns instead of only first column
column :: Matrix -> Matrix
column (Matrix matrix) =
  Matrix $ transpose matrix

mAdd :: Matrix -> Matrix -> Matrix
mAdd matrix matrix' =
  let lineM = addM' matrix matrix' 
      s = mSize matrix
  in Matrix $ listToM (extractMaybe s (0,0)) lineM
  where addM' :: Matrix -> Matrix -> [Int]
        addM' m m' =
          let elems = concat $ extractM m
              elems' = concat $ extractM m'
          in zipWith (+) elems elems'
        listToM :: (Line, Column) -> [Int] -> [[Int]]
        listToM _ [] = []
        listToM s@(_, col) list = 
          let taken = take col list
              dropped = drop col list
          in taken : listToM s dropped 

mScale :: Matrix -> Int -> Matrix
mScale (Matrix m) s = 
  Matrix $ mScale' m s
  where
    mScale' :: [[Int]] -> Int -> [[Int]]
    mScale' [] _ = []
    mScale' (x:xs) scale = fmap (* scale) x : mScale' xs scale
    
