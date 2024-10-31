module MyLib where

newtype Matrix = Matrix [[Int]] 

extractM :: Matrix -> [[Int]]
extractM (Matrix m) = m

instance Show Matrix where
  show (Matrix m) = fmtMatrix m

fmtMatrix :: [[Int]] -> String
fmtMatrix m = 
  column m 
  where column :: [[Int]] -> String
        column [] = "" 
        column (x:xs) = 
          "[ " ++ line x ++ "\n" ++ column xs 
        line :: [Int] -> String 
        line [] = "]"
        line (x:xs) = (show x) ++ " " ++ line xs

type Line = Int
type Column = Int

sizeM :: Matrix -> Maybe (Line, Column)
sizeM (Matrix m@(x:_)) = Just $ (length m , length x) 
sizeM _ = Nothing

mIsSquare :: Matrix -> Bool
mIsSquare m =
  let s = sizeM m
  in fmap fst s == fmap snd s

mSizeEq :: Matrix -> Matrix -> Bool
mSizeEq m m' =
  let s = sizeM m
      s' = sizeM m'
  in and [
    (fmap fst s) == (fmap fst s'),
    (fmap snd s) == (fmap snd s')
  ]

multipliable :: Matrix -> Matrix -> Bool
multipliable m m' = 
  let s = sizeM m
      s' = sizeM m'
  in fmap snd s == fmap fst s'

addM :: Matrix -> Matrix -> Matrix
addM matrix matrix' =
  let lineM = addM' matrix matrix' 
      s = sizeM matrix
  in Matrix $ listToM (extractMaybe s (0,0)) lineM
  where addM' :: Matrix -> Matrix -> [Int]
        addM' m m' =
          let elems = concat $ extractM m
              elems' = concat $ extractM m'
          in zipWith (+) elems elems'
        listToM :: (Line, Column) -> [Int] -> [[Int]]
        listToM _ [] = []
        listToM s@(_, column) list = 
          let taken = take column list
              dropped = drop column list
          in taken : listToM s dropped 

extractMaybe :: Maybe a -> a -> a
extractMaybe (Just a) _ = a
extractMaybe Nothing fallback = fallback 

