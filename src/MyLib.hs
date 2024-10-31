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

addM :: Matrix -> Matrix -> [Int]
addM m m' =
  let lineM = addM' m m' 
      s = sizeM m
  in listToM s lineM  
  where addM' :: Matrix -> Matrix -> [Int]
        addM' m m' =
          let elems = concat $ extractM m
              elems' = concat $ extractM m'
          in zipWith (+) elems elems'
        listToM :: (Line, Column) -> [Int] -> Matrix
        listToM _ [] = []
        listToM (_, column) list = 
          let taken = take column
              dropped = drop column
          in taken : listToM dropped 

  

