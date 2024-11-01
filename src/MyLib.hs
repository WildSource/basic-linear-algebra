module MyLib where
import GHC.OldList (transpose)
import GHC.Exception (underflowException)

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
      in (fst $ extractMaybe s, snd $ extractMaybe s')


mIsSquare :: Matrix -> Bool
mIsSquare m =
  let s = mSize m
  in fmap fst s == fmap snd s

mSizeEq :: Matrix -> Matrix -> Bool
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

mMultiply :: Matrix -> Matrix -> [Int]
mMultiply (Matrix m) m' =
  let col = extractM $ column m'
  in calculate m col
  where
    calculate :: [[Int]] -> [[Int]] -> [Int] 
    calculate l1 l2 = [calculate' x z [] | x <- l1, z <- l2] 

    calculate' :: [Int] -> [Int] -> [Int] -> Int 
    calculate' [] [] acc = foldl' (+) 0 acc
    calculate' [] (_:_) _ = 0
    calculate' (_:_) [] _ = 0
    calculate' (x:xs) (z:zs) acc = calculate' xs zs ((x * z) : acc)
         
      
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
    
