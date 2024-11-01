# Basic linear algebra in Haskell
A very basic linear algebra library in Haskell 
(Only works for Int for now)

# Features
- Matrix creation
- Matrix addition
- Matrix multiplication
# Usage
## create a Matrix
matrices are just a list of lists of Ints
> let matrix = Matrix [[1,2],[3,4]]
## multiply matrices 
1. verify if 2 matrices are multipliable 
> multipliable m1 m2
2. call mMultiply function
> mMultiply m m'
## add matrices 
1. verify if 2 matrices are addable
> mSizeMult m m'
2. call mMultiply
> mMultiply m m'

