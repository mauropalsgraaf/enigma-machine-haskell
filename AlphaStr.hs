module AlphaStr (AlphaStr, alphaStr, AlphaChar(unAlphaChar), alphaChar) where

import Data.Char (isLetter, toUpper)

type AlphaStr = [AlphaChar]
newtype AlphaChar = AlphaChar { unAlphaChar :: Char } deriving (Show, Eq)

alphaStr :: String -> Maybe AlphaStr
alphaStr s = mapM alphaChar s
  
alphaChar :: Char -> Maybe AlphaChar
alphaChar c =
  if isLetter c then Just $ AlphaChar $ toUpper c else Nothing

