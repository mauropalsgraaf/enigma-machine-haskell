module Enigma where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import AlphaStr (AlphaStr, alphaStr, AlphaChar, alphaChar, unAlphaChar)
import Data.Functor.Identity
import Reflector (Reflector, reflector, unReflector)
import Data.List (find)

data Machine = Machine { left :: Rotor, middle :: Rotor, right :: Rotor } deriving (Eq, Show)

newtype Rotor = Rotor { mapping :: [(Char, Char)] } deriving (Eq, Show)

machine = Machine { left = leftRotor
                  , middle = middleRotor
                  , right = rightRotor
                  }

-- next level: type level correct rotor
leftRotor = Rotor { mapping = mapping }
    where mapping = [ ('A', 'E'),
            ('B', 'K'),
            ('C', 'M'),
            ('D', 'F'),
            ('E', 'L'),
            ('F', 'G'),
            ('G', 'D'),
            ('H', 'Q'),
            ('I', 'V'),
            ('J', 'Z'),
            ('K', 'N'),
            ('L', 'T'),
            ('M', 'O'),
            ('N', 'W'),
            ('O', 'Y'),
            ('P', 'H'),
            ('Q', 'X'),
            ('R', 'U'),
            ('S', 'S'),
            ('T', 'P'),
            ('U', 'A'),
            ('V', 'I'),
            ('W', 'B'),
            ('X', 'R'),
            ('Y', 'C'),
            ('Z', 'J')
            ]

middleRotor = Rotor { mapping = mapping }
    where mapping = [ ('A', 'A'),
            ('B', 'J'),
            ('C', 'D'),
            ('D', 'K'),
            ('E', 'S'),
            ('F', 'I'),
            ('G', 'R'),
            ('H', 'U'),
            ('I', 'X'),
            ('J', 'B'),
            ('K', 'L'),
            ('L', 'H'),
            ('M', 'W'),
            ('N', 'T'),
            ('O', 'M'),
            ('P', 'C'),
            ('Q', 'Q'),
            ('R', 'G'),
            ('S', 'Z'),
            ('T', 'N'),
            ('U', 'P'),
            ('V', 'Y'),
            ('W', 'F'),
            ('X', 'V'),
            ('Y', 'O'),
            ('Z', 'E')
            ]

rightRotor = Rotor { mapping = mapping }
    where mapping = [ ('A', 'B'),
            ('B', 'D'),
            ('C', 'F'),
            ('D', 'H'),
            ('E', 'J'),
            ('F', 'L'),
            ('G', 'C'),
            ('H', 'P'),
            ('I', 'R'),
            ('J', 'T'),
            ('K', 'X'),
            ('L', 'V'),
            ('M', 'Z'),
            ('N', 'N'),
            ('O', 'Y'),
            ('P', 'E'),
            ('Q', 'I'),
            ('R', 'W'),
            ('S', 'G'),
            ('T', 'A'),
            ('U', 'K'),
            ('V', 'M'),
            ('W', 'U'),
            ('X', 'S'),
            ('Y', 'Q'),
            ('Z', 'O')
            ]

encode :: Machine -> AlphaStr -> AlphaStr
encode machine s = fst $ runState (encodeString s) machine

decode :: Machine -> AlphaStr -> AlphaStr
decode = encode

encodeString :: AlphaStr -> State Machine AlphaStr
encodeString = mapM encodeChar
  
encodeChar :: AlphaChar -> State Machine AlphaChar
encodeChar c = state stateFunction
  -- state stateFunction
  where stateFunction :: Machine -> (AlphaChar,  Machine)
        stateFunction m@(Machine { left = left, middle = middle, right = right }) =
          ((encodeCharOneStepWithRotor left . encodeCharOneStepWithRotor middle . encodeCharOneStepWithRotor right) c, m)         

encodeCharOneStepWithRotor :: Rotor -> AlphaChar -> AlphaChar
encodeCharOneStepWithRotor rotor c =
  unsafeGet $ snd <$> charMapping >>= alphaChar
  where listOfCharMap = mapping rotor
        charMapping = find ((==) (unAlphaChar c) . fst) listOfCharMap
  
unsafeGet :: Maybe a -> a
unsafeGet (Just a) = a
