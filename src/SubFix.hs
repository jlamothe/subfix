{-|
Module      : SubFix
Description : core functions for subfix
Copyright   : (C) Jonathan Lamothe
License     : GPL-3
Maintainer  : jonathan@jlamothe.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

module SubFix (
  -- * Data Types
  Caption (..),
  -- * Functions
  convert,
  decode,
  encode,
) where

import Data.Char (chr)

-- | Defines a caption group
data Caption = Caption
  { capID :: Int
  -- ^ The caption identifier
  , capStart :: Integer
  -- ^ The caption start time (in milliseconds)
  , capEnd :: Integer
  -- ^ The caption end time (in milliseconds)
  , capText :: String
  -- ^ The caption text
  } deriving (Eq, Show)

-- | Applies the transformations to a caption group
convert :: Caption -> Caption
convert c = c { capText = editText $ capText c }

-- | Decodes the text of a subtitle file
decode
  :: String
  -- ^ The encoded text
  -> Either String [Caption]
  -- ^ The resulting caption list, or a message describing the error
  -- that occured.
decode = undefined

-- | Encodes a list of caption groups
encode :: [Caption] -> String
encode = undefined

editText :: String -> String
editText = checkCaret . checkNotes

checkCaret :: String -> String
checkCaret ('^' : str) = "{\\an8}" ++ str
checkCaret str = str

checkNotes :: String -> String
checkNotes "" = ""
checkNotes ('#' : '#' : str) = chr 0x2669 : checkNotes str
checkNotes (ch : str) = ch : checkNotes str

--jl
